#include "parse/parser.h"

#include <utility>

#include "absl/strings/str_format.h"

Parser::Parser(Lexer* lexer) : lexer_(lexer) {}

absl::StatusOr<std::vector<std::unique_ptr<GlobalDecl>>> Parser::Parse() {
  std::vector<std::unique_ptr<GlobalDecl>> result;
  while (true) {
    Token token = BTRY(lexer_->PeekToken());
    if (token.is_eof()) {
      break;
    }

    switch (token.type) {
      case TokenType::kInclude:
        result.emplace_back(BTRY(ParseInclude()));
        break;
      case TokenType::kFn: {
        std::unique_ptr<FuncDecl> func = BTRY(ParseFuncDecl());
        result.push_back(
            std::make_unique<ExprGlobalDecl>(token, std::move(func)));
        break;
      }
      case TokenType::kStatic:
      case TokenType::kLet:
      case TokenType::kMut: {
        std::unique_ptr<Decl> decl = BTRY(ParseDecl());
        result.push_back(
            std::make_unique<ExprGlobalDecl>(token, std::move(decl)));
        break;
      }
      default:
        return MakeError(absl::StrCat("Unexpected token: ", token.text),
                         token.location);
    }
  }

  return result;
}

absl::StatusOr<Token> Parser::HandleEof(Token token) {
  if (token.is_eof()) {
    if (!last_token_) {
      return absl::InvalidArgumentError("Unexpected EOF");
    }
    return MakeError("Unexpected EOF", last_token_->location);
  }

  last_token_.emplace(token);
  return token;
}

absl::StatusOr<Token> Parser::PeekToken() {
  return HandleEof(BTRY(lexer_->PeekToken()));
}

absl::StatusOr<Token> Parser::PopToken() {
  return HandleEof(BTRY(lexer_->PopToken()));
}

absl::StatusOr<Token> Parser::PopTokenType(TokenType type) {
  Token token = BTRY(PopToken());

  if (token.type != type) {
    auto msg =
        absl::StrFormat("Unexpected %s. Expected %s",
                        TokenTypeToString(token.type), TokenTypeToString(type));
    return MakeError(msg, token.location);
  }

  return token;
}

absl::StatusOr<std::unique_ptr<IncludeGlobalDecl>> Parser::ParseInclude() {
  Token start_token = BTRY(PopTokenType(TokenType::kInclude));
  Token token = BTRY(PopToken());

  if (token.type == TokenType::kString) {
    return std::make_unique<IncludeGlobalDecl>(
        start_token, IncludeGlobalDeclType::kQuote, token.text);
  }

  if (token.type == TokenType::kLt) {
    Token path = BTRY(PopToken());
    BTRY(PopTokenType(TokenType::kGt));
    return std::make_unique<IncludeGlobalDecl>(
        start_token, IncludeGlobalDeclType::kBracket, path.text);
  }

  return MakeError("Unexpected token after include", token.location);
}

absl::StatusOr<std::unique_ptr<FuncDecl>> Parser::ParseFuncDecl() {
  Token start_token = BTRY(PopTokenType(TokenType::kFn));
  Token name = BTRY(PopTokenType(TokenType::kId));
  BTRY(PopTokenType(TokenType::kLParen));

  std::vector<std::unique_ptr<Decl>> args;
  while (true) {
    Token token = BTRY(PeekToken());
    if (token.type == TokenType::kRParen) {
      break;
    }

    args.push_back(BTRY(ParseDeclVar()));
    token = BTRY(PeekToken());
    if (token.type == TokenType::kRParen) {
      break;
    }
    BTRY(PopTokenType(TokenType::kComma));
  }
  BTRY(PopTokenType(TokenType::kRParen));
  BTRY(PopTokenType(TokenType::kArrow));

  std::unique_ptr<Type> ret_type = BTRY(ParseType());

  Token next = BTRY(PopToken());
  std::unique_ptr<Expr> body;

  if (next.type == TokenType::kSemi) {
    // Just a declaration.
  } else if (next.type == TokenType::kLBrace) {
    body = BTRY(ParseCompoundExpr());
  }

  return std::make_unique<FuncDecl>(start_token, name.text, std::move(args),
                                    std::move(ret_type), std::move(body));
}

absl::StatusOr<std::unique_ptr<Decl>> Parser::ParseDecl() {
  Token token = BTRY(PopToken());
  DeclFlags flags = kDeclFlagsNone;
  if (token.type == TokenType::kLet) {
  } else if (token.type == TokenType::kMut) {
    flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);
  } else if (token.type == TokenType::kStatic) {
    flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);

    Token next = BTRY(PeekToken());
    if (next.type == TokenType::kMut) {
      flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);
      BTRY(PopToken());
    }
  }

  return ParseDeclVar(flags);
}

absl::StatusOr<std::unique_ptr<Decl>> Parser::ParseDeclVar(DeclFlags flags) {
  Token name = BTRY(PopTokenType(TokenType::kId));
  BTRY(PopTokenType(TokenType::kColon));
  std::unique_ptr<Type> type = BTRY(ParseType());

  Token token = BTRY(PeekToken());

  std::unique_ptr<Expr> expr;
  if (token.type == TokenType::kEq) {
    BTRY(PopToken());
    expr = BTRY(ParseSingleExpr());
  }

  return std::make_unique<Decl>(name, flags, name.text, std::move(type),
                                std::move(expr));
}

absl::StatusOr<std::unique_ptr<Type>> Parser::ParseType() {
  Token first = BTRY(PeekToken());
  if (first.type == TokenType::kStar) {
    BTRY(PopToken());
    return std::make_unique<PointerType>(first, BTRY(ParseType()));
  }

  Token name = BTRY(PopTokenType(TokenType::kId));

  Token next = BTRY(PeekToken());
  if (next.type != TokenType::kLt) {
    return std::make_unique<BaseType>(name, name.text);
  }

  BTRY(PopToken());

  std::vector<std::unique_ptr<Type>> types;
  while (true) {
    Token next = BTRY(PeekToken());
    if (next.type == TokenType::kGt) {
      BTRY(PopToken());
      break;
    }

    types.push_back(BTRY(ParseType()));
  }

  return std::make_unique<TemplateType>(name, name.text, std::move(types));
}

absl::StatusOr<std::unique_ptr<Expr>> Parser::ParseCompoundExpr() {
  Token start_token = BTRY(PeekToken());

  std::vector<std::unique_ptr<Expr>> exprs;
  while (true) {
    Token token = BTRY(PeekToken());
    if (token.type == TokenType::kRBrace) {
      break;
    }
    exprs.push_back(BTRY(ParseSingleExpr()));
    BTRY(PopTokenType(TokenType::kSemi));
  }

  return std::make_unique<CompoundExpr>(start_token, std::move(exprs));
}

absl::StatusOr<std::unique_ptr<Expr>> Parser::ParseSingleExpr() {
  Token token = BTRY(PeekToken());

  switch (token.type) {
    case TokenType::kStatic:
    case TokenType::kLet:
    case TokenType::kMut: {
      return BTRY(ParseDecl());
    }
    case TokenType::kId:
      return std::make_unique<VariableExpr>(token, token.text);
    case TokenType::kIntLit:
      return std::make_unique<IntExpr>(token);
    case TokenType::kFloatLit:
      return std::make_unique<FloatExpr>(token);
    case TokenType::kString:
      return std::make_unique<StringExpr>(token);
    default:
      return MakeError(absl::StrCat("Unexpected token: ", token.text),
                       token.location);
  }
}
