#include "parse/parser.h"

#include <utility>

#include "absl/strings/str_format.h"

Parser::Parser(Lexer* lexer) : lexer_(lexer) {}

bcf::Result<std::vector<std::unique_ptr<GlobalDecl>>> Parser::Parse() {
  std::vector<std::unique_ptr<GlobalDecl>> result;
  while (true) {
    absl::optional<Token> token = TRY(lexer_->PeekToken());
    if (!token) {
      break;
    }

    switch (token->type()) {
      case TokenType::kInclude:
        result.emplace_back(TRY(ParseInclude()));
        break;
      case TokenType::kFn: {
        std::unique_ptr<FuncDecl> func = TRY(ParseFuncDecl());
        result.push_back(
            std::make_unique<ExprGlobalDecl>(*token, std::move(func)));
        break;
      }
      case TokenType::kStatic:
      case TokenType::kLet:
      case TokenType::kMut: {
        std::unique_ptr<Decl> decl = TRY(ParseDecl());
        result.push_back(
            std::make_unique<ExprGlobalDecl>(*token, std::move(decl)));
        break;
      }
      default:
        return MakeError(absl::StrCat("Unexpected token: ", token->text()),
                         token->location());
    }
  }

  return result;
}

bcf::Result<Token> Parser::HandleEof(absl::optional<Token> token) {
  if (!token) {
    if (!last_token_) {
      return bcf::Err("Unexpected EOF");
    }
    return MakeError("Unexpected EOF", last_token_->location());
  }

  last_token_.emplace(*token);
  return *token;
}

bcf::Result<Token> Parser::PeekToken() {
  return HandleEof(TRY(lexer_->PeekToken()));
}

bcf::Result<Token> Parser::PopToken() {
  return HandleEof(TRY(lexer_->PopToken()));
}

bcf::Result<Token> Parser::PopTokenType(TokenType type) {
  Token token = TRY(PopToken());

  if (token.type() != type) {
    auto msg = absl::StrFormat("Unexpected %s. Expected %s",
                               TokenTypeToString(token.type()),
                               TokenTypeToString(type));
    return MakeError(msg, token.location());
  }

  return token;
}

bcf::Result<std::unique_ptr<IncludeGlobalDecl>> Parser::ParseInclude() {
  Token start_token = TRY(PopTokenType(TokenType::kInclude));
  Token token = TRY(PopToken());

  if (token.type() == TokenType::kString) {
    return std::make_unique<IncludeGlobalDecl>(
        start_token, IncludeGlobalDeclType::kQuote, token.text());
  }

  if (token.type() == TokenType::kLt) {
    Token path = TRY(PopToken());
    TRY(PopTokenType(TokenType::kGt));
    return std::make_unique<IncludeGlobalDecl>(
        start_token, IncludeGlobalDeclType::kBracket, path.text());
  }

  return MakeError("Unexpected token after include", token.location());
}

bcf::Result<std::unique_ptr<FuncDecl>> Parser::ParseFuncDecl() {
  Token start_token = TRY(PopTokenType(TokenType::kFn));
  Token name = TRY(PopTokenType(TokenType::kId));
  TRY(PopTokenType(TokenType::kLParen));

  std::vector<std::unique_ptr<Decl>> args;
  while (true) {
    Token token = TRY(PeekToken());
    if (token.type() == TokenType::kRParen) {
      break;
    }

    args.push_back(TRY(ParseDeclVar()));
    TRY(PopTokenType(TokenType::kComma));
  }

  TRY(PopTokenType(TokenType::kArrow));

  std::unique_ptr<Type> ret_type = TRY(ParseType());

  Token next = TRY(PopToken());
  std::unique_ptr<Expr> body;

  if (next.type() == TokenType::kSemi) {
    // Just a declaration.
  } else if (next.type() == TokenType::kLBrace) {
    body = TRY(ParseCompoundExpr());
  }

  return std::make_unique<FuncDecl>(start_token, name.text(), std::move(args),
                                    std::move(ret_type), std::move(body));
}

bcf::Result<std::unique_ptr<Decl>> Parser::ParseDecl() {
  Token token = TRY(PopToken());
  DeclFlags flags = kDeclFlagsNone;
  if (token.type() == TokenType::kLet) {
  } else if (token.type() == TokenType::kMut) {
    flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);
  } else if (token.type() == TokenType::kStatic) {
    flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);

    Token next = TRY(PeekToken());
    if (next.type() == TokenType::kMut) {
      flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);
      TRY(PopToken());
    }
  }

  return ParseDeclVar(flags);
}

bcf::Result<std::unique_ptr<Decl>> Parser::ParseDeclVar(DeclFlags flags) {
  Token name = TRY(PopTokenType(TokenType::kId));
  TRY(PopTokenType(TokenType::kColon));
  std::unique_ptr<Type> type = TRY(ParseType());

  Token token = TRY(PeekToken());

  std::unique_ptr<Expr> expr;
  if (token.type() == TokenType::kEq) {
    TRY(PopToken());
    expr = TRY(ParseSingleExpr());
  }

  return std::make_unique<Decl>(name, flags, name.text(), std::move(type),
                                std::move(expr));
}

bcf::Result<std::unique_ptr<Type>> Parser::ParseType() {
  Token name = TRY(PopTokenType(TokenType::kId));

  Token next = TRY(PeekToken());
  if (next.type() != TokenType::kLt) {
    return {std::make_unique<BaseType>(name, name.text())};
  }

  TRY(PopToken());

  std::vector<std::unique_ptr<Type>> types;
  while (true) {
    Token next = TRY(PeekToken());
    if (next.type() == TokenType::kGt) {
      TRY(PopToken());
      break;
    }

    types.push_back(TRY(ParseType()));
  }

  return {std::make_unique<TemplateType>(name, name.text(), std::move(types))};
}

bcf::Result<std::unique_ptr<Expr>> Parser::ParseCompoundExpr() {
  Token start_token = TRY(PeekToken());

  std::vector<std::unique_ptr<Expr>> exprs;
  while (true) {
    Token token = TRY(PeekToken());
    if (token.type() == TokenType::kRBrace) {
      break;
    }
    exprs.push_back(TRY(ParseSingleExpr()));
    TRY(PopTokenType(TokenType::kSemi));
  }

  return {std::make_unique<CompoundExpr>(start_token, std::move(exprs))};
}

bcf::Result<std::unique_ptr<Expr>> Parser::ParseSingleExpr() {
  Token token = TRY(PeekToken());

  switch (token.type()) {
    case TokenType::kStatic:
    case TokenType::kLet:
    case TokenType::kMut: {
      return {TRY(ParseDecl())};
    }
    case TokenType::kId:
      return {std::make_unique<VariableExpr>(token, token.text())};
    case TokenType::kIntLit:
      return {std::make_unique<IntExpr>(token)};
    case TokenType::kFloatLit:
      return {std::make_unique<FloatExpr>(token)};
    case TokenType::kString:
      return {std::make_unique<StringExpr>(token)};
    default:
      return MakeError(absl::StrCat("Unexpected token: ", token.text()),
                       token.location());
  }
}
