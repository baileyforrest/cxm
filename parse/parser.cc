#include "parse/parser.h"

#include <utility>

#include "absl/strings/str_format.h"
#include "util/error.h"

Parser::Parser(Lexer* lexer) : lexer_(lexer) {}

std::vector<std::unique_ptr<GlobalDecl>> Parser::Parse() {
  std::vector<std::unique_ptr<GlobalDecl>> result;
  while (true) {
    Token token = lexer_->PeekToken();
    if (token.is_eof()) {
      break;
    }

    switch (token.type) {
      case TokenType::kInclude:
        result.emplace_back(ParseInclude());
        break;
      case TokenType::kFn: {
        result.push_back(ParseFuncDecl());
        break;
      }
      case TokenType::kStatic:
      case TokenType::kLet:
      case TokenType::kMut: {
        result.push_back(std::make_unique<DeclGlobalDecl>(token, ParseDecl()));
        break;
      }
      default:
        throw Error(absl::StrCat("Unexpected token: ", token.text),
                    token.location);
    }
  }

  return result;
}

Token Parser::HandleEof(Token token) {
  if (token.is_eof()) {
    if (!last_token_) {
      throw Error("Unexpected EOF");
    }
    throw Error("Unexpected EOF", last_token_->location);
  }

  last_token_.emplace(token);
  return token;
}

Token Parser::PeekToken() { return HandleEof(lexer_->PeekToken()); }

Token Parser::PopToken() { return HandleEof(lexer_->PopToken()); }

Token Parser::PopTokenType(TokenType type) {
  Token token = PopToken();

  if (token.type != type) {
    throw Error(
        absl::StrFormat("Unexpected %s. Expected %s",
                        TokenTypeToString(token.type), TokenTypeToString(type)),
        token.location);
  }

  return token;
}

std::unique_ptr<IncludeGlobalDecl> Parser::ParseInclude() {
  Token start_token = PopTokenType(TokenType::kInclude);
  Token token = PopToken();

  if (token.type == TokenType::kString) {
    return std::make_unique<IncludeGlobalDecl>(
        start_token, IncludeGlobalDeclType::kQuote, token.text);
  }

  if (token.type == TokenType::kLt) {
    Token path = PopToken();
    PopTokenType(TokenType::kGt);
    return std::make_unique<IncludeGlobalDecl>(
        start_token, IncludeGlobalDeclType::kBracket, path.text);
  }

  throw Error("Unexpected token after include", token.location);
}

std::unique_ptr<FuncDecl> Parser::ParseFuncDecl() {
  Token start_token = PopTokenType(TokenType::kFn);
  Token name = PopTokenType(TokenType::kId);
  PopTokenType(TokenType::kLParen);

  std::vector<std::unique_ptr<Decl>> args;
  while (true) {
    Token token = PeekToken();
    if (token.type == TokenType::kRParen) {
      break;
    }

    args.push_back(ParseDeclVar());
    token = PeekToken();
    if (token.type == TokenType::kRParen) {
      break;
    }
    PopTokenType(TokenType::kComma);
  }
  PopTokenType(TokenType::kRParen);
  PopTokenType(TokenType::kArrow);

  std::unique_ptr<Type> ret_type = ParseType();

  Token next = PopToken();
  std::unique_ptr<CompoundStmt> body;

  if (next.type == TokenType::kSemi) {
    // Just a declaration.
  } else if (next.type == TokenType::kLBrace) {
    body = ParseCompoundStmt();
  }

  return std::make_unique<FuncDecl>(start_token, name.text, std::move(args),
                                    std::move(ret_type), std::move(body));
}

std::unique_ptr<Stmt> Parser::ParseStmt() {
  Token token = PeekToken();
  switch (token.type) {
    case TokenType::kStatic:
    case TokenType::kLet:
    case TokenType::kMut: {
      return std::make_unique<DeclStmt>(ParseDecl());
    }
    case TokenType::kIf:
      return ParseIf();
    default:
      throw Error(absl::StrCat("Unexpected token: ", token.text),
                  token.location);
  }
}

std::unique_ptr<Stmt> Parser::ParseIf() {
  Token start_token = PopTokenType(TokenType::kIf);
  std::unique_ptr<Expr> test = ParseExpr();

  PopTokenType(TokenType::kLBrace);
  std::unique_ptr<CompoundStmt> true_stmt = ParseCompoundStmt();

  std::unique_ptr<CompoundStmt> false_stmt;
  if (PeekToken().type == TokenType::kLBrace) {
    PopToken();
    false_stmt = ParseCompoundStmt();
  }

  return std::make_unique<IfStmt>(start_token, std::move(test),
                                  std::move(true_stmt), std::move(false_stmt));
}

std::unique_ptr<Decl> Parser::ParseDecl() {
  Token token = PopToken();
  DeclFlags flags = kDeclFlagsNone;
  if (token.type == TokenType::kLet) {
  } else if (token.type == TokenType::kMut) {
    flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);
  } else if (token.type == TokenType::kStatic) {
    flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);

    Token next = PeekToken();
    if (next.type == TokenType::kMut) {
      flags = static_cast<DeclFlags>(kDeclFlagsMut | flags);
      PopToken();
    }
  }

  return ParseDeclVar(flags);
}

std::unique_ptr<Decl> Parser::ParseDeclVar(DeclFlags flags) {
  Token name = PopTokenType(TokenType::kId);
  PopTokenType(TokenType::kColon);
  std::unique_ptr<Type> type = ParseType();

  Token token = PeekToken();

  std::unique_ptr<Expr> expr;
  if (token.type == TokenType::kEq) {
    PopToken();
    expr = ParseExpr();
  }

  return std::make_unique<Decl>(name, flags, name.text, std::move(type),
                                std::move(expr));
}

std::unique_ptr<Type> Parser::ParseType() {
  Token first = PeekToken();
  if (first.type == TokenType::kStar) {
    PopToken();
    return std::make_unique<PointerType>(first, ParseType());
  }

  Token name = PopTokenType(TokenType::kId);

  Token next = PeekToken();
  if (next.type != TokenType::kLt) {
    return std::make_unique<BaseType>(name, name.text);
  }

  PopToken();

  std::vector<std::unique_ptr<Type>> types;
  while (true) {
    Token next = PeekToken();
    if (next.type == TokenType::kGt) {
      PopToken();
      break;
    }

    types.push_back(ParseType());
  }

  return std::make_unique<TemplateType>(name, name.text, std::move(types));
}

std::unique_ptr<CompoundStmt> Parser::ParseCompoundStmt() {
  Token start_token = PeekToken();

  std::vector<std::unique_ptr<Stmt>> stmts;
  while (true) {
    Token token = PeekToken();
    if (token.type == TokenType::kRBrace) {
      break;
    }
    stmts.push_back(ParseStmt());
    PopTokenType(TokenType::kSemi);
  }

  return std::make_unique<CompoundStmt>(start_token, std::move(stmts));
}

std::unique_ptr<Expr> Parser::ParseExpr() {
  Token token = PeekToken();

  switch (token.type) {
    case TokenType::kId:
      PopToken();
      return std::make_unique<VariableExpr>(token, token.text);
    case TokenType::kIntLit:
      PopToken();
      return std::make_unique<IntExpr>(token);
    case TokenType::kFloatLit:
      PopToken();
      return std::make_unique<FloatExpr>(token);
    case TokenType::kString:
      PopToken();
      return std::make_unique<StringExpr>(token);
    default:
      throw Error(absl::StrCat("Unexpected token: ", token.text),
                  token.location);
  }
}
