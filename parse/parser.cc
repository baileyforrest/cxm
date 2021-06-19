#include "parse/parser.h"

#include <utility>

#include "absl/strings/str_format.h"
#include "util/error.h"

Parser::Parser(Lexer* lexer) : lexer_(lexer) {}

std::vector<Rc<GlobalDecl>> Parser::Parse() {
  std::vector<Rc<GlobalDecl>> result;
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
        result.push_back(Rc<DeclGlobalDecl>::Make(token, ParseDecl()));
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

Rc<IncludeGlobalDecl> Parser::ParseInclude() {
  Token start_token = PopTokenType(TokenType::kInclude);
  Token token = PopToken();

  if (token.type == TokenType::kString) {
    return Rc<IncludeGlobalDecl>::Make(
        start_token, IncludeGlobalDeclType::kQuote, token.text);
  }

  if (token.type == TokenType::kLt) {
    Token path = PopToken();
    PopTokenType(TokenType::kGt);
    return Rc<IncludeGlobalDecl>::Make(
        start_token, IncludeGlobalDeclType::kBracket, path.text);
  }

  throw Error("Unexpected token after include", token.location);
}

Rc<FuncDecl> Parser::ParseFuncDecl() {
  Token start_token = PopTokenType(TokenType::kFn);
  Token name = PopTokenType(TokenType::kId);
  PopTokenType(TokenType::kLParen);

  std::vector<Rc<Decl>> args;
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

  Rc<Type> ret_type = ParseType();

  Token next = PopToken();
  Rc<CompoundStmt> body;

  if (next.type == TokenType::kSemi) {
    // Just a declaration.
  } else if (next.type == TokenType::kLBrace) {
    body = ParseCompoundStmt();
  }

  return Rc<FuncDecl>::Make(start_token, name.text, std::move(args), ret_type,
                            body);
}

Rc<Stmt> Parser::ParseStmt() {
  Token token = PeekToken();
  switch (token.type) {
    case TokenType::kStatic:
    case TokenType::kLet:
    case TokenType::kMut: {
      return Rc<DeclStmt>::Make(ParseDecl());
    }
    case TokenType::kIf:
      return ParseIf();
    default:
      throw Error(absl::StrCat("Unexpected token: ", token.text),
                  token.location);
  }
}

Rc<Stmt> Parser::ParseIf() {
  Token start_token = PopTokenType(TokenType::kIf);
  Rc<Expr> test = ParseExpr();

  PopTokenType(TokenType::kLBrace);
  Rc<CompoundStmt> true_stmt = ParseCompoundStmt();

  Rc<CompoundStmt> false_stmt;
  if (PeekToken().type == TokenType::kLBrace) {
    PopToken();
    false_stmt = ParseCompoundStmt();
  }

  return Rc<IfStmt>::Make(start_token, test, true_stmt, false_stmt);
}

Rc<Decl> Parser::ParseDecl() {
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

Rc<Decl> Parser::ParseDeclVar(DeclFlags flags) {
  Token name = PopTokenType(TokenType::kId);
  PopTokenType(TokenType::kColon);
  Rc<Type> type = ParseType();

  Token token = PeekToken();

  Rc<Expr> expr;
  if (token.type == TokenType::kEq) {
    PopToken();
    expr = ParseExpr();
  }

  return Rc<Decl>::Make(name, flags, name.text, type, expr);
}

Rc<Type> Parser::ParseType() {
  Token first = PeekToken();
  if (first.type == TokenType::kStar) {
    PopToken();
    return Rc<PointerType>::Make(first, ParseType());
  }

  Token name = PopTokenType(TokenType::kId);

  Token next = PeekToken();
  if (next.type != TokenType::kLt) {
    return Rc<BaseType>::Make(name, name.text);
  }

  PopToken();

  std::vector<Rc<Type>> types;
  while (true) {
    Token next = PeekToken();
    if (next.type == TokenType::kGt) {
      PopToken();
      break;
    }

    types.push_back(ParseType());
  }

  return Rc<TemplateType>::Make(name, name.text, std::move(types));
}

Rc<CompoundStmt> Parser::ParseCompoundStmt() {
  Token start_token = PeekToken();

  std::vector<Rc<Stmt>> stmts;
  while (true) {
    Token token = PeekToken();
    if (token.type == TokenType::kRBrace) {
      break;
    }
    stmts.push_back(ParseStmt());
    PopTokenType(TokenType::kSemi);
  }

  return Rc<CompoundStmt>::Make(start_token, std::move(stmts));
}

Rc<Expr> Parser::ParseExpr() {
  Token token = PeekToken();

  switch (token.type) {
    case TokenType::kId:
      PopToken();
      return Rc<VariableExpr>::Make(token, token.text);
    case TokenType::kIntLit:
      PopToken();
      return Rc<IntExpr>::Make(token);
    case TokenType::kFloatLit:
      PopToken();
      return Rc<FloatExpr>::Make(token);
    case TokenType::kString:
      PopToken();
      return Rc<StringExpr>::Make(token);
    default:
      throw Error(absl::StrCat("Unexpected token: ", token.text),
                  token.location);
  }
}
