#include "parse/parser.h"

#include <stack>
#include <utility>

#include "absl/strings/str_format.h"
#include "util/error.h"

namespace {

std::optional<BinExprType> TokenToBinExprType(TokenType type) {
#define CASE(token, expr_type) \
  case TokenType::token:       \
    return BinExprType::expr_type

  switch (type) {
    CASE(kStar, kTimes);
    CASE(kDiv, kDiv);
    CASE(kMod, kMod);
    CASE(kPlus, kPlus);
    CASE(kMinus, kMinus);
    CASE(kRShift, kRShift);
    CASE(kLShift, kLShift);
    CASE(kLt, kLt);
    CASE(kGt, kGt);
    CASE(kLe, kLe);
    CASE(kGe, kGe);
    CASE(kEq, kEq);
    CASE(kNe, kNe);
    CASE(kBitAnd, kBitAnd);
    CASE(kBitXor, kBitXor);
    CASE(kBitOr, kBitOr);
    CASE(kLogicAnd, kLogicAnd);
    CASE(kLogicOr, kLogicOr);
    default:
      return absl::nullopt;
  }
#undef CASE
}

// Based on:
// https://en.cppreference.com/w/cpp/language/operator_precedence
int BinExprPrecedence(BinExprType type) {
  constexpr int kMax = 100;

  switch (type) {
    case BinExprType::kTimes:
    case BinExprType::kDiv:
    case BinExprType::kMod:
      return kMax - 5;

    case BinExprType::kPlus:
    case BinExprType::kMinus:
      return kMax - 6;

    case BinExprType::kRShift:
    case BinExprType::kLShift:
      return kMax - 7;

    case BinExprType::kLt:
    case BinExprType::kGt:
    case BinExprType::kLe:
    case BinExprType::kGe:
      return kMax - 9;

    case BinExprType::kEq:
    case BinExprType::kNe:
      return kMax - 10;

    case BinExprType::kBitAnd:
      return kMax - 11;
    case BinExprType::kBitXor:
      return kMax - 12;
    case BinExprType::kBitOr:
      return kMax - 13;
    case BinExprType::kLogicAnd:
      return kMax - 14;
    case BinExprType::kLogicOr:
      return kMax - 15;

    default:
      break;
  }

  // Should not happen.
  abort();
}

}  // namespace

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
      return Rc<ExprStmt>::Make(ParseExpr());
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
  Rc<Expr> expr = ParseUnaryExpr();

  absl::optional<BinExprType> bin_expr_type =
      TokenToBinExprType(PeekToken().type);
  if (!bin_expr_type.has_value()) {
    return expr;
  }

  struct ExprAndOp {
    Rc<Expr> expr;
    BinExprType op;
    Token token;
  };
  std::stack<ExprAndOp> stack;
  stack.push(ExprAndOp{expr, *bin_expr_type, PopToken()});

  Rc<Expr> right_most;
  while (true) {
    right_most = ParseUnaryExpr();
    absl::optional<BinExprType> next_bin_expr_type =
        TokenToBinExprType(PeekToken().type);
    if (!next_bin_expr_type.has_value()) {
      break;
    }
    Token next_op_token = PopToken();

    auto& top = stack.top();
    if (BinExprPrecedence(top.op) >= BinExprPrecedence(*next_bin_expr_type)) {
      top.expr = Rc<BinaryExpr>::Make(top.token, top.op, top.expr, right_most);
      top.op = *next_bin_expr_type;
      top.token = next_op_token;
    } else {
      stack.push({right_most, *next_bin_expr_type, next_op_token});
    }
  }

  while (!stack.empty()) {
    auto& top = stack.top();
    right_most = Rc<BinaryExpr>::Make(top.token, top.op, top.expr, right_most);
    stack.pop();
  }

  return right_most;
}

Rc<Expr> Parser::ParseUnaryExpr() {
  Token token = PeekToken();

  // TODO(bcf): Handle unary ops.

  Rc<Expr> expr;
  switch (token.type) {
    case TokenType::kId:
    case TokenType::kScope:
      expr = ParseVariableExpr();
      break;
    case TokenType::kIntLit:
      return Rc<IntExpr>::Make(PopToken());
    case TokenType::kFloatLit:
      return Rc<FloatExpr>::Make(PopToken());
    case TokenType::kString:
      return Rc<StringExpr>::Make(PopToken());
    default:
      break;
  }

  if (PeekToken().type == TokenType::kLParen) {
    return ParseCallExpr(expr);
  }

  return expr;
}

Rc<VariableExpr> Parser::ParseVariableExpr() {
  Token start_token = PeekToken();

  bool fully_qualified = false;
  if (start_token.type == TokenType::kCond) {
    fully_qualified = true;
    PopToken();
  }

  std::vector<absl::string_view> namespaces;
  Token right_most = PopTokenType(TokenType::kId);

  while (PeekToken().type == TokenType::kScope) {
    PopToken();
    namespaces.push_back(right_most.text);
    right_most = PopTokenType(TokenType::kId);
  }

  return Rc<VariableExpr>::Make(start_token, fully_qualified, namespaces,
                                right_most.text);
}

Rc<CallExpr> Parser::ParseCallExpr(Rc<Expr> func) {
  Token start_token = PopToken();  // kLParen.

  std::vector<Rc<Expr>> args;
  while (PeekToken().type != TokenType::kRParen) {
    args.push_back(ParseExpr());
  }
  PopToken();  // kRParen.

  return Rc<CallExpr>::Make(start_token, func, std::move(args));
}
