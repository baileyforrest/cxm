#include "cxm/parse/parser.h"

#include <stack>
#include <utility>

#include "absl/strings/str_format.h"
#include "cxm/util/error.h"

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

absl::optional<UnaryExprType> TokenToUnaryExprType(TokenType type) {
  switch (type) {
    case TokenType::kMinus:
      return UnaryExprType::kUnaryMinus;
    case TokenType::kLogicNot:
      return UnaryExprType::kLogicNot;
    case TokenType::kBitNot:
      return UnaryExprType::kBitNot;
    case TokenType::kStar:
      return UnaryExprType::kDeref;
    case TokenType::kBitAnd:
      return UnaryExprType::kAddr;
    default:
      return absl::nullopt;
  }
}

}  // namespace

Parser::Parser(Lexer* lexer) : lexer_(lexer) {}

absl::StatusOr<CompilationUnit> Parser::Parse() {
  CompilationUnit cu;
  try {
    cu.global_decls = ParseImpl();
  } catch (Error& error) {
    return absl::InvalidArgumentError(error.ToString());
  }
  return cu;
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

std::vector<Rc<GlobalDecl>> Parser::ParseImpl() {
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
      case TokenType::kLet:
      case TokenType::kMut: {
        result.push_back(Rc<UnaryGlobalDecl>::New(token, ParseDecl()));
        break;
      }
      case TokenType::kClass:
      case TokenType::kStruct:
      case TokenType::kUnion:
        result.push_back(Rc<UnaryGlobalDecl>::New(token, ParseClass()));
        break;
      default:
        throw Error(absl::StrCat("Unexpected token: ", token.text),
                    token.location);
    }
  }

  return result;
}

Rc<IncludeGlobalDecl> Parser::ParseInclude() {
  Token start_token = PopTokenType(TokenType::kInclude);
  Token token = PopToken();

  if (token.type == TokenType::kString) {
    return Rc<IncludeGlobalDecl>::New(
        start_token, IncludeGlobalDeclType::kQuote, token.text);
  }

  if (token.type == TokenType::kLt) {
    Token path = PopToken();
    PopTokenType(TokenType::kGt);
    return Rc<IncludeGlobalDecl>::New(
        start_token, IncludeGlobalDeclType::kBracket, path.text);
  }

  throw Error("Unexpected token after include", token.location);
}

Rc<FuncDecl> Parser::ParseFuncDecl(FuncSpec spec) {
  auto ret = Rc<FuncDecl>::New(PopTokenType(TokenType::kFn));
  ret->spec = spec;
  ret->name = PopTokenType(TokenType::kId).text;
  ret->args = ParseFuncArgs();

  if (PeekToken().type == TokenType::kMut) {
    PopToken();
    ret->spec = static_cast<FuncSpec>(ret->spec & ~kFuncSpecConst);
  }

  PopTokenType(TokenType::kArrow);
  ret->ret_type = ParseType();

  Token next = PopToken();
  if (next.type == TokenType::kSemi) {
    // Just a declaration.
  } else if (next.type == TokenType::kLBrace) {
    ret->body = ParseCompoundStmt();
    PopTokenType(TokenType::kRBrace);
  }

  return ret;
}

std::vector<Rc<Decl>> Parser::ParseFuncArgs() {
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
  return args;
}

Rc<Stmt> Parser::ParseStmt() {
  Token token = PeekToken();

  switch (token.type) {
    case TokenType::kStatic:
    case TokenType::kLet:
    case TokenType::kMut: {
      return Rc<UnaryStmt>::New(ParseDecl());
    }
    case TokenType::kIf:
      return ParseIf();
    case TokenType::kReturn: {
      PopToken();  // kReturn
      Rc<Stmt> stmt = Rc<ReturnStmt>::New(ParseExpr());
      PopTokenType(TokenType::kSemi);
      return stmt;
    }
    default: {
      Rc<Stmt> stmt = Rc<UnaryStmt>::New(ParseExpr());
      PopTokenType(TokenType::kSemi);
      return stmt;
    }
  }
}

Rc<Stmt> Parser::ParseIf() {
  auto ret = Rc<IfStmt>::New(PopTokenType(TokenType::kIf));
  ret->test = ParseExpr();

  PopTokenType(TokenType::kLBrace);
  ret->true_stmt = ParseCompoundStmt();
  PopTokenType(TokenType::kRBrace);

  if (PeekToken().type == TokenType::kLBrace) {
    PopToken();
    ret->false_stmt = ParseCompoundStmt();
    PopTokenType(TokenType::kRBrace);
  }

  return ret;
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

  Rc<Decl> decl = ParseDeclVar(flags);
  PopTokenType(TokenType::kSemi);
  return decl;
}

Rc<Decl> Parser::ParseDeclVar(DeclFlags flags) {
  auto ret = Rc<Decl>::New(PopTokenType(TokenType::kId), flags);

  if (PeekToken().type == TokenType::kColon) {
    PopToken();
    ret->type = ParseType();
  }

  if (PeekToken().type == TokenType::kAssign) {
    PopToken();
    ret->expr = ParseExpr();
  }

  return ret;
}

Rc<Type> Parser::ParseType() {
  // Pointer.
  if (PeekToken().type == TokenType::kStar) {
    auto ret = Rc<PointerType>::New(PopToken());

    if (PeekToken().type == TokenType::kConst) {
      PopToken();
      ret->qual = kCvQualConst;
    }

    ret->sub_type = ParseType();
    return ret;
  }

  // Reference.
  if (PeekToken().type == TokenType::kBitAnd) {
    auto ret = Rc<PointerType>::New(PopToken());
    ret->qual = kCvQualConst;
    if (PeekToken().type == TokenType::kMut) {
      PopToken();
      ret->qual = kCvQualNone;
    }

    ret->sub_type = ParseType();
    return ret;
  }

  return ParseBaseType();
}

Rc<BaseType> Parser::ParseBaseType() {
  auto ret = Rc<BaseType>::New(ParseIdentifier());

  if (PeekToken().type == TokenType::kLt) {
    PopToken();
    while (PeekToken().type != TokenType::kGt) {
      ret->template_args.push_back(ParseType());
    }
    PopToken();
  }

  return ret;
}

Rc<Class> Parser::ParseClass() {
  auto ret = Rc<Class>::New(PopToken());

  ClassAccessType access_type;

  switch (ret->token.type) {
    case TokenType::kClass:
      ret->type = ClassType::kClass;
      access_type = ClassAccessType::kPrivate;
      break;
    case TokenType::kStruct:
      ret->type = ClassType::kStruct;
      access_type = ClassAccessType::kPublic;
      break;
    case TokenType::kUnion:
      ret->type = ClassType::kUnion;
      access_type = ClassAccessType::kPublic;
      break;
    default:
      throw Error(absl::StrCat("Unexpected token: ", ret->token.text),
                  ret->location());
  }
  ret->name = PopTokenType(TokenType::kId).text;

  if (PeekToken().type == TokenType::kColon) {
    PopToken();

    while (PeekToken().type != TokenType::kLBrace) {
      ClassBase& base = ret->bases.emplace_back();
      if (PeekToken().type == TokenType::kPublic) {
        PopToken();
        base.access = ClassAccessType::kPublic;
      } else if (PeekToken().type == TokenType::kPrivate) {
        PopToken();
        base.access = ClassAccessType::kPrivate;
      }

      base.type = ParseBaseType();
    }
  }
  PopTokenType(TokenType::kLBrace);

  while (PeekToken().type != TokenType::kRBrace) {
    if (PeekToken().type == TokenType::kPublic) {
      PopToken();
      PopTokenType(TokenType::kColon);
      access_type = ClassAccessType::kPublic;
      ret->sections.push_back({.access = access_type});
    } else if (PeekToken().type == TokenType::kPrivate) {
      PopToken();
      PopTokenType(TokenType::kColon);
      access_type = ClassAccessType::kPrivate;
      ret->sections.push_back({.access = access_type});
    }

    if (ret->sections.empty()) {
      ret->sections.push_back({.access = access_type});
    }

    if (PeekToken().type == TokenType::kThisType) {
      ret->sections.back().members.push_back(ParseClassCtor(ret->name));
      continue;
    }

    if (PeekToken().type == TokenType::kFn) {
      ret->sections.back().members.push_back(ParseFuncDecl(kFuncSpecConst));
      continue;
    }

    ret->sections.back().members.push_back(ParseDeclVar(kDeclFlagsMut));
    PopTokenType(TokenType::kSemi);
  }
  PopToken();

  return ret;
}

Rc<ClassCtor> Parser::ParseClassCtor(std::string_view name) {
  auto ret = Rc<ClassCtor>::New(PopTokenType(TokenType::kThisType));
  ret->name = name;
  ret->args = ParseFuncArgs();

  if (PeekToken().type == TokenType::kSemi) {
    PopToken();
    return ret;
  }

  if (PeekToken().type == TokenType::kColon) {
    PopToken();

    while (PeekToken().type != TokenType::kLBrace) {
      ClassCtorMemberInit& member_init = ret->member_inits.emplace_back();
      member_init.token = PopTokenType(TokenType::kId);
      PopTokenType(TokenType::kLParen);
      member_init.expr = ParseExpr();
      PopTokenType(TokenType::kRParen);

      if (PeekToken().type != TokenType::kComma) {
        break;
      }
      PopToken();
    }
  }

  PopTokenType(TokenType::kLBrace);
  ret->body = ParseCompoundStmt();
  PopTokenType(TokenType::kRBrace);
  return ret;
}

Rc<CompoundStmt> Parser::ParseCompoundStmt() {
  auto ret = Rc<CompoundStmt>::New(PeekToken());

  while (true) {
    Token token = PeekToken();
    if (token.type == TokenType::kRBrace) {
      break;
    }
    ret->stmts.push_back(ParseStmt());
  }

  return ret;
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
      top.expr = Rc<BinaryExpr>::New(top.token, top.op, top.expr, right_most);
      top.op = *next_bin_expr_type;
      top.token = next_op_token;
    } else {
      stack.push({right_most, *next_bin_expr_type, next_op_token});
    }
  }

  while (!stack.empty()) {
    auto& top = stack.top();
    right_most = Rc<BinaryExpr>::New(top.token, top.op, top.expr, right_most);
    stack.pop();
  }

  return right_most;
}

Rc<Expr> Parser::ParseUnaryExpr() {
  struct UnaryExprAndToken {
    UnaryExprType type;
    Token token;
  };
  std::stack<UnaryExprAndToken> unary_exprs;

  while (true) {
    absl::optional<UnaryExprType> unary_expr =
        TokenToUnaryExprType(PeekToken().type);
    if (!unary_expr.has_value()) {
      break;
    }

    unary_exprs.push({*unary_expr, PopToken()});
  }

  Rc<Expr> expr;
  switch (PeekToken().type) {
    case TokenType::kId:
    case TokenType::kScope:
      expr = Rc<VariableExpr>::New(ParseIdentifier());
      break;
    case TokenType::kIntLit:
      return Rc<IntExpr>::New(PopToken());
    case TokenType::kFloatLit:
      return Rc<FloatExpr>::New(PopToken());
    case TokenType::kString:
      return Rc<StringExpr>::New(PopToken());
    default:
      throw Error(absl::StrCat("Unexpected token: ", PeekToken().text),
                  PeekToken().location);
      break;
  }

  while (true) {
    bool done = false;
    switch (PeekToken().type) {
      case TokenType::kLParen:
        expr = ParseCallExpr(expr);
        break;
      case TokenType::kLBrack: {
        Token token = PopToken();
        Rc<Expr> subscript = ParseExpr();
        PopTokenType(TokenType::kRBrack);

        expr = Rc<BinaryExpr>::New(token, BinExprType::kSubscript, expr,
                                   subscript);
        break;
      }
      case TokenType::kDot: {
        Token token = PopToken();
        Token member_name = PopTokenType(TokenType::kId);
        expr = Rc<MemberAccessExpr>::New(token, expr, member_name.text);
        break;
      }
      default:
        done = true;
        break;
    }

    if (done) {
      break;
    }
  }

  while (!unary_exprs.empty()) {
    auto& top = unary_exprs.top();
    expr = Rc<UnaryExpr>::New(top.token, top.type, expr);
    unary_exprs.pop();
  }

  return expr;
}

Rc<CallExpr> Parser::ParseCallExpr(Rc<Expr> func) {
  auto ret = Rc<CallExpr>::New(PopTokenType(TokenType::kLParen));
  ret->func = func;

  while (PeekToken().type != TokenType::kRParen) {
    ret->args.push_back(ParseExpr());
    if (PeekToken().type == TokenType::kRParen) {
      break;
    }
    PopTokenType(TokenType::kComma);
  }
  PopTokenType(TokenType::kRParen);

  return ret;
}

Identifier Parser::ParseIdentifier() {
  Identifier id = {.token = PeekToken()};

  if (id.token.type == TokenType::kCond) {
    id.fully_qualified = true;
    PopToken();
  }

  Token right_most = PopTokenType(TokenType::kId);
  while (PeekToken().type == TokenType::kScope) {
    PopToken();
    id.namespaces.push_back(right_most.text);
    right_most = PopTokenType(TokenType::kId);
  }

  id.name = right_most.text;
  return id;
}
