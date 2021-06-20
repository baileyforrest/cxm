#pragma once

#include <memory>
#include <vector>

#include "absl/status/statusor.h"
#include "cxm/lex/lexer.h"
#include "cxm/parse/ast.h"
#include "cxm/util/rc.h"

class Parser {
 public:
  explicit Parser(Lexer* lexer);

  absl::StatusOr<std::vector<Rc<GlobalDecl>>> Parse();

 private:
  Token HandleEof(Token token);
  Token PeekToken();
  Token PopToken();
  Token PopTokenType(TokenType type);

  std::vector<Rc<GlobalDecl>> ParseImpl();
  Rc<IncludeGlobalDecl> ParseInclude();
  Rc<FuncDecl> ParseFuncDecl();

  Rc<Stmt> ParseStmt();
  Rc<Stmt> ParseIf();

  Rc<Decl> ParseDecl();
  Rc<Decl> ParseDeclVar(DeclFlags flags = kDeclFlagsNone);
  Rc<Type> ParseType();
  Rc<CompoundStmt> ParseCompoundStmt();

  Rc<Expr> ParseExpr();
  Rc<Expr> ParseUnaryExpr();
  Rc<VariableExpr> ParseVariableExpr();
  Rc<CallExpr> ParseCallExpr(Rc<Expr> func);

  std::optional<Token> last_token_;
  Lexer* const lexer_;
};
