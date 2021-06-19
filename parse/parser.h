#pragma once

#include <memory>
#include <vector>

#include "lex/lexer.h"
#include "parse/ast.h"
#include "util/rc.h"

class Parser {
 public:
  explicit Parser(Lexer* lexer);

  std::vector<Rc<GlobalDecl>> Parse();

 private:
  Token HandleEof(Token token);
  Token PeekToken();
  Token PopToken();
  Token PopTokenType(TokenType type);

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

  std::optional<Token> last_token_;
  Lexer* const lexer_;
};
