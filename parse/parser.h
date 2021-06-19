#pragma once

#include <memory>
#include <vector>

#include "lex/lexer.h"
#include "parse/ast.h"

class Parser {
 public:
  explicit Parser(Lexer* lexer);

  std::vector<std::unique_ptr<GlobalDecl>> Parse();

 private:
  Token HandleEof(Token token);
  Token PeekToken();
  Token PopToken();
  Token PopTokenType(TokenType type);

  std::unique_ptr<IncludeGlobalDecl> ParseInclude();
  std::unique_ptr<FuncDecl> ParseFuncDecl();

  std::unique_ptr<Stmt> ParseStmt();
  std::unique_ptr<Stmt> ParseIf();

  std::unique_ptr<Decl> ParseDecl();
  std::unique_ptr<Decl> ParseDeclVar(DeclFlags flags = kDeclFlagsNone);
  std::unique_ptr<Type> ParseType();
  std::unique_ptr<CompoundStmt> ParseCompoundStmt();
  std::unique_ptr<Expr> ParseExpr();

  absl::optional<Token> last_token_;
  Lexer* const lexer_;
};
