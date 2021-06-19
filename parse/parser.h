#pragma once

#include <memory>
#include <vector>

#include "bcf/err.h"
#include "lex/lexer.h"
#include "parse/ast.h"

class Parser {
 public:
  explicit Parser(Lexer* lexer);

  Parser(const Parser&) = delete;
  Parser& operator=(const Parser&) = delete;

  bcf::Result<std::vector<std::unique_ptr<GlobalDecl>>> Parse();

 private:
  bcf::Result<Token> HandleEof(absl::optional<Token> token);
  bcf::Result<Token> PeekToken();
  bcf::Result<Token> PopToken();
  bcf::Result<Token> PopTokenType(TokenType type);

  bcf::Result<std::unique_ptr<IncludeGlobalDecl>> ParseInclude();
  bcf::Result<std::unique_ptr<FuncDecl>> ParseFuncDecl();
  bcf::Result<std::unique_ptr<Decl>> ParseDecl();
  bcf::Result<std::unique_ptr<Decl>> ParseDeclVar(
      DeclFlags flags = kDeclFlagsNone);
  bcf::Result<std::unique_ptr<Type>> ParseType();
  bcf::Result<std::unique_ptr<Expr>> ParseCompoundExpr();
  bcf::Result<std::unique_ptr<Expr>> ParseSingleExpr();

  absl::optional<Token> last_token_;
  Lexer* const lexer_;
};
