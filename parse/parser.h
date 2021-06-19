#pragma once

#include <memory>
#include <vector>

#include "absl/status/statusor.h"
#include "lex/lexer.h"
#include "parse/ast.h"

class Parser {
 public:
  explicit Parser(Lexer* lexer);

  Parser(const Parser&) = delete;
  Parser& operator=(const Parser&) = delete;

  absl::StatusOr<std::vector<std::unique_ptr<GlobalDecl>>> Parse();

 private:
  absl::StatusOr<Token> HandleEof(absl::optional<Token> token);
  absl::StatusOr<Token> PeekToken();
  absl::StatusOr<Token> PopToken();
  absl::StatusOr<Token> PopTokenType(TokenType type);

  absl::StatusOr<std::unique_ptr<IncludeGlobalDecl>> ParseInclude();
  absl::StatusOr<std::unique_ptr<FuncDecl>> ParseFuncDecl();
  absl::StatusOr<std::unique_ptr<Decl>> ParseDecl();
  absl::StatusOr<std::unique_ptr<Decl>> ParseDeclVar(
      DeclFlags flags = kDeclFlagsNone);
  absl::StatusOr<std::unique_ptr<Type>> ParseType();
  absl::StatusOr<std::unique_ptr<Expr>> ParseCompoundExpr();
  absl::StatusOr<std::unique_ptr<Expr>> ParseSingleExpr();

  absl::optional<Token> last_token_;
  Lexer* const lexer_;
};