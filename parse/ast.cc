#include "parse/ast.h"

#include "absl/strings/str_cat.h"

void AstStringBuilder::Append(std::string_view text) {
  for (auto c : text) {
    if (c == '\n') {
      indent_next_ = true;
    } else {
      if (indent_next_) {
        indent_next_ = false;
        for (int i = 0; i < indent_; i += 1) {
          str_.push_back(' ');
          str_.push_back(' ');
        }
      }
    }
    str_.push_back(c);
  }
}

void BaseType::AppendString(AstStringBuilder* builder) const {
  builder->Append(token().text);
}

void TemplateType::AppendString(AstStringBuilder* builder) const {
  builder->Append(token().text);
  builder->Append("<");
  for (const auto& arg : args_) {
    arg->AppendString(builder);
    builder->Append(", ");
  }
  builder->Append(">");
}

void PointerType::AppendString(AstStringBuilder* builder) const {
  builder->Append("*");
  sub_type_->AppendString(builder);
}

void ReferenceType::AppendString(AstStringBuilder* builder) const {
  builder->Append("&");
  sub_type_->AppendString(builder);
}

std::string_view ExprTypeToString(ExprType type) {
#define CASE_STR(item) \
  case ExprType::item: \
    return #item

  switch (type) {
    CASE_STR(kVariable);
    CASE_STR(kInt);
    CASE_STR(kFloat);
    CASE_STR(kString);
    CASE_STR(kBinary);
    CASE_STR(kUnary);
    CASE_STR(kCast);
    CASE_STR(kCall);
    CASE_STR(kMemberAccess);
    CASE_STR(kInitList);
  }

  return "UNKNOWN ExprType";
#undef CASE_STR
}

void Expr::AppendString(AstStringBuilder* builder) const {
  builder->Append(ExprTypeToString(GetExprType()));
  builder->Append("(");
  builder->Append(token().text);
  builder->Append(")");
}

void VariableExpr::AppendString(AstStringBuilder* builder) const {
  if (fully_qualified_) {
    builder->Append("::");
  }
  for (const auto& item : namespaces_) {
    builder->Append(item);
    builder->Append("::");
  }
  builder->Append(name_);
}

std::string_view BinExprTypeToString(BinExprType type) {
#define CASE_STR(item)    \
  case BinExprType::item: \
    return #item

  switch (type) {
    CASE_STR(kPlus);
    CASE_STR(kMinus);
    CASE_STR(kTimes);
    CASE_STR(kDiv);
    CASE_STR(kMod);
    CASE_STR(kLt);
    CASE_STR(kLe);
    CASE_STR(kGt);
    CASE_STR(kGe);
    CASE_STR(kEq);
    CASE_STR(kNe);
    CASE_STR(kBitAnd);
    CASE_STR(kBitXor);
    CASE_STR(kBitOr);
    CASE_STR(kLShift);
    CASE_STR(kRShift);
    CASE_STR(kLogicAnd);
    CASE_STR(kLogicOr);
    CASE_STR(kSubscript);
  }

  return "UNKNOWN BinExprType";
#undef CASE_STR
}

void BinaryExpr::AppendString(AstStringBuilder* builder) const {
  builder->Append("BINARY(");
  builder->Append(BinExprTypeToString(bin_expr_type_));
  builder->Append(",");

  builder->Indent();
  builder->Append("\n");

  left_->AppendString(builder);
  builder->Append(",\n");
  right_->AppendString(builder);

  builder->DeIndent();
  builder->Append(",\n)");
}

std::string_view UnaryExprTypeToString(UnaryExprType type) {
#define CASE_STR(item)      \
  case UnaryExprType::item: \
    return #item

  switch (type) {
    CASE_STR(kUnaryMinus);
    CASE_STR(kLogicNot);
    CASE_STR(kBitNot);
    CASE_STR(kDeref);
    CASE_STR(kAddr);
    CASE_STR(kParen);
  }

  return "UNKNOWN UnaryExprType";
#undef CASE_STR
}

void UnaryExpr::AppendString(AstStringBuilder* builder) const {
  builder->Append("UNARY(");
  builder->Append(UnaryExprTypeToString(unary_expr_type_));
  builder->Append(",");

  builder->Indent();
  builder->Append("\n");

  expr_->AppendString(builder);

  builder->DeIndent();
  builder->Append("\n)");
}

void CallExpr::AppendString(AstStringBuilder* builder) const {
  builder->Append("CALL(");
  builder->Indent();
  builder->Append("\n");
  func_->AppendString(builder);

  for (auto& expr : args_) {
    builder->Append(",\n");
    expr->AppendString(builder);
  }

  builder->DeIndent();
  builder->Append("\n)");
}

void MemberAccessExpr::AppendString(AstStringBuilder* builder) const {
  builder->Append("MEMBER_ACCESS(");
  builder->Indent();
  builder->Append("\n");

  expr_->AppendString(builder);
  builder->Append(",\n");

  builder->Append(member_name_);

  builder->DeIndent();
  builder->Append("\n)");
}

void InitListExpr::AppendString(AstStringBuilder* builder) const {
  builder->Append("INIT_LIST(");
  builder->Indent();
  builder->Append("\n");

  for (const auto& expr : exprs_) {
    expr->AppendString(builder);
    builder->Append(",\n");
  }

  builder->DeIndent();
  builder->Append("\n)");
}

std::string DeclFlagsToString(DeclFlags decl_flags) {
  if (decl_flags == kDeclFlagsNone) {
    return "kDeclFlagsNone";
  }

  std::string ret;
  if (decl_flags & kDeclFlagsMut) {
    absl::StrAppend(&ret, "kDeclFlagsMut|");
  }

  if (decl_flags & kDeclFlagsStatic) {
    absl::StrAppend(&ret, "kDeclFlagsStatic|");
  }

  return ret;
}

void Decl::AppendString(AstStringBuilder* builder) const {
  builder->Append("DECL(");
  builder->Append(DeclFlagsToString(decl_flags_));
  builder->Append(", ");
  builder->Append(name_);
  builder->Append(", ");

  if (type_) {
    type_->AppendString(builder);
  }

  if (expr_) {
    expr_->AppendString(builder);
  }

  builder->Append(")");
}

std::string_view StmtTypeToString(StmtType type) {
#define CASE_STR(item) \
  case StmtType::item: \
    return #item

  switch (type) {
    CASE_STR(kCompound);
    CASE_STR(kDecl);
    CASE_STR(kExpr);
    CASE_STR(kIf);
    CASE_STR(kWhile);
    CASE_STR(kFor);
    CASE_STR(kSwitch);
    CASE_STR(kReturn);
  }

  return "UNKNOWN Stmt";
#undef CASE_STR
}

void CompoundStmt::AppendString(AstStringBuilder* builder) const {
  builder->Append("COMPOUND(");
  builder->Indent();
  builder->Append("\n");

  for (const auto& stmt : stmts_) {
    stmt->AppendString(builder);
    builder->Append(",\n");
  }

  builder->DeIndent();
  builder->Append(")");
}

void IfStmt::AppendString(AstStringBuilder* builder) const {
  builder->Append("IF(");
  builder->Indent();
  builder->Append("\n");

  test_->AppendString(builder);
  builder->Append(",\n");

  true_->AppendString(builder);
  builder->Append(",\n");

  if (false_) {
    false_->AppendString(builder);
    builder->Append(",\n");
  }

  builder->DeIndent();
  builder->Append(")");
}

void WhileStmt::AppendString(AstStringBuilder* builder) const {
  builder->Append("WHILE(");
  builder->Indent();
  builder->Append("\n");

  test_->AppendString(builder);
  builder->Append(",\n");

  body_->AppendString(builder);
  builder->Append(",\n");

  builder->DeIndent();
  builder->Append("\n)");
}

void ForStmt::AppendString(AstStringBuilder* builder) const {
  builder->Append("FOR(");
  builder->Indent();
  builder->Append("\n");

  decl_->AppendString(builder);
  builder->Append(",\n");

  expr_->AppendString(builder);
  builder->Append(",\n");

  builder->DeIndent();
  builder->Append("\n)");
}

void SwitchStmt::AppendString(AstStringBuilder* builder) const {
  builder->Append("SWITCH(");
  builder->Indent();
  builder->Append("\n");

  test_->AppendString(builder);
  builder->Append(",\n");

  builder->Append("cases: {");
  builder->Indent();
  builder->Append("\n");
  for (auto& item : cases_) {
    builder->Append("case: {");
    builder->Indent();
    builder->Append("\n");

    item.test->AppendString(builder);
    builder->Append(",\n");
    item.expr->AppendString(builder);

    builder->DeIndent();
    builder->Append("}\n");
  }
  builder->DeIndent();
  builder->Append("}\n");

  default_->AppendString(builder);
  builder->Append(",\n");

  builder->DeIndent();
  builder->Append("\n)");
}

void IncludeGlobalDecl::AppendString(AstStringBuilder* builder) const {
  builder->Append("INCLUDE ");
  if (type_ == IncludeGlobalDeclType::kBracket) {
    builder->Append("<");
  } else {
    builder->Append("\"");
  }
  builder->Append(path_);
  if (type_ == IncludeGlobalDeclType::kBracket) {
    builder->Append(">");
  } else {
    builder->Append("\"");
  }
}

void FuncDecl::AppendString(AstStringBuilder* builder) const {
  builder->Append("FUNC(");
  builder->Append(name_);
  builder->Append(", {");
  builder->Indent();
  builder->Append("\n");

  for (const auto& arg : args_) {
    arg->AppendString(builder);
    builder->Append(",\n");
  }

  builder->Append("},\n");

  ret_type_->AppendString(builder);
  builder->Append(",\n");

  if (body_) {
    body_->AppendString(builder);
  }

  builder->DeIndent();
  builder->Append("\n)");
}
