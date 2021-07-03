#include "cxm/gen/code-gen.h"

void CodeGen::Run(const CompilationUnit& cu) {
  for (const auto& decl : cu.global_decls) {
    decl->Accept(*this);
    Append("\n");
  }
}

void CodeGen::Visit(const BaseType& node) { Append(node.name); }

void CodeGen::Visit(const TemplateType& node) {
  Append(node.name);
  Append("<");
  for (const auto& item : node.args) {
    item->Accept(*this);
  }
  Append(">");
}

void CodeGen::Visit(const PointerType& node) {
  node.sub_type->Accept(*this);
  Append("*");
}

void CodeGen::Visit(const ReferenceType& node) {
  node.sub_type->Accept(*this);
  Append("&");
}

void CodeGen::Visit(const VariableExpr& node) {
  if (node.fully_qualified) {
    Append("::");
  }
  for (const auto& item : node.namespaces) {
    Append(item);
    Append("::");
  }
  Append(node.name);
}

void CodeGen::Visit(const IntExpr& node) { Append(node.token.text); }

void CodeGen::Visit(const FloatExpr& node) { Append(node.token.text); }

void CodeGen::Visit(const StringExpr& node) {
  Append("\"");
  Append(node.token.text);
  Append("\"");
}

void CodeGen::Visit(const BinaryExpr& node) {
#define CASE(type, val)   \
  case BinExprType::type: \
    Append(val);          \
    break

  node.left->Accept(*this);

  if (node.bin_expr_type != BinExprType::kSubscript) {
    Append(" ");
  }

  switch (node.bin_expr_type) {
    CASE(kPlus, "+");
    CASE(kMinus, "-");
    CASE(kTimes, "*");
    CASE(kDiv, "/");
    CASE(kMod, "%");
    CASE(kLt, "<");
    CASE(kLe, "<=");
    CASE(kGt, ">");
    CASE(kGe, ">=");
    CASE(kEq, "==");
    CASE(kNe, "!=");
    CASE(kBitAnd, "&");
    CASE(kBitXor, "^");
    CASE(kBitOr, "|");
    CASE(kLShift, "<<");
    CASE(kRShift, ">>");
    CASE(kLogicAnd, "&&");
    CASE(kLogicOr, "||");

    case BinExprType::kSubscript:
      Append("[");
      node.right->Accept(*this);
      Append("]");
      return;
  }

  Append(" ");
  node.right->Accept(*this);
#undef CASE
}

void CodeGen::Visit(const UnaryExpr& node) {
#define CASE(type, val)     \
  case UnaryExprType::type: \
    Append(val);            \
    break

  switch (node.unary_expr_type) {
    CASE(kUnaryMinus, "-");
    CASE(kLogicNot, "!");
    CASE(kBitNot, "~");
    CASE(kDeref, "*");
    CASE(kAddr, "&");

    case UnaryExprType::kParen: {
      Append("(");
      node.expr->Accept(*this);
      Append(")");
      break;
    }
  }

  node.expr->Accept(*this);
#undef CASE
}

void CodeGen::Visit(const CallExpr& node) {
  node.func->Accept(*this);
  Append("(");
  for (const auto& arg : node.args) {
    arg->Accept(*this);
    if (&arg != &node.args.back()) {
      Append(", ");
    }
  }
  Append(")");
}

void CodeGen::Visit(const MemberAccessExpr& node) {
  node.expr->Accept(*this);
  Append(".");
  Append(node.member_name);
}

void CodeGen::Visit(const InitListExpr& node) {
  Append("{");
  for (const auto& expr : node.exprs) {
    expr->Accept(*this);
  }
  Append("}");
}

void CodeGen::Visit(const Decl& node) {
  if (node.decl_flags & kDeclFlagsStatic) {
    Append("static ");
  }

  const Type* type = node.type.get();

  bool is_const = !(node.decl_flags & kDeclFlagsMut);
  bool late_const =
      type != nullptr && type->GetTypeType() == TypeType::kPointer;

  if (is_const && !late_const) {
    Append("const ");
  }

  if (node.type) {
    node.type->Accept(*this);
  } else {
    Append("auto");
  }
  Append(" ");
  if (is_const && late_const) {
    Append("const ");
  }

  Append(node.name);
  if (node.expr) {
    Append(" = ");
    node.expr->Accept(*this);
  }
}

void CodeGen::Visit(const CompoundStmt& node) {
  Append("{");
  Indent();
  Append("\n");

  for (const auto& stmt : node.stmts) {
    stmt->Accept(*this);
    Append("\n");
  }

  DeIndent();
  Append("}\n");
}

void CodeGen::Visit(const DeclStmt& node) {
  node.decl->Accept(*this);
  Append(";");
}

void CodeGen::Visit(const ExprStmt& node) {
  node.expr->Accept(*this);
  Append(";");
}

void CodeGen::Visit(const IfStmt& node) {
  Append("if (");
  node.test->Accept(*this);
  Append(") ");
  node.true_stmt->Accept(*this);

  if (node.false_stmt) {
    Append(" else ");
    node.false_stmt->Accept(*this);
  }
}

void CodeGen::Visit(const WhileStmt& node) {
  Append("while (");
  node.test->Accept(*this);
  Append(")");
  node.body->Accept(*this);
}

void CodeGen::Visit(const ForStmt& node) {
  Append("for (");
  node.decl->Accept(*this);
  Append(" : ");
  node.expr->Accept(*this);
  Append(")");
  node.body->Accept(*this);
}

void CodeGen::Visit(const SwitchStmt& node) {
  Append("switch (");
  node.test->Accept(*this);
  Append(") {\n");
  Indent();

  for (const auto& item : node.cases) {
    Append("case ");
    item.test->Accept(*this);
    Append(":\n");
    Indent();
    item.stmt->Accept(*this);
    DeIndent();
  }

  if (node.default_expr) {
    Append("default:\n");
    Indent();
    node.default_expr->Accept(*this);
    DeIndent();
  }

  DeIndent();
  Append("}\n");
}

void CodeGen::Visit(const ReturnStmt& node) {
  Append("return ");
  node.expr->Accept(*this);
  Append(";");
}

void CodeGen::Visit(const IncludeGlobalDecl& node) {
  Append("#include ");
  if (node.type == IncludeGlobalDeclType::kBracket) {
    Append("<");
  } else {
    Append("\"");
  }
  Append(node.path);
  if (node.type == IncludeGlobalDeclType::kBracket) {
    Append(">");
  } else {
    Append("\"");
  }
}

void CodeGen::Visit(const UnaryGlobalDecl& node) {
  std::visit([&](const auto& arg) { arg->Accept(*this); }, node.val);
}

void CodeGen::Visit(const FuncDecl& node) {
  node.ret_type->Accept(*this);
  Append("\n");
  Append(node.name);
  Append("(");

  for (const auto& arg : node.args) {
    arg->Accept(*this);
    if (&arg != &node.args.back()) {
      Append(", ");
    }
  }

  Append(") ");
  node.body->Accept(*this);
}

void CodeGen::Append(std::string_view text) {
  for (auto c : text) {
    if (c == '\n') {
      indent_next_ = true;
    } else {
      if (indent_next_) {
        indent_next_ = false;
        for (int i = 0; i < indent_; i += 1) {
          ostream_.put(' ');
          ostream_.put(' ');
        }
      }
    }
    ostream_.put(c);
  }
}
