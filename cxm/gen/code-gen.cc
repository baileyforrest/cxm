#include "cxm/gen/code-gen.h"

void CodeGen::Run(const CompilationUnit& cu) {
  for (const auto& decl : cu.global_decls) {
    decl->Accept(*this);
    Emit("\n");
  }
}

void CodeGen::Visit(const BaseType& node) {
  EmitIdentifier(node.id);
  if (!node.template_args.empty()) {
    Emit("<");
    for (const auto& item : node.template_args) {
      item->Accept(*this);
    }
    Emit(">");
  }
}

void CodeGen::Visit(const PointerType& node) {
  node.sub_type->Accept(*this);
  Emit("*");
}

void CodeGen::Visit(const ReferenceType& node) {
  node.sub_type->Accept(*this);
  Emit("&");
}

void CodeGen::Visit(const ClassCtor& node) {
  Emit(node.name, "(");
  for (const auto& arg : node.args) {
    if (&arg != &node.args.front()) {
      Emit(", ");
    }
    arg->Accept(*this);
  }
  Emit(")");

  if (!node.member_inits.empty()) {
    Indent();
    Indent();
    Emit("\n: ");
    for (const auto& item : node.member_inits) {
      if (&item != &node.member_inits.front()) {
        Emit(", ");
      }
      Emit(item.name(), "(");
      item.expr->Accept(*this);
      Emit(")");
    }
    DeIndent();
    DeIndent();
  }

  if (!node.body) {
    Emit(";\n");
    return;
  }

  Emit(" ");
  node.body->Accept(*this);
  Emit("\n");
}

void CodeGen::Visit(const Class& node) {
  Emit("\n");
  switch (node.type) {
    case ClassType::kClass:
      Emit("class");
      break;
    case ClassType::kStruct:
      Emit("struct");
      break;
    case ClassType::kUnion:
      Emit("union");
      break;
  }
  Emit(" ", node.name, " {\n");

  for (const auto& section : node.sections) {
    bool show_header = [&] {
      if (&section != &node.sections[0]) {
        return true;
      }

      if (node.type == ClassType::kClass) {
        return section.access == ClassAccessType::kPublic;
      }

      return section.access == ClassAccessType::kPrivate;
    }();

    if (show_header) {
      Emit(" ");
      switch (section.access) {
        case ClassAccessType::kPublic:
          Emit("public");
          break;
        case ClassAccessType::kPrivate:
          Emit("private");
          break;
      }
      Emit(":\n");
      Indent();
    } else {
      Indent();
    }

    for (const auto& member : section.members) {
      std::visit([&](const auto& val) { val->Accept(*this); }, member);
      if (std::holds_alternative<Rc<Decl>>(member)) {
        Emit(";");
      }

      Emit("\n");
    }

    DeIndent();
  }

  Emit("};");
}

void CodeGen::Visit(const VariableExpr& node) { EmitIdentifier(node.id); }

void CodeGen::Visit(const IntExpr& node) { Emit(node.token.text); }

void CodeGen::Visit(const FloatExpr& node) { Emit(node.token.text); }

void CodeGen::Visit(const StringExpr& node) {
  Emit("\"", node.token.text, "\"");
}

void CodeGen::Visit(const BinaryExpr& node) {
#define CASE(type, val)   \
  case BinExprType::type: \
    Emit(val);            \
    break

  node.left->Accept(*this);

  if (node.bin_expr_type != BinExprType::kSubscript) {
    Emit(" ");
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
      Emit("[");
      node.right->Accept(*this);
      Emit("]");
      return;
  }

  Emit(" ");
  node.right->Accept(*this);
#undef CASE
}

void CodeGen::Visit(const UnaryExpr& node) {
#define CASE(type, val)     \
  case UnaryExprType::type: \
    Emit(val);              \
    break

  switch (node.unary_expr_type) {
    CASE(kUnaryMinus, "-");
    CASE(kLogicNot, "!");
    CASE(kBitNot, "~");
    CASE(kDeref, "*");
    CASE(kAddr, "&");

    case UnaryExprType::kParen: {
      Emit("(");
      node.expr->Accept(*this);
      Emit(")");
      break;
    }
  }

  node.expr->Accept(*this);
#undef CASE
}

void CodeGen::Visit(const CallExpr& node) {
  node.func->Accept(*this);
  Emit("(");
  for (const auto& arg : node.args) {
    arg->Accept(*this);
    if (&arg != &node.args.back()) {
      Emit(", ");
    }
  }
  Emit(")");
}

void CodeGen::Visit(const MemberAccessExpr& node) {
  node.expr->Accept(*this);
  Emit(".", node.member_name);
}

void CodeGen::Visit(const InitListExpr& node) {
  Emit("{");
  for (const auto& expr : node.exprs) {
    expr->Accept(*this);
  }
  Emit("}");
}

void CodeGen::Visit(const Decl& node) {
  if (node.decl_flags & kDeclFlagsStatic) {
    Emit("static ");
  }

  const Type* type = node.type.get();

  bool is_const = !(node.decl_flags & kDeclFlagsMut);
  bool late_const =
      type != nullptr && type->GetTypeType() == TypeType::kPointer;

  if (is_const && !late_const) {
    Emit("const ");
  }

  if (node.type) {
    node.type->Accept(*this);
  } else {
    Emit("auto");
  }
  Emit(" ");
  if (is_const && late_const) {
    Emit("const ");
  }

  Emit(node.name);
  if (node.expr) {
    Emit(" = ");
    node.expr->Accept(*this);
  }
}

void CodeGen::Visit(const CompoundStmt& node) {
  Emit("{\n");
  Indent();

  for (const auto& stmt : node.stmts) {
    stmt->Accept(*this);
    Emit("\n");
  }

  DeIndent();
  Emit("}");
}

void CodeGen::Visit(const UnaryStmt& node) {
  std::visit([&](const auto& val) { val->Accept(*this); }, node.val);
  Emit(";");
}

void CodeGen::Visit(const IfStmt& node) {
  Emit("if (");
  node.test->Accept(*this);
  Emit(") ");
  node.true_stmt->Accept(*this);

  if (node.false_stmt) {
    Emit(" else ");
    node.false_stmt->Accept(*this);
  }
  Emit("\n");
}

void CodeGen::Visit(const WhileStmt& node) {
  Emit("while (");
  node.test->Accept(*this);
  Emit(")");
  node.body->Accept(*this);
}

void CodeGen::Visit(const ForStmt& node) {
  Emit("for (");
  node.decl->Accept(*this);
  Emit(" : ");
  node.expr->Accept(*this);
  Emit(")");
  node.body->Accept(*this);
}

void CodeGen::Visit(const SwitchStmt& node) {
  Emit("switch (");
  node.test->Accept(*this);
  Emit(") {\n");
  Indent();

  for (const auto& item : node.cases) {
    Emit("case ");
    item.test->Accept(*this);
    Emit(":\n");
    Indent();
    item.stmt->Accept(*this);
    DeIndent();
  }

  if (node.default_expr) {
    Emit("default:\n");
    Indent();
    node.default_expr->Accept(*this);
    DeIndent();
  }

  DeIndent();
  Emit("}\n");
}

void CodeGen::Visit(const ReturnStmt& node) {
  Emit("return ");
  node.expr->Accept(*this);
  Emit(";");
}

void CodeGen::Visit(const IncludeGlobalDecl& node) {
  Emit("#include ");
  if (node.type == IncludeGlobalDeclType::kBracket) {
    Emit("<");
  } else {
    Emit("\"");
  }
  Emit(node.path);
  if (node.type == IncludeGlobalDeclType::kBracket) {
    Emit(">");
  } else {
    Emit("\"");
  }
}

void CodeGen::Visit(const UnaryGlobalDecl& node) {
  std::visit([&](const auto& val) { val->Accept(*this); }, node.val);
}

void CodeGen::Visit(const FuncDecl& node) {
  node.ret_type->Accept(*this);
  Emit(" ", node.name, "(");

  for (const auto& arg : node.args) {
    arg->Accept(*this);
    if (&arg != &node.args.back()) {
      Emit(", ");
    }
  }

  Emit(")");
  if (node.body) {
    Emit(" ");
    node.body->Accept(*this);
  } else {
    Emit(";");
  }
  Emit("\n");
}

void CodeGen::EmitOne(std::string_view text) {
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

void CodeGen::EmitIdentifier(const Identifier& id) {
  if (id.fully_qualified) {
    Emit("::");
  }
  for (const auto& item : id.namespaces) {
    Emit(item, "::");
  }
  Emit(id.name);
}
