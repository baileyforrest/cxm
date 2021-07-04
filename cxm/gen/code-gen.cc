#include "cxm/gen/code-gen.h"

#include <iostream>
#include <sstream>

void CodeGen::Run(const CompilationUnit& cu) {
  for (const auto& decl : cu.global_decls) {
    Emit(decl, "\n");
  }
}

void CodeGen::Visit(const BaseType& node) {
  if (node.flags & kTypeFlagsConst) {
    Emit("const ");
  }
  if (node.flags & kTypeFlagsVolatile) {
    Emit("volatile ");
  }

  Emit(node.id);
  if (!node.template_args.empty()) {
    Emit("<");
    for (const auto& item : node.template_args) {
      Emit(item);
    }
    Emit(">");
  }
}

void CodeGen::Visit(const PointerType& node) {
  if (node.GetTypeType() == TypeType::kReference) {
    if (node.flags & kTypeFlagsConst) {
      Emit("const ");
    }
    if (node.flags & kTypeFlagsVolatile) {
      Emit("volatile ");
    }
    Emit(node.sub_type, "&");
    return;
  }

  Emit(node.sub_type);
  Emit("*");
  if (node.flags & kTypeFlagsConst) {
    Emit(" const");
  }
  if (node.flags & kTypeFlagsVolatile) {
    Emit(" volatile");
  }
}

void CodeGen::Visit(const ClassCtor& node) {
  Emit("explicit ", node.name, "(");
  if (!node.args.empty()) {
    Indent(2);
    Emit("\n");
    for (const auto& arg : node.args) {
      Emit(arg);
      if (&arg != &node.args.back()) {
        Emit(",\n");
      }
    }
    DeIndent(2);
  }
  Emit(")");

  if (!node.member_inits.empty()) {
    Indent(2);
    Emit("\n: ");
    for (const auto& item : node.member_inits) {
      if (&item != &node.member_inits.front()) {
        Emit(", ");
      }
      Emit(item.name(), "(", item.expr, ")");
    }
    DeIndent(2);
  }

  if (!node.body) {
    Emit(";\n");
    return;
  }

  Emit(" ", node.body, "\n");
}

void CodeGen::Visit(const ClassDtor& node) {
  Emit("~", node.name, "()");

  if (node.body) {
    Emit(node.body);
  } else {
    Emit(";");
  }
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
      std::visit([&](const auto& val) { Emit(val); }, member);
      if (std::holds_alternative<Rc<Decl>>(member)) {
        Emit(";");
      }

      Emit("\n");
    }

    DeIndent();
  }

  Emit("};");
}

void CodeGen::Visit(const VariableExpr& node) { Emit(node.id); }

void CodeGen::Visit(const IntExpr& node) { Emit(node.token.text); }

void CodeGen::Visit(const FloatExpr& node) { Emit(node.token.text); }

void CodeGen::Visit(const StringExpr& node) {
  std::string_view delim = node.GetExprType() == ExprType::kString ? "\"" : "'";

  Emit(delim, node.token.text, delim);
}

void CodeGen::Visit(const BinaryExpr& node) {
#define CASE(type, val)   \
  case BinExprType::type: \
    Emit(val);            \
    break

  Emit(node.left);

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
      Emit("[", node.right, "]");
      return;
  }

  Emit(" ", node.right);
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
      Emit("(", node.expr, ")");
      break;
    }
  }

  Emit(node.expr);
#undef CASE
}

void CodeGen::Visit(const CallExpr& node) {
  Emit(node.func, "(");

  std::ostringstream oss;
  PushStream(&oss);
  for (const auto& arg : node.args) {
    Emit(arg);
    if (&arg != &node.args.back()) {
      Emit(", ");
    }
  }
  PopStream();
  if (column() + oss.str().size() < kMaxCol) {
    Emit(oss.str(), ")");
    return;
  }

  Emit("\n");
  Indent(2);

  for (const auto& arg : node.args) {
    bool is_first = &arg == &node.args.front();
    bool is_last = &arg == &node.args.back();

    std::ostringstream oss;
    PushStream(&oss);
    Emit(arg);
    if (!is_last) {
      Emit(",");
    }
    PopStream();

    if (oss.str().size() + column() > kMaxCol) {
      Emit("\n", oss.str());
    } else {
      if (!is_first) {
        Emit(" ");
      }
      Emit(oss.str());
    }
  }

  DeIndent(2);
  Emit(")");
}

void CodeGen::Visit(const MemberAccessExpr& node) {
  Emit(node.expr);
  if (node.GetExprType() == ExprType::kMemberAccessDot) {
    Emit(".");
  } else {
    Emit("->");
  }
  Emit(node.member_name);
}

void CodeGen::Visit(const InitListExpr& node) {
  Emit("{");
  for (const auto& expr : node.exprs) {
    Emit(expr);
  }
  Emit("}");
}

void CodeGen::Visit(const Decl& node) {
  if (node.flags & kDeclFlagsStatic) {
    Emit("static ");
  }

  if (!(node.flags & kDeclFlagsMut) && !node.type) {
    Emit("const ");
  }

  if (node.type) {
    Emit(node.type);
  } else {
    Emit("auto");
  }
  Emit(" ");

  Emit(node.name);
  if (node.expr) {
    Emit(" = ", node.expr);
  }
}

void CodeGen::Visit(const TypeAlias& node) {
  Emit("using ", node.name, " = ", node.type, ";");
}

void CodeGen::Visit(const CompoundStmt& node) {
  if (node.stmts.empty()) {
    Emit("{}");
    return;
  }

  Emit("{\n");
  Indent();
  for (const auto& stmt : node.stmts) {
    Emit(stmt, "\n");
  }
  DeIndent();
  Emit("}");
}

void CodeGen::Visit(const UnaryStmt& node) {
  std::visit([&](const auto& val) { Emit(val, ";"); }, node.val);
}

void CodeGen::Visit(const IfStmt& node) {
  Emit("if (", node.test, ") ", node.true_stmt);
  if (node.false_stmt) {
    Emit(" else ", node.false_stmt);
  }
  Emit("\n");
}

void CodeGen::Visit(const WhileStmt& node) {
  Emit("while (", node.test, ")", node.body);
}

void CodeGen::Visit(const ForStmt& node) {
  Emit("for (", node.decl, " : ", node.expr, ") ", node.body, "\n");
}

void CodeGen::Visit(const SwitchStmt& node) {
  Emit("switch (", node.test, ") {\n");
  Indent();

  for (const auto& item : node.cases) {
    Emit("case ", item.test, ":\n");
    Indent();
    Emit(item.stmt);
    DeIndent();
  }

  if (node.default_expr) {
    Emit("default:\n");
    Indent();
    Emit(node.default_expr);
    DeIndent();
  }

  DeIndent();
  Emit("}\n");
}

void CodeGen::Visit(const ReturnStmt& node) { Emit("return ", node.expr, ";"); }

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
  std::visit([&](const auto& val) { Emit(val); }, node.val);
}

void CodeGen::Visit(const FuncDecl& node) {
  if (node.flags & kFuncFlagsStatic) {
    Emit("static ");
  }

  Emit(node.ret_type, " ", node.name, "(");

  for (const auto& arg : node.args) {
    Emit(arg);
    if (&arg != &node.args.back()) {
      Emit(", ");
    }
  }

  Emit(")");
  if (node.flags & kFuncFlagsConst) {
    Emit(" const");
  }

  if (node.body) {
    Emit(" ", node.body);
  } else {
    Emit(";");
  }
  Emit("\n");
}
