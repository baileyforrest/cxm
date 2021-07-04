#include "cxm/parse/print-ast.h"

#include "cxm/parse/emit-ast-visitor.h"

namespace {

class AstStringPrinter : public EmitAstVisitor {
 public:
  explicit AstStringPrinter(std::ostream* ostream) : EmitAstVisitor(ostream) {}

  void Print(const CompilationUnit& cu) {
    for (const auto& decl : cu.global_decls) {
      Emit(decl, "\n");
    }
  }

 private:
  void Visit(const BaseType& node) override {
    if (node.flags & kTypeFlagsConst) {
      Emit("const ");
    }
    if (node.flags & kTypeFlagsVolatile) {
      Emit("volatile ");
    }

    Emit(node.id);
    if (!node.template_args.empty()) {
      Emit("<");
      for (const auto& arg : node.template_args) {
        Emit(arg, ", ");
      }
      Emit(">");
    }
  }

  void Visit(const PointerType& node) override {
    if (node.flags & kTypeFlagsConst) {
      Emit("const ");
    }
    if (node.flags & kTypeFlagsVolatile) {
      Emit("volatile ");
    }

    if (node.GetTypeType() == TypeType::kPointer) {
      Emit("*");
    } else {
      Emit("&");
    }

    Emit(node.sub_type);
  }

  void Visit(const ClassCtor& node) override {
    Emit("CTOR(", node.name, ", {\n");
    Indent();
    for (const auto& arg : node.args) {
      Emit(arg, ",\n");
    }
    DeIndent();
    Emit("}, {\n");

    Indent();
    for (const auto& init : node.member_inits) {
      Emit("{", init.name(), ", ", init.expr, "},\n");
    }
    DeIndent();
    Emit("}, ");
    if (node.body) {
      Emit(node.body);
    }
    Emit(")");
  }

  void Visit(const ClassDtor& node) override {
    Emit("DTOR(", node.name, ", ");
    if (node.body) {
      Emit(node.body);
    }
    Emit(")");
  }

  void Visit(const Class& node) override {
    switch (node.type) {
      case ClassType::kClass:
        Emit("CLASS");
        break;
      case ClassType::kStruct:
        Emit("STRUCT");
        break;
      case ClassType::kUnion:
        Emit("UNION");
        break;
    }
    Emit("(", node.name, ",");
    Indent();
    for (const auto& section : node.sections) {
      Emit("\n");
      switch (section.access) {
        case ClassAccessType::kPublic:
          Emit("PUBLIC");
          break;
        case ClassAccessType::kPrivate:
          Emit("PRIVATE");
          break;
      }
      Emit("{\n");
      Indent();

      for (const auto& member : section.members) {
        std::visit([&](const auto& val) { Emit(val, ",\n"); }, member);
      }

      DeIndent();
      Emit("},");
    }
    DeIndent();
    Emit("\n)");
  }

  void Visit(const VariableExpr& node) override { Emit(node.id); }

  void Visit(const IntExpr& node) override {
    Emit("INT(", node.token.text, ")");
  }

  void Visit(const FloatExpr& node) override {
    Emit("FLOAT(", node.token.text, ")");
  }

  void Visit(const StringExpr& node) override {
    if (node.GetExprType() == ExprType::kString) {
      Emit("STRING(\"", node.token.text, "\")");
    } else {
      Emit("CHAR('", node.token.text, "')");
    }
  }

  void Visit(const BinaryExpr& node) override {
    Emit("BINARY(", BinExprTypeToString(node.bin_expr_type), ",", "\n");
    Indent();
    Emit(node.left, ",\n", node.right);
    DeIndent();
    Emit(",\n)");
  }

  void Visit(const UnaryExpr& node) override {
    Emit("UNARY(", UnaryExprTypeToString(node.unary_expr_type), ",\n");
    Indent();
    Emit(node.expr);
    DeIndent();
    Emit("\n)");
  }

  void Visit(const CallExpr& node) override {
    Emit("CALL(\n");
    Indent();
    Emit(node.func);

    for (const auto& expr : node.args) {
      Emit(",\n", expr);
    }

    DeIndent();
    Emit("\n)");
  }

  void Visit(const MemberAccessExpr& node) override {
    Emit("MEMBER_ACCESS(\n");
    Indent();
    Emit(node.expr, ",\n", node.member_name);
    DeIndent();
    Emit("\n)");
  }

  void Visit(const InitListExpr& node) override {
    Emit("INIT_LIST(\n");
    Indent();
    for (const auto& expr : node.exprs) {
      Emit(expr, ",\n");
    }
    DeIndent();
    Emit("\n)");
  }

  void Visit(const Decl& node) override {
    Emit("DECL(", DeclFlagsToString(node.flags), ", ", node.name, ", ");

    if (node.type) {
      Emit(node.type);
    }
    Emit(", ");

    if (node.expr) {
      Emit(node.expr);
    }

    Emit(")");
  }

  void Visit(const TypeAlias& node) override {
    Emit("USING(", node.name, ", ", node.type, ")");
  }

  void Visit(const CompoundStmt& node) override {
    Emit("COMPOUND(\n");
    Indent();
    for (const auto& stmt : node.stmts) {
      Emit(stmt, ",\n");
    }
    DeIndent();
    Emit(")");
  }

  void Visit(const UnaryStmt& node) override {
    std::visit([&](const auto& val) { Emit(val); }, node.val);
  }

  void Visit(const IfStmt& node) override {
    Emit("IF(\n");
    Indent();

    Emit(node.test, ",\n");
    Emit(node.true_stmt, ",\n");

    if (node.false_stmt) {
      Emit(node.false_stmt, ",\n");
    }

    DeIndent();
    Emit(")");
  }

  void Visit(const WhileStmt& node) override {
    Emit("WHILE(\n");
    Indent();
    Emit(node.test, ",\n");
    Emit(node.body, ",\n");
    DeIndent();
    Emit("\n)");
  }

  void Visit(const ForStmt& node) override {
    Emit("FOR(\n");
    Indent();
    Emit(node.decl, ",\n");
    Emit(node.expr, ",\n");
    DeIndent();
    Emit(")");
  }

  void Visit(const SwitchStmt& node) override {
    Emit("SWITCH(\n");
    Indent();

    Emit(node.test, ",\n");
    Emit("cases: {\n");
    Indent();
    for (auto& item : node.cases) {
      Emit("case: {\n");
      Indent();
      Emit(item.test, ",\n", item.stmt);
      DeIndent();
      Emit("}\n");
    }
    DeIndent();
    Emit("}\n");

    if (node.default_expr) {
      Emit(node.default_expr, ",\n");
    }

    DeIndent();
    Emit("\n)");
  }

  void Visit(const ReturnStmt& node) override {
    Emit("RETURN(", node.expr, ")");
  }
  void Visit(const IncludeGlobalDecl& node) override {
    Emit("INCLUDE ");
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

  void Visit(const UnaryGlobalDecl& node) override {
    std::visit([&](const auto& val) { Emit(val); }, node.val);
  }

  void Visit(const FuncDecl& node) override {
    Emit("FUNC(", node.name, ", {\n");
    Indent();

    for (const auto& arg : node.args) {
      Emit(arg, ",\n");
    }

    Emit("},\n");
    if (node.flags & kFuncFlagsConst) {
      Emit("const ");
    }
    if (node.flags & kFuncFlagsStatic) {
      Emit("static ");
    }

    Emit(",\n");
    Emit(node.ret_type, ",\n");

    if (node.body) {
      Emit(node.body);
    }

    DeIndent();
    Emit("\n)");
  }
};

}  // namespace

void PrintAst(const CompilationUnit& cu, std::ostream& ostream) {
  AstStringPrinter builder(&ostream);
  builder.Print(cu);
}
