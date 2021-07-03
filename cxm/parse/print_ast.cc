#include "cxm/parse/print_ast.h"

namespace {

class AstStringPrinter : public AstVisitor {
 public:
  explicit AstStringPrinter(std::ostream& ostream) : ostream_(ostream) {}

  void Print(const CompilationUnit& cu) {
    for (const auto& decl : cu.global_decls) {
      decl->Accept(*this);
      Emit("\n");
    }
  }

 private:
  void Indent() { indent_ += 1; }
  void DeIndent() { indent_ -= 1; }

  void EmitOne(std::string_view text) {
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

  template <typename T>
  void Emit(T val) {
    EmitOne(val);
  }

  template <typename T, typename... Args>
  void Emit(T val, Args... args) {
    EmitOne(val);
    Emit(args...);
  }

  void EmitIdentifier(const Identifier& id) {
    if (id.fully_qualified) {
      Emit("::");
    }
    for (const auto& item : id.namespaces) {
      Emit(item, "::");
    }
    Emit(id.name);
  }

  void Visit(const BaseType& node) override {
    EmitIdentifier(node.id);
    if (!node.template_args.empty()) {
      Emit("<");
      for (const auto& arg : node.template_args) {
        arg->Accept(*this);
        Emit(", ");
      }
      Emit(">");
    }
  }

  void Visit(const PointerType& node) override {
    Emit("*");
    node.sub_type->Accept(*this);
  }

  void Visit(const ReferenceType& node) override {
    Emit("&");
    node.sub_type->Accept(*this);
  }

  void Visit(const ClassCtor& node) override {
    Emit("CTOR(", node.name, ", {\n");
    Indent();
    for (const auto& arg : node.args) {
      arg->Accept(*this);
      Emit(",\n");
    }
    DeIndent();
    Emit("}, {\n");

    Indent();
    for (const auto& init : node.member_inits) {
      Emit("{", init.name(), ", ");
      init.expr->Accept(*this);
      Emit("},\n");
    }
    DeIndent();
    Emit("}, ");
    node.body->Accept(*this);
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
        std::visit([&](const auto& val) { val->Accept(*this); }, member);
        Emit(",\n");
      }

      DeIndent();
      Emit("},");
    }
    DeIndent();
    Emit("\n)");
  }

  void Visit(const VariableExpr& node) override { EmitIdentifier(node.id); }

  void Visit(const IntExpr& node) override {
    Emit("INT(", node.token.text, ")");
  }

  void Visit(const FloatExpr& node) override {
    Emit("FLOAT(", node.token.text, ")");
  }

  void Visit(const StringExpr& node) override {
    Emit("STRING(\"", node.token.text, "\")");
  }

  void Visit(const BinaryExpr& node) override {
    Emit("BINARY(", BinExprTypeToString(node.bin_expr_type), ",", "\n");
    Indent();

    node.left->Accept(*this);
    Emit(",\n");
    node.right->Accept(*this);

    DeIndent();
    Emit(",\n)");
  }

  void Visit(const UnaryExpr& node) override {
    Emit("UNARY(", UnaryExprTypeToString(node.unary_expr_type), ",\n");
    Indent();

    node.expr->Accept(*this);

    DeIndent();
    Emit("\n)");
  }

  void Visit(const CallExpr& node) override {
    Emit("CALL(\n");
    Indent();
    node.func->Accept(*this);

    for (const auto& expr : node.args) {
      Emit(",\n");
      expr->Accept(*this);
    }

    DeIndent();
    Emit("\n)");
  }

  void Visit(const MemberAccessExpr& node) override {
    Emit("MEMBER_ACCESS(\n");
    Indent();

    node.expr->Accept(*this);
    Emit(",\n");

    Emit(node.member_name);

    DeIndent();
    Emit("\n)");
  }

  void Visit(const InitListExpr& node) override {
    Emit("INIT_LIST(\n");
    Indent();

    for (const auto& expr : node.exprs) {
      expr->Accept(*this);
      Emit(",\n");
    }

    DeIndent();
    Emit("\n)");
  }

  void Visit(const Decl& node) override {
    Emit("DECL(", DeclFlagsToString(node.decl_flags), ", ", node.name, ", ");

    if (node.type) {
      node.type->Accept(*this);
    }
    Emit(", ");

    if (node.expr) {
      node.expr->Accept(*this);
    }

    Emit(")");
  }

  void Visit(const CompoundStmt& node) override {
    Emit("COMPOUND(\n");
    Indent();

    for (const auto& stmt : node.stmts) {
      stmt->Accept(*this);
      Emit(",\n");
    }

    DeIndent();
    Emit(")");
  }

  void Visit(const UnaryStmt& node) override {
    std::visit([&](const auto& val) { val->Accept(*this); }, node.val);
  }

  void Visit(const IfStmt& node) override {
    Emit("IF(\n");
    Indent();

    node.test->Accept(*this);
    Emit(",\n");

    node.true_stmt->Accept(*this);
    Emit(",\n");

    if (node.false_stmt) {
      node.false_stmt->Accept(*this);
      Emit(",\n");
    }

    DeIndent();
    Emit(")");
  }

  void Visit(const WhileStmt& node) override {
    Emit("WHILE(\n");
    Indent();

    node.test->Accept(*this);
    Emit(",\n");

    node.body->Accept(*this);
    Emit(",\n");

    DeIndent();
    Emit("\n)");
  }

  void Visit(const ForStmt& node) override {
    Emit("FOR(\n");
    Indent();

    node.decl->Accept(*this);
    Emit(",\n");

    node.expr->Accept(*this);
    Emit(",\n");

    DeIndent();
    Emit("\n)");
  }

  void Visit(const SwitchStmt& node) override {
    Emit("SWITCH(\n");
    Indent();

    node.test->Accept(*this);
    Emit(",\n");

    Emit("cases: {\n");
    Indent();
    for (auto& item : node.cases) {
      Emit("case: {\n");
      Indent();

      item.test->Accept(*this);
      Emit(",\n");
      item.stmt->Accept(*this);

      DeIndent();
      Emit("}\n");
    }
    DeIndent();
    Emit("}\n");

    node.default_expr->Accept(*this);
    Emit(",\n");

    DeIndent();
    Emit("\n)");
  }

  void Visit(const ReturnStmt& node) override {
    Emit("RETURN(");
    node.expr->Accept(*this);
    Emit(")");
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
    std::visit([&](const auto& val) { val->Accept(*this); }, node.val);
  }

  void Visit(const FuncDecl& node) override {
    Emit("FUNC(", node.name, ", {\n");
    Indent();

    for (const auto& arg : node.args) {
      arg->Accept(*this);
      Emit(",\n");
    }

    Emit("},\n");

    node.ret_type->Accept(*this);
    Emit(",\n");

    if (node.body) {
      node.body->Accept(*this);
    }

    DeIndent();
    Emit("\n)");
  }

  std::ostream& ostream_;
  bool indent_next_ = false;
  int indent_ = 0;
};

}  // namespace

void PrintAst(const CompilationUnit& cu, std::ostream& ostream) {
  AstStringPrinter builder(ostream);
  builder.Print(cu);
}
