#include "cxm/parse/print_ast.h"

namespace {

class AstStringPrinter : public AstVisitor {
 public:
  explicit AstStringPrinter(std::ostream& ostream) : ostream_(ostream) {}

  void Print(const CompilationUnit& cu) {
    for (const auto& decl : cu.global_decls) {
      decl->Accept(*this);
      Append("\n");
    }
  }

 private:
  void Indent() { indent_ += 1; }
  void DeIndent() { indent_ -= 1; }

  void Append(std::string_view text) {
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

  void Visit(const BaseType& node) override { Append(node.token.text); }

  void Visit(const TemplateType& node) override {
    Append(node.token.text);
    Append("<");
    for (const auto& arg : node.args) {
      arg->Accept(*this);
      Append(", ");
    }
    Append(">");
  }

  void Visit(const PointerType& node) override {
    Append("*");
    node.sub_type->Accept(*this);
  }

  void Visit(const ReferenceType& node) override {
    Append("&");
    node.sub_type->Accept(*this);
  }

  void Visit(const VariableExpr& node) override {
    if (node.fully_qualified) {
      Append("::");
    }
    for (const auto& item : node.namespaces) {
      Append(item);
      Append("::");
    }
    Append(node.name);
  }

  void Visit(const IntExpr& node) override {
    Append("INT(");
    Append(node.token.text);
    Append(")");
  }

  void Visit(const FloatExpr& node) override {
    Append("FLOAT(");
    Append(node.token.text);
    Append(")");
  }

  void Visit(const StringExpr& node) override {
    Append("STRING(\"");
    Append(node.token.text);
    Append("\")");
  }

  void Visit(const BinaryExpr& node) override {
    Append("BINARY(");
    Append(BinExprTypeToString(node.bin_expr_type));
    Append(",");

    Indent();
    Append("\n");

    node.left->Accept(*this);
    Append(",\n");
    node.right->Accept(*this);

    DeIndent();
    Append(",\n)");
  }

  void Visit(const UnaryExpr& node) override {
    Append("UNARY(");
    Append(UnaryExprTypeToString(node.unary_expr_type));
    Append(",");

    Indent();
    Append("\n");

    node.expr->Accept(*this);

    DeIndent();
    Append("\n)");
  }

  void Visit(const CallExpr& node) override {
    Append("CALL(");
    Indent();
    Append("\n");
    node.func->Accept(*this);

    for (const auto& expr : node.args) {
      Append(",\n");
      expr->Accept(*this);
    }

    DeIndent();
    Append("\n)");
  }

  void Visit(const MemberAccessExpr& node) override {
    Append("MEMBER_ACCESS(");
    Indent();
    Append("\n");

    node.expr->Accept(*this);
    Append(",\n");

    Append(node.member_name);

    DeIndent();
    Append("\n)");
  }

  void Visit(const InitListExpr& node) override {
    Append("INIT_LIST(");
    Indent();
    Append("\n");

    for (const auto& expr : node.exprs) {
      expr->Accept(*this);
      Append(",\n");
    }

    DeIndent();
    Append("\n)");
  }

  void Visit(const Decl& node) override {
    Append("DECL(");
    Append(DeclFlagsToString(node.decl_flags));
    Append(", ");
    Append(node.name);
    Append(", ");

    if (node.type) {
      node.type->Accept(*this);
    }

    if (node.expr) {
      node.expr->Accept(*this);
    }

    Append(")");
  }

  void Visit(const CompoundStmt& node) override {
    Append("COMPOUND(");
    Indent();
    Append("\n");

    for (const auto& stmt : node.stmts) {
      stmt->Accept(*this);
      Append(",\n");
    }

    DeIndent();
    Append(")");
  }

  void Visit(const UnaryStmt& node) override {
    std::visit([&](const auto& val) { val->Accept(*this); }, node.val);
  }

  void Visit(const IfStmt& node) override {
    Append("IF(");
    Indent();
    Append("\n");

    node.test->Accept(*this);
    Append(",\n");

    node.true_stmt->Accept(*this);
    Append(",\n");

    if (node.false_stmt) {
      node.false_stmt->Accept(*this);
      Append(",\n");
    }

    DeIndent();
    Append(")");
  }

  void Visit(const WhileStmt& node) override {
    Append("WHILE(");
    Indent();
    Append("\n");

    node.test->Accept(*this);
    Append(",\n");

    node.body->Accept(*this);
    Append(",\n");

    DeIndent();
    Append("\n)");
  }

  void Visit(const ForStmt& node) override {
    Append("FOR(");
    Indent();
    Append("\n");

    node.decl->Accept(*this);
    Append(",\n");

    node.expr->Accept(*this);
    Append(",\n");

    DeIndent();
    Append("\n)");
  }

  void Visit(const SwitchStmt& node) override {
    Append("SWITCH(");
    Indent();
    Append("\n");

    node.test->Accept(*this);
    Append(",\n");

    Append("cases: {");
    Indent();
    Append("\n");
    for (auto& item : node.cases) {
      Append("case: {");
      Indent();
      Append("\n");

      item.test->Accept(*this);
      Append(",\n");
      item.stmt->Accept(*this);

      DeIndent();
      Append("}\n");
    }
    DeIndent();
    Append("}\n");

    node.default_expr->Accept(*this);
    Append(",\n");

    DeIndent();
    Append("\n)");
  }

  void Visit(const ReturnStmt& node) override {
    Append("RETURN(");
    node.expr->Accept(*this);
    Append(")");
  }
  void Visit(const IncludeGlobalDecl& node) override {
    Append("INCLUDE ");
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

  void Visit(const UnaryGlobalDecl& node) override {
    std::visit([&](const auto& val) { val->Accept(*this); }, node.val);
  }

  void Visit(const FuncDecl& node) override {
    Append("FUNC(");
    Append(node.name);
    Append(", {");
    Indent();
    Append("\n");

    for (const auto& arg : node.args) {
      arg->Accept(*this);
      Append(",\n");
    }

    Append("},\n");

    node.ret_type->Accept(*this);
    Append(",\n");

    if (node.body) {
      node.body->Accept(*this);
    }

    DeIndent();
    Append("\n)");
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
