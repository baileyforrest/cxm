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

  virtual void Visit(const BaseType& node) { Append(node.token.text); }

  virtual void Visit(const TemplateType& node) {
    Append(node.token.text);
    Append("<");
    for (const auto& arg : node.args) {
      arg->Accept(*this);
      Append(", ");
    }
    Append(">");
  }

  virtual void Visit(const PointerType& node) {
    Append("*");
    node.sub_type->Accept(*this);
  }

  virtual void Visit(const ReferenceType& node) {
    Append("&");
    node.sub_type->Accept(*this);
  }

  virtual void Visit(const VariableExpr& node) {
    if (node.fully_qualified) {
      Append("::");
    }
    for (const auto& item : node.namespaces) {
      Append(item);
      Append("::");
    }
    Append(node.name);
  }

  virtual void Visit(const IntExpr& node) {
    Append("INT(");
    Append(node.token.text);
    Append(")");
  }

  virtual void Visit(const FloatExpr& node) {
    Append("FLOAT(");
    Append(node.token.text);
    Append(")");
  }

  virtual void Visit(const StringExpr& node) {
    Append("STRING(\"");
    Append(node.token.text);
    Append("\")");
  }

  virtual void Visit(const BinaryExpr& node) {
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

  virtual void Visit(const UnaryExpr& node) {
    Append("UNARY(");
    Append(UnaryExprTypeToString(node.unary_expr_type));
    Append(",");

    Indent();
    Append("\n");

    node.expr->Accept(*this);

    DeIndent();
    Append("\n)");
  }

  virtual void Visit(const CallExpr& node) {
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

  virtual void Visit(const MemberAccessExpr& node) {
    Append("MEMBER_ACCESS(");
    Indent();
    Append("\n");

    node.expr->Accept(*this);
    Append(",\n");

    Append(node.member_name);

    DeIndent();
    Append("\n)");
  }

  virtual void Visit(const InitListExpr& node) {
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

  virtual void Visit(const Decl& node) {
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

  virtual void Visit(const CompoundStmt& node) {
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

  virtual void Visit(const DeclStmt& node) { node.decl->Accept(*this); }

  virtual void Visit(const ExprStmt& node) { node.expr->Accept(*this); }

  virtual void Visit(const IfStmt& node) {
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

  virtual void Visit(const WhileStmt& node) {
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

  virtual void Visit(const ForStmt& node) {
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

  virtual void Visit(const SwitchStmt& node) {
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
      item.expr->Accept(*this);

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

  virtual void Visit(const ReturnStmt& node) {
    Append("RETURN(");
    node.expr->Accept(*this);
    Append(")");
  }
  virtual void Visit(const IncludeGlobalDecl& node) {
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

  virtual void Visit(const DeclGlobalDecl& node) { node.decl->Accept(*this); }

  virtual void Visit(const FuncDecl& node) {
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
