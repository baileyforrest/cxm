#pragma once

#include "cxm/parse/ast.h"

class CodeGen : public AstVisitor {
 public:
  explicit CodeGen(std::ostream* ostream) : ostream_(*ostream) {}

  void Run(const CompilationUnit& cu);

  void Visit(const BaseType& node) override;
  void Visit(const TemplateType& node) override;
  void Visit(const PointerType& node) override;
  void Visit(const ReferenceType& node) override;
  void Visit(const VariableExpr& node) override;
  void Visit(const IntExpr& node) override;
  void Visit(const FloatExpr& node) override;
  void Visit(const StringExpr& node) override;
  void Visit(const BinaryExpr& node) override;
  void Visit(const UnaryExpr& node) override;
  void Visit(const CallExpr& node) override;
  void Visit(const MemberAccessExpr& node) override;
  void Visit(const InitListExpr& node) override;
  void Visit(const Decl& node) override;
  void Visit(const CompoundStmt& node) override;
  void Visit(const DeclStmt& node) override;
  void Visit(const ExprStmt& node) override;
  void Visit(const IfStmt& node) override;
  void Visit(const WhileStmt& node) override;
  void Visit(const ForStmt& node) override;
  void Visit(const SwitchStmt& node) override;
  void Visit(const ReturnStmt& node) override;
  void Visit(const IncludeGlobalDecl& node) override;
  void Visit(const UnaryGlobalDecl& node) override;
  void Visit(const FuncDecl& node) override;

 private:
  void Indent() { indent_ += 1; }
  void DeIndent() { indent_ -= 1; }

  void Append(std::string_view text);

  std::ostream& ostream_;
  bool indent_next_ = false;
  int indent_ = 0;

  const Decl* cur_decl_ = nullptr;
};
