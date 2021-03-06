#pragma once

#include "cxm/parse/ast.h"
#include "cxm/parse/emit-ast-visitor.h"

class CodeGen : public EmitAstVisitor {
 public:
  explicit CodeGen(std::ostream* ostream) : EmitAstVisitor(ostream) {}

  void Run(const CompilationUnit& cu);

  void Visit(const BaseType& node) override;
  void Visit(const PointerType& node) override;
  void Visit(const ClassCtor& node) override;
  void Visit(const ClassDtor& node) override;
  void Visit(const Class& node) override;
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
  void Visit(const TypeAlias& node) override;
  void Visit(const CompoundStmt& node) override;
  void Visit(const UnaryStmt& node) override;
  void Visit(const IfStmt& node) override;
  void Visit(const WhileStmt& node) override;
  void Visit(const ForStmt& node) override;
  void Visit(const SwitchStmt& node) override;
  void Visit(const ReturnStmt& node) override;
  void Visit(const IncludeGlobalDecl& node) override;
  void Visit(const UnaryGlobalDecl& node) override;
  void Visit(const FuncDecl& node) override;

 private:
  const Decl* cur_decl_ = nullptr;
};
