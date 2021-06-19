#pragma once

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "lex/token.h"
#include "util/rc.h"

class AstStringBuilder {
 public:
  AstStringBuilder() = default;

  void Append(std::string_view text);
  void Indent() { indent_ += 1; }
  void DeIndent() { indent_ -= 1; }

  const std::string& str() const { return str_; }

 private:
  int indent_ = 0;
  std::string str_;
};

class AstNode {
 public:
  explicit AstNode(const Token& token) : token_(token) {}
  virtual ~AstNode() = default;

  virtual void AppendString(AstStringBuilder* builder) const = 0;

  const Token& token() const { return token_; }
  const Location& location() { return token_.location; }

 private:
  const Token token_;
};

enum class TypeType {
  kBase,
  kTemplate,
  kPointer,
  kReference,
};

class Type : public AstNode {
 public:
  using AstNode::AstNode;

  virtual TypeType GetTypeType() const = 0;
};

class BaseType : public Type {
 public:
  explicit BaseType(const Token& token, std::string_view name)
      : Type(token), name_(name) {}

  void AppendString(AstStringBuilder* builder) const override;
  TypeType GetTypeType() const override { return TypeType::kBase; }

 private:
  const std::string name_;
};

class TemplateType : public Type {
 public:
  explicit TemplateType(const Token& token, std::string_view name,
                        std::vector<Rc<Type>> args)
      : Type(token), name_(name), args_(std::move(args)) {}

  void AppendString(AstStringBuilder* builder) const override;
  TypeType GetTypeType() const override { return TypeType::kTemplate; }

  const std::vector<Rc<Type>>& args() const { return args_; }

 private:
  const std::string name_;
  const std::vector<Rc<Type>> args_;
};

class PointerType : public Type {
 public:
  explicit PointerType(const Token& token, Rc<Type> sub_type)
      : Type(token), sub_type_(sub_type) {}

  void AppendString(AstStringBuilder* builder) const override;
  TypeType GetTypeType() const override { return TypeType::kPointer; }

 private:
  const Rc<Type> sub_type_;
};

class ReferenceType : public Type {
 public:
  explicit ReferenceType(const Token& token, Rc<Type> sub_type)
      : Type(token), sub_type_(sub_type) {}

  void AppendString(AstStringBuilder* builder) const override;
  TypeType GetTypeType() const override { return TypeType::kReference; }

 private:
  const Rc<Type> sub_type_;
};

enum class ExprType {
  kVariable,
  kInt,
  kFloat,
  kString,
  kBinary,
  kUnary,
  kCast,
  kCall,
  kMemberAccess,
  kInitList,
};

std::string_view ExprTypeToString(ExprType type);

class Expr : public AstNode {
 public:
  using AstNode::AstNode;

  void AppendString(AstStringBuilder* builder) const override;
  virtual ExprType GetExprType() const = 0;
};

class VariableExpr : public Expr {
 public:
  explicit VariableExpr(const Token& token, std::string_view name)
      : Expr(token), name_(name) {}

  ExprType GetExprType() const override { return ExprType::kVariable; }

 private:
  const std::string name_;
};

class IntExpr : public Expr {
 public:
  explicit IntExpr(const Token& token) : Expr(token) {}

  ExprType GetExprType() const override { return ExprType::kInt; }
};

class FloatExpr : public Expr {
 public:
  explicit FloatExpr(const Token& token) : Expr(token) {}

  ExprType GetExprType() const override { return ExprType::kFloat; }
};

class StringExpr : public Expr {
 public:
  explicit StringExpr(const Token& token) : Expr(token) {}

  ExprType GetExprType() const override { return ExprType::kString; }
};

enum class BinExprType {
  kPlus,
  kMinus,
  kTimes,
  kDiv,
  kMod,
  kLt,
  kLe,
  kGt,
  kGe,
  kEq,
  kNe,
  kBitAnd,
  kBitXor,
  kBitOr,
  kLShift,
  kRShift,
  kLogicAnd,
  kLogicOr,
};

std::string_view BinExprTypeToString(BinExprType type);

class BinaryExpr : public Expr {
 public:
  explicit BinaryExpr(const Token& token, BinExprType bin_expr_type,
                      Rc<Expr> left, Rc<Expr> right)
      : Expr(token),
        bin_expr_type_(bin_expr_type),
        left_(left),
        right_(right) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kBinary; }

  BinExprType bin_expr_type() const { return bin_expr_type_; }

 private:
  const BinExprType bin_expr_type_;

  const Rc<Expr> left_;
  const Rc<Expr> right_;
};

enum class UnaryExprType {
  kUnaryMinus,
  kDeref,
  kAddr,
  kLogicNot,
  kParen,
  kReturn,
};

std::string_view UnaryExprTypeToString(UnaryExprType type);

class UnaryExpr : public Expr {
 public:
  explicit UnaryExpr(const Token& token, UnaryExprType unary_expr_type,
                     Rc<Expr> expr)
      : Expr(token), unary_expr_type_(unary_expr_type), expr_(expr) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kUnary; }

  UnaryExprType unary_expr_type() const { return unary_expr_type_; }

 private:
  const UnaryExprType unary_expr_type_;
  const Rc<Expr> expr_;
};

class CallExpr : public Expr {
 public:
  explicit CallExpr(const Token& token, Rc<Expr> func,
                    std::vector<Rc<Expr>> args)
      : Expr(token), func_(func), args_(std::move(args)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kCall; }

 private:
  const Rc<Expr> func_;
  const std::vector<Rc<Expr>> args_;
};

class MemberAccessExpr : public Expr {
 public:
  explicit MemberAccessExpr(const Token& token, Rc<Expr> expr,
                            std::string_view member_name)
      : Expr(token), expr_(expr), member_name_(member_name) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kMemberAccess; }

 private:
  const Rc<Expr> expr_;
  const std::string member_name_;
};

class InitListExpr : public Expr {
 public:
  explicit InitListExpr(const Token& token, std::vector<Rc<Expr>> exprs)
      : Expr(token), exprs_(std::move(exprs)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kInitList; }

 private:
  const std::vector<Rc<Expr>> exprs_;
};

enum DeclFlags {
  kDeclFlagsNone = 0,
  kDeclFlagsMut = 1 << 0,
  kDeclFlagsStatic = 1 << 1,
};

std::string DeclFlagsToString(DeclFlags type);

class Decl : public AstNode {
 public:
  explicit Decl(const Token& token, DeclFlags decl_flags, std::string_view name,
                Rc<Type> type, Rc<Expr> expr)
      : AstNode(token),
        decl_flags_(decl_flags),
        name_(name),
        type_(type),
        expr_(expr) {}

  void AppendString(AstStringBuilder* builder) const override;

  DeclFlags decl_flags() const { return decl_flags_; }

 private:
  const DeclFlags decl_flags_;
  const std::string name_;
  const Rc<Type> type_;
  const Rc<Expr> expr_;
};

enum class StmtType {
  kCompound,
  kDecl,
  kExpr,
  kIf,
  kWhile,
  kFor,
  kSwitch,
};

std::string_view StmtTypeToString(ExprType type);

class Stmt : public AstNode {
 public:
  using AstNode::AstNode;

  virtual StmtType GetStmtType() const = 0;
};

class CompoundStmt : public Stmt {
 public:
  explicit CompoundStmt(const Token& token, std::vector<Rc<Stmt>> stmts)
      : Stmt(token), stmts_(std::move(stmts)) {}

  void AppendString(AstStringBuilder* builder) const override;
  StmtType GetStmtType() const override { return StmtType::kCompound; }

 private:
  const std::vector<Rc<Stmt>> stmts_;
};

class DeclStmt : public Stmt {
 public:
  explicit DeclStmt(Rc<Decl> decl) : Stmt(decl->token()), decl_(decl) {}

  void AppendString(AstStringBuilder* builder) const override {
    return decl_->AppendString(builder);
  }
  StmtType GetStmtType() const override { return StmtType::kCompound; }

 private:
  const Rc<Decl> decl_;
};

class IfStmt : public Stmt {
 public:
  explicit IfStmt(const Token& token, Rc<Expr> test, Rc<Stmt> true_val,
                  Rc<Stmt> false_val)
      : Stmt(token), test_(test), true_(true_val), false_(false_val) {}

  void AppendString(AstStringBuilder* builder) const override;
  StmtType GetStmtType() const override { return StmtType::kIf; }

 private:
  const Rc<Expr> test_;
  const Rc<Stmt> true_;
  const Rc<Stmt> false_;
};

class WhileStmt : public Stmt {
 public:
  explicit WhileStmt(const Token& token, Rc<Expr> test, Rc<Stmt> body)
      : Stmt(token), test_(test), body_(body) {}

  void AppendString(AstStringBuilder* builder) const override;
  StmtType GetStmtType() const override { return StmtType::kWhile; }

 private:
  const Rc<Expr> test_;
  const Rc<Stmt> body_;
};

class ForStmt : public Stmt {
 public:
  explicit ForStmt(const Token& token, Rc<Decl> decl, Rc<Expr> expr)
      : Stmt(token), decl_(decl), expr_(expr) {}

  void AppendString(AstStringBuilder* builder) const override;
  StmtType GetStmtType() const override { return StmtType::kFor; }

 private:
  const Rc<Decl> decl_;
  const Rc<Expr> expr_;
};

class SwitchStmt : public Stmt {
 public:
  struct Case {
    Rc<Expr> test;
    Rc<Expr> expr;
  };

  explicit SwitchStmt(const Token& token, Rc<Expr> test,
                      std::vector<Case> cases, Rc<Expr> default_val)
      : Stmt(token),
        test_(test),
        cases_(std::move(cases)),
        default_(default_val) {}

  void AppendString(AstStringBuilder* builder) const override;
  StmtType GetStmtType() const override { return StmtType::kSwitch; }

 private:
  const Rc<Expr> test_;
  const std::vector<Case> cases_;
  const Rc<Expr> default_;
};

enum class GlobalDeclType {
  kInclude,
  kDecl,
  kFunc,
};

class GlobalDecl : public AstNode {
 public:
  using AstNode::AstNode;

  virtual GlobalDeclType GetGlobalDeclType() const = 0;
};

enum class IncludeGlobalDeclType {
  kBracket,
  kQuote,
};

class IncludeGlobalDecl : public GlobalDecl {
 public:
  explicit IncludeGlobalDecl(const Token& token, IncludeGlobalDeclType type,
                             std::string_view path)
      : GlobalDecl(token), type_(type), path_(path) {}

  void AppendString(AstStringBuilder* builder) const override;
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kInclude;
  }

 private:
  const IncludeGlobalDeclType type_;
  const std::string path_;
};

class DeclGlobalDecl : public GlobalDecl {
 public:
  explicit DeclGlobalDecl(const Token& token, Rc<Decl> decl)
      : GlobalDecl(token), decl_(decl) {}

  void AppendString(AstStringBuilder* builder) const override {
    decl_->AppendString(builder);
  }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kDecl;
  }

 private:
  const Rc<Decl> decl_;
};

class FuncDecl : public GlobalDecl {
 public:
  explicit FuncDecl(const Token& token, std::string_view name,
                    std::vector<Rc<Decl>> args, Rc<Type> ret_type,
                    Rc<CompoundStmt> body)
      : GlobalDecl(token),
        name_(name),
        args_(std::move(args)),
        ret_type_(ret_type),
        body_(body) {}

  void AppendString(AstStringBuilder* builder) const override;
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kFunc;
  }

 private:
  const std::string name_;
  const std::vector<Rc<Decl>> args_;
  const Rc<Type> ret_type_;
  const Rc<CompoundStmt> body_;
};
