#pragma once

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "lex/token.h"

class AstStringBuilder {
 public:
  AstStringBuilder() = default;

  void Append(absl::string_view text);
  void Indent() { indent_ += 1; }
  void DeIndent() { indent_ -= 1; }

  const std::string& str() const { return str_; }

 private:
  int indent_ = 0;
  std::string str_;
};

class AstNode {
 public:
  explicit AstNode(const Token& start_token) : start_token_(start_token) {}
  virtual ~AstNode() = default;

  virtual void AppendString(AstStringBuilder* builder) const = 0;

  const Token& start_token() const { return start_token_; }
  const Location& location() { return start_token_.location; }

 private:
  const Token start_token_;
};

enum class TypeType {
  kBase,
  kTemplate,
};

class Type : public AstNode {
 public:
  explicit Type(const Token& start_token) : AstNode(start_token) {}

  virtual TypeType GetTypeType() const = 0;
};

class BaseType : public Type {
 public:
  explicit BaseType(const Token& start_token, absl::string_view name)
      : Type(start_token), name_(name) {}

  void AppendString(AstStringBuilder* builder) const override;
  TypeType GetTypeType() const override { return TypeType::kBase; }

 private:
  const std::string name_;
};

class TemplateType : public Type {
 public:
  explicit TemplateType(const Token& start_token, absl::string_view name,
                        std::vector<std::unique_ptr<Type>> args)
      : Type(start_token), name_(name), args_(std::move(args)) {}

  void AppendString(AstStringBuilder* builder) const override;
  TypeType GetTypeType() const override { return TypeType::kTemplate; }

  const std::vector<std::unique_ptr<Type>>& args() const { return args_; }

 private:
  const std::string name_;
  const std::vector<std::unique_ptr<Type>> args_;
};

enum class ExprType {
  kDecl,
  kFuncDecl,
  kVariable,
  kAssign,
  kInt,
  kFloat,
  kString,
  kBinary,
  kUnary,
  kCast,
  kCall,
  kMemberAccess,
  kInitList,
  kCompound,
  kIf,
  kWhile,
  kFor,
  kSwitch,
};

absl::string_view ExprTypeToString(ExprType type);

class Expr : public AstNode {
 public:
  explicit Expr(const Token& start_token) : AstNode(start_token) {}

  void AppendString(AstStringBuilder* builder) const override;
  virtual ExprType GetExprType() const = 0;
};

enum class GlobalDeclType {
  kInclude,
  kExpr,
};

class GlobalDecl : public AstNode {
 public:
  explicit GlobalDecl(const Token& start_token) : AstNode(start_token) {}

  virtual GlobalDeclType GetGlobalDeclType() const = 0;
};

enum class IncludeGlobalDeclType {
  kBracket,
  kQuote,
};

class IncludeGlobalDecl : public GlobalDecl {
 public:
  explicit IncludeGlobalDecl(const Token& start_token,
                             IncludeGlobalDeclType type, absl::string_view path)
      : GlobalDecl(start_token), type_(type), path_(path) {}

  void AppendString(AstStringBuilder* builder) const override;
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kInclude;
  }

 private:
  const IncludeGlobalDeclType type_;
  const std::string path_;
};

class ExprGlobalDecl : public GlobalDecl {
 public:
  explicit ExprGlobalDecl(const Token& start_token, std::unique_ptr<Expr> expr)
      : GlobalDecl(start_token), expr_(std::move(expr)) {}

  void AppendString(AstStringBuilder* builder) const override {
    expr_->AppendString(builder);
  }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kExpr;
  }

 private:
  const std::unique_ptr<Expr> expr_;
};

enum DeclFlags {
  kDeclFlagsNone = 0,
  kDeclFlagsMut = 1 << 0,
  kDeclFlagsStatic = 1 << 1,
};

std::string DeclFlagsToString(DeclFlags type);

class Decl : public Expr {
 public:
  explicit Decl(const Token& start_token, DeclFlags decl_flags,
                absl::string_view name, std::unique_ptr<Type> type,
                std::unique_ptr<Expr> expr)
      : Expr(start_token),
        decl_flags_(decl_flags),
        name_(name),
        type_(std::move(type)),
        expr_(std::move(expr)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kDecl; }

  DeclFlags decl_flags() const { return decl_flags_; }

 private:
  const DeclFlags decl_flags_;
  const std::string name_;
  const std::unique_ptr<Type> type_;
  const std::unique_ptr<Expr> expr_;
};

// Function declaration.
// TODO(bcf): Probably shouldn't be an expression and just should be a
// GlobalDecl..
class FuncDecl : public Expr {
 public:
  explicit FuncDecl(const Token& start_token, absl::string_view name,
                    std::vector<std::unique_ptr<Decl>> args,
                    std::unique_ptr<Type> ret_type, std::unique_ptr<Expr> body)
      : Expr(start_token),
        name_(name),
        args_(std::move(args)),
        ret_type_(std::move(ret_type)),
        body_(std::move(body)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kFuncDecl; }

 private:
  const std::string name_;
  const std::vector<std::unique_ptr<Decl>> args_;
  const std::unique_ptr<Type> ret_type_;
  const std::unique_ptr<Expr> body_;
};

class VariableExpr : public Expr {
 public:
  explicit VariableExpr(const Token& start_token, absl::string_view name)
      : Expr(start_token), name_(name) {}

  ExprType GetExprType() const override { return ExprType::kVariable; }

 private:
  const std::string name_;
};

class IntExpr : public Expr {
 public:
  explicit IntExpr(const Token& start_token) : Expr(start_token) {}

  ExprType GetExprType() const override { return ExprType::kInt; }
};

class FloatExpr : public Expr {
 public:
  explicit FloatExpr(const Token& start_token) : Expr(start_token) {}

  ExprType GetExprType() const override { return ExprType::kFloat; }
};

class StringExpr : public Expr {
 public:
  explicit StringExpr(const Token& start_token) : Expr(start_token) {}

  ExprType GetExprType() const override { return ExprType::kString; }
};

enum class BinaryExprType {
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
  kIndex,
};

absl::string_view BinaryExprTypeToString(BinaryExprType type);

class BinaryExpr : public Expr {
 public:
  explicit BinaryExpr(const Token& start_token, BinaryExprType bin_expr_type,
                      std::unique_ptr<Expr> left, std::unique_ptr<Expr> right)
      : Expr(start_token),
        bin_expr_type_(bin_expr_type),
        left_(std::move(left)),
        right_(std::move(right)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kBinary; }

  BinaryExprType bin_expr_type() const { return bin_expr_type_; }

 private:
  const BinaryExprType bin_expr_type_;

  const std::unique_ptr<Expr> left_;
  const std::unique_ptr<Expr> right_;
};

enum class UnaryExprType {
  kUnaryMinus,
  kDeref,
  kAddr,
  kLogicNot,
  kParen,
  kReturn,
};

absl::string_view UnaryExprTypeToString(UnaryExprType type);

class UnaryExpr : public Expr {
 public:
  explicit UnaryExpr(const Token& start_token, UnaryExprType unary_expr_type,
                     std::unique_ptr<Expr> expr)
      : Expr(start_token),
        unary_expr_type_(unary_expr_type),
        expr_(std::move(expr)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kUnary; }

  UnaryExprType unary_expr_type() const { return unary_expr_type_; }

 private:
  const UnaryExprType unary_expr_type_;
  const std::unique_ptr<Expr> expr_;
};

class CallExpr : public Expr {
 public:
  explicit CallExpr(const Token& start_token, std::unique_ptr<Expr> func,
                    std::vector<std::unique_ptr<Expr>> args)
      : Expr(start_token), func_(std::move(func)), args_(std::move(args)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kCall; }

 private:
  const std::unique_ptr<Expr> func_;
  const std::vector<std::unique_ptr<Expr>> args_;
};

class MemberAccessExpr : public Expr {
 public:
  explicit MemberAccessExpr(const Token& start_token,
                            std::unique_ptr<Expr> expr,
                            absl::string_view member_name)
      : Expr(start_token), expr_(std::move(expr)), member_name_(member_name) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kMemberAccess; }

 private:
  const std::unique_ptr<Expr> expr_;
  const std::string member_name_;
};

class InitListExpr : public Expr {
 public:
  explicit InitListExpr(const Token& start_token,
                        std::vector<std::unique_ptr<Expr>> exprs)
      : Expr(start_token), exprs_(std::move(exprs)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kInitList; }

 private:
  const std::vector<std::unique_ptr<Expr>> exprs_;
};

class CompoundExpr : public Expr {
 public:
  explicit CompoundExpr(const Token& start_token,
                        std::vector<std::unique_ptr<Expr>> exprs)
      : Expr(start_token), exprs_(std::move(exprs)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kInitList; }

 private:
  const std::vector<std::unique_ptr<Expr>> exprs_;
};

class IfExpr : public Expr {
 public:
  explicit IfExpr(const Token& start_token, std::unique_ptr<Expr> test,
                  std::unique_ptr<Expr> true_val,
                  std::unique_ptr<Expr> false_val)
      : Expr(start_token),
        test_(std::move(test)),
        true_(std::move(true_val)),
        false_(std::move(false_val)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kIf; }

 private:
  const std::unique_ptr<Expr> test_;
  const std::unique_ptr<Expr> true_;
  const std::unique_ptr<Expr> false_;
};

class WhileExpr : public Expr {
 public:
  explicit WhileExpr(const Token& start_token, std::unique_ptr<Expr> test,
                     std::unique_ptr<Expr> body)
      : Expr(start_token), test_(std::move(test)), body_(std::move(body)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kWhile; }

 private:
  const std::unique_ptr<Expr> test_;
  const std::unique_ptr<Expr> body_;
};

class ForExpr : public Expr {
 public:
  explicit ForExpr(const Token& start_token, std::unique_ptr<Decl> decl,
                   std::unique_ptr<Expr> expr)
      : Expr(start_token), decl_(std::move(decl)), expr_(std::move(expr)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kFor; }

 private:
  const std::unique_ptr<Decl> decl_;
  const std::unique_ptr<Expr> expr_;
};

class SwitchExpr : public Expr {
 public:
  struct Case {
    std::unique_ptr<Expr> test;
    std::unique_ptr<Expr> expr;
  };

  explicit SwitchExpr(const Token& start_token, std::unique_ptr<Expr> test,
                      std::vector<Case> cases,
                      std::unique_ptr<Expr> default_val)
      : Expr(start_token),
        test_(std::move(test)),
        cases_(std::move(cases)),
        default_(std::move(default_val)) {}

  void AppendString(AstStringBuilder* builder) const override;
  ExprType GetExprType() const override { return ExprType::kSwitch; }

 private:
  const std::unique_ptr<Expr> test_;
  const std::vector<Case> cases_;
  const std::unique_ptr<Expr> default_;
};
