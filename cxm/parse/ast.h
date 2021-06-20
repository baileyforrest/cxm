#pragma once

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "cxm/lex/token.h"
#include "cxm/util/rc.h"

struct BaseType;
struct TemplateType;
struct PointerType;
struct ReferenceType;
struct VariableExpr;
struct IntExpr;
struct FloatExpr;
struct StringExpr;
struct BinaryExpr;
struct UnaryExpr;
struct CallExpr;
struct MemberAccessExpr;
struct InitListExpr;
struct Decl;
struct CompoundStmt;
struct DeclStmt;
struct ExprStmt;
struct IfStmt;
struct WhileStmt;
struct ForStmt;
struct SwitchStmt;
struct ReturnStmt;
struct IncludeGlobalDecl;
struct DeclGlobalDecl;
struct FuncDecl;

struct AstVisitor {
  virtual ~AstVisitor() = default;

  virtual void Visit(const BaseType& node) {}
  virtual void Visit(const TemplateType& node) {}
  virtual void Visit(const PointerType& node) {}
  virtual void Visit(const ReferenceType& node) {}
  virtual void Visit(const VariableExpr& node) {}
  virtual void Visit(const IntExpr& node) {}
  virtual void Visit(const FloatExpr& node) {}
  virtual void Visit(const StringExpr& node) {}
  virtual void Visit(const BinaryExpr& node) {}
  virtual void Visit(const UnaryExpr& node) {}
  virtual void Visit(const CallExpr& node) {}
  virtual void Visit(const MemberAccessExpr& node) {}
  virtual void Visit(const InitListExpr& node) {}
  virtual void Visit(const Decl& node) {}
  virtual void Visit(const CompoundStmt& node) {}
  virtual void Visit(const DeclStmt& node) {}
  virtual void Visit(const ExprStmt& node) {}
  virtual void Visit(const IfStmt& node) {}
  virtual void Visit(const WhileStmt& node) {}
  virtual void Visit(const ForStmt& node) {}
  virtual void Visit(const SwitchStmt& node) {}
  virtual void Visit(const ReturnStmt& node) {}
  virtual void Visit(const IncludeGlobalDecl& node) {}
  virtual void Visit(const DeclGlobalDecl& node) {}
  virtual void Visit(const FuncDecl& node) {}
};

struct AstNode {
  explicit AstNode(const Token& token) : token(token) {}
  virtual ~AstNode() = default;

  virtual void Accept(AstVisitor& visitor) const = 0;

  const Location& location() { return token.location; }

  const Token token;
};

enum struct TypeType {
  kBase,
  kTemplate,
  kPointer,
  kReference,
};

struct Type : public AstNode {
  using AstNode::AstNode;

  virtual TypeType GetTypeType() const = 0;
};

struct BaseType : public Type {
  explicit BaseType(const Token& token, std::string_view name)
      : Type(token), name(name) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kBase; }

  const std::string name;
};

struct TemplateType : public Type {
  explicit TemplateType(const Token& token, std::string_view name,
                        std::vector<Rc<Type>> args)
      : Type(token), name(name), args(std::move(args)) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kTemplate; }

  const std::string name;
  const std::vector<Rc<Type>> args;
};

struct PointerType : public Type {
  explicit PointerType(const Token& token, Rc<Type> sub_type)
      : Type(token), sub_type(sub_type) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kPointer; }

  const Rc<Type> sub_type;
};

struct ReferenceType : public Type {
  explicit ReferenceType(const Token& token, Rc<Type> sub_type)
      : Type(token), sub_type(sub_type) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kReference; }

  const Rc<Type> sub_type;
};

enum struct ExprType {
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

struct Expr : public AstNode {
  using AstNode::AstNode;

  virtual ExprType GetExprType() const = 0;
};

struct VariableExpr : public Expr {
  explicit VariableExpr(const Token& token, bool fully_qualified,
                        std::vector<std::string_view> namespaces,
                        std::string_view name)
      : Expr(token),
        fully_qualified(fully_qualified),
        namespaces(std::move(namespaces)),
        name(name) {}

  ExprType GetExprType() const override { return ExprType::kVariable; }
  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }

  const bool fully_qualified;
  const std::vector<std::string_view> namespaces;
  const std::string name;
};

struct IntExpr : public Expr {
  explicit IntExpr(const Token& token) : Expr(token) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kInt; }
};

struct FloatExpr : public Expr {
  explicit FloatExpr(const Token& token) : Expr(token) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kFloat; }
};

struct StringExpr : public Expr {
  explicit StringExpr(const Token& token) : Expr(token) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kString; }
};

enum struct BinExprType {
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

  kSubscript,  // a[i]
};

std::string_view BinExprTypeToString(BinExprType type);

struct BinaryExpr : public Expr {
  explicit BinaryExpr(const Token& token, BinExprType bin_expr_type,
                      Rc<Expr> left, Rc<Expr> right)
      : Expr(token), bin_expr_type(bin_expr_type), left(left), right(right) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kBinary; }

  const BinExprType bin_expr_type;

  const Rc<Expr> left;
  const Rc<Expr> right;
};

enum struct UnaryExprType {
  kUnaryMinus,
  kLogicNot,
  kBitNot,
  kDeref,
  kAddr,
  kParen,
};

std::string_view UnaryExprTypeToString(UnaryExprType type);

struct UnaryExpr : public Expr {
  explicit UnaryExpr(const Token& token, UnaryExprType unary_expr_type,
                     Rc<Expr> expr)
      : Expr(token), unary_expr_type(unary_expr_type), expr(expr) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kUnary; }

  const UnaryExprType unary_expr_type;
  const Rc<Expr> expr;
};

struct CallExpr : public Expr {
  explicit CallExpr(const Token& token, Rc<Expr> func,
                    std::vector<Rc<Expr>> args)
      : Expr(token), func(func), args(std::move(args)) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kCall; }

  const Rc<Expr> func;
  const std::vector<Rc<Expr>> args;
};

struct MemberAccessExpr : public Expr {
  explicit MemberAccessExpr(const Token& token, Rc<Expr> expr,
                            std::string_view member_name)
      : Expr(token), expr(expr), member_name(member_name) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kMemberAccess; }

  const Rc<Expr> expr;
  const std::string member_name;
};

struct InitListExpr : public Expr {
  explicit InitListExpr(const Token& token, std::vector<Rc<Expr>> exprs)
      : Expr(token), exprs(std::move(exprs)) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kInitList; }

  const std::vector<Rc<Expr>> exprs;
};

enum DeclFlags {
  kDeclFlagsNone = 0,
  kDeclFlagsMut = 1 << 0,
  kDeclFlagsStatic = 1 << 1,
};

std::string DeclFlagsToString(DeclFlags type);

struct Decl : public AstNode {
  explicit Decl(const Token& token, DeclFlags decl_flags, std::string_view name,
                Rc<Type> type, Rc<Expr> expr)
      : AstNode(token),
        decl_flags(decl_flags),
        name(name),
        type(type),
        expr(expr) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }

  const DeclFlags decl_flags;
  const std::string name;
  const Rc<Type> type;
  const Rc<Expr> expr;
};

enum class StmtType {
  kCompound,
  kDecl,
  kExpr,
  kIf,
  kWhile,
  kFor,
  kSwitch,
  kReturn,
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
      : Stmt(token), stmts(std::move(stmts)) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kCompound; }

  const std::vector<Rc<Stmt>> stmts;
};

struct DeclStmt : public Stmt {
  explicit DeclStmt(Rc<Decl> decl) : Stmt(decl->token), decl(decl) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kCompound; }

  const Rc<Decl> decl;
};

struct ExprStmt : public Stmt {
  explicit ExprStmt(Rc<Expr> expr) : Stmt(expr->token), expr(expr) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kExpr; }

  const Rc<Expr> expr;
};

struct IfStmt : public Stmt {
  explicit IfStmt(const Token& token, Rc<Expr> test, Rc<Stmt> true_stmt,
                  Rc<Stmt> false_stmt)
      : Stmt(token), test(test), true_stmt(true_stmt), false_stmt(false_stmt) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kIf; }

  const Rc<Expr> test;
  const Rc<Stmt> true_stmt;
  const Rc<Stmt> false_stmt;
};

struct WhileStmt : public Stmt {
  explicit WhileStmt(const Token& token, Rc<Expr> test, Rc<Stmt> body)
      : Stmt(token), test(test), body(body) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kWhile; }

  const Rc<Expr> test;
  const Rc<Stmt> body;
};

struct ForStmt : public Stmt {
  explicit ForStmt(const Token& token, Rc<Decl> decl, Rc<Expr> expr)
      : Stmt(token), decl(decl), expr(expr) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kFor; }

  const Rc<Decl> decl;
  const Rc<Expr> expr;
};

struct SwitchStmt : public Stmt {
  struct Case {
    Rc<Expr> test;
    Rc<Expr> expr;
  };
  explicit SwitchStmt(const Token& token, Rc<Expr> test,
                      std::vector<Case> cases, Rc<Expr> default_expr)
      : Stmt(token),
        test(test),
        cases(std::move(cases)),
        default_expr(default_expr) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kSwitch; }

  const Rc<Expr> test;
  const std::vector<Case> cases;
  const Rc<Expr> default_expr;
};

struct ReturnStmt : public Stmt {
  explicit ReturnStmt(Rc<Expr> expr) : Stmt(expr->token), expr(expr) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kExpr; }

  const Rc<Expr> expr;
};

enum struct GlobalDeclType {
  kInclude,
  kDecl,
  kFunc,
};

struct GlobalDecl : public AstNode {
  using AstNode::AstNode;

  virtual GlobalDeclType GetGlobalDeclType() const = 0;
};

enum struct IncludeGlobalDeclType {
  kBracket,
  kQuote,
};

struct IncludeGlobalDecl : public GlobalDecl {
  explicit IncludeGlobalDecl(const Token& token, IncludeGlobalDeclType type,
                             std::string_view path)
      : GlobalDecl(token), type(type), path(path) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kInclude;
  }

  const IncludeGlobalDeclType type;
  const std::string path;
};

struct DeclGlobalDecl : public GlobalDecl {
  explicit DeclGlobalDecl(const Token& token, Rc<Decl> decl)
      : GlobalDecl(token), decl(decl) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kDecl;
  }

  const Rc<Decl> decl;
};

struct FuncDecl : public GlobalDecl {
  explicit FuncDecl(const Token& token, std::string_view name,
                    std::vector<Rc<Decl>> args, Rc<Type> ret_type,
                    Rc<CompoundStmt> body)
      : GlobalDecl(token),
        name(name),
        args(std::move(args)),
        ret_type(ret_type),
        body(body) {}

  void Accept(AstVisitor& visitor) const { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kFunc;
  }

  const std::string name;
  const std::vector<Rc<Decl>> args;
  const Rc<Type> ret_type;
  const Rc<CompoundStmt> body;
};

struct CompilationUnit {
  std::vector<Rc<GlobalDecl>> global_decls;
};
