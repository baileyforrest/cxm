#pragma once

#include <memory>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "cxm/lex/token.h"
#include "cxm/util/rc.h"

struct BaseType;
struct PointerType;
struct ClassCtor;
struct Class;
struct Expr;
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
struct UnaryStmt;
struct IfStmt;
struct WhileStmt;
struct ForStmt;
struct SwitchStmt;
struct ReturnStmt;
struct IncludeGlobalDecl;
struct UnaryGlobalDecl;
struct FuncDecl;

struct AstVisitor {
  virtual ~AstVisitor() = default;

  virtual void Visit(const BaseType& node) = 0;
  virtual void Visit(const PointerType& node) = 0;
  virtual void Visit(const ClassCtor& node) = 0;
  virtual void Visit(const Class& node) = 0;
  virtual void Visit(const VariableExpr& node) = 0;
  virtual void Visit(const IntExpr& node) = 0;
  virtual void Visit(const FloatExpr& node) = 0;
  virtual void Visit(const StringExpr& node) = 0;
  virtual void Visit(const BinaryExpr& node) = 0;
  virtual void Visit(const UnaryExpr& node) = 0;
  virtual void Visit(const CallExpr& node) = 0;
  virtual void Visit(const MemberAccessExpr& node) = 0;
  virtual void Visit(const InitListExpr& node) = 0;
  virtual void Visit(const Decl& node) = 0;
  virtual void Visit(const CompoundStmt& node) = 0;
  virtual void Visit(const UnaryStmt& node) = 0;
  virtual void Visit(const IfStmt& node) = 0;
  virtual void Visit(const WhileStmt& node) = 0;
  virtual void Visit(const ForStmt& node) = 0;
  virtual void Visit(const SwitchStmt& node) = 0;
  virtual void Visit(const ReturnStmt& node) = 0;
  virtual void Visit(const IncludeGlobalDecl& node) = 0;
  virtual void Visit(const UnaryGlobalDecl& node) = 0;
  virtual void Visit(const FuncDecl& node) = 0;
};

struct AstNode {
  explicit AstNode(const Token& token) : token(token) {}
  virtual ~AstNode() = default;

  virtual void Accept(AstVisitor& visitor) const = 0;

  const Location& location() { return token.location; }

  const Token token;
};

struct Identifier {
  Token token;
  bool fully_qualified = false;
  std::vector<std::string_view> namespaces;
  std::string_view name;
};

enum class TypeType {
  kBase,
  kPointer,
  kReference,
};

struct Type : public AstNode {
  using AstNode::AstNode;
  virtual TypeType GetTypeType() const = 0;
};

struct BaseType : public Type {
  explicit BaseType(Identifier id) : Type(id.token), id(std::move(id)) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kBase; }

  Identifier id;
  std::vector<Rc<Type>> template_args;
};

enum CvQual {
  kCvQualNone,
  kCvQualConst = 1 << 0,
  kCvQualVolatile = 1 << 1,
};

// Pointer or Reference.
struct PointerType : public Type {
  using Type::Type;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  TypeType GetTypeType() const override {
    if (token.type == TokenType::kStar) {
      return TypeType::kPointer;
    }
    assert(token.type == TokenType::kBitAnd);
    return TypeType::kReference;
  }

  Rc<Type> sub_type;
  CvQual qual{};
};

enum class ClassType {
  kClass,
  kStruct,
  kUnion,
};

enum class ClassAccessType {
  kPublic,
  kPrivate,
};

struct ClassBase {
  ClassAccessType access{};
  Rc<BaseType> type;
};

struct ClassCtorMemberInit {
  std::string_view name() const { return token.text; }

  Token token;
  Rc<Expr> expr;
};

struct ClassCtor : public AstNode {
  using AstNode::AstNode;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }

  std::string_view name;
  std::vector<Rc<Decl>> args;
  std::vector<ClassCtorMemberInit> member_inits;
  Rc<CompoundStmt> body;
};

using ClassMember = std::variant<Rc<ClassCtor>, Rc<FuncDecl>, Rc<Decl>>;

struct ClassSection {
  ClassAccessType access{};
  std::vector<ClassMember> members;
};

struct Class : public AstNode {
  using AstNode::AstNode;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }

  ClassType type{};
  std::string_view name;
  std::vector<ClassBase> bases;
  std::vector<ClassSection> sections;
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

struct Expr : public AstNode {
  using AstNode::AstNode;
  virtual ExprType GetExprType() const = 0;
};

struct VariableExpr : public Expr {
  explicit VariableExpr(Identifier id) : Expr(id.token), id(std::move(id)) {}

  ExprType GetExprType() const override { return ExprType::kVariable; }
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }

  Identifier id;
};

struct IntExpr : public Expr {
  using Expr::Expr;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kInt; }
};

struct FloatExpr : public Expr {
  using Expr::Expr;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kFloat; }
};

struct StringExpr : public Expr {
  using Expr::Expr;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
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

  kSubscript,  // a[i]
};

std::string_view BinExprTypeToString(BinExprType type);

struct BinaryExpr : public Expr {
  explicit BinaryExpr(const Token& token, BinExprType bin_expr_type,
                      Rc<Expr> left, Rc<Expr> right)
      : Expr(token), bin_expr_type(bin_expr_type), left(left), right(right) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kBinary; }

  BinExprType bin_expr_type{};
  Rc<Expr> left;
  Rc<Expr> right;
};

enum class UnaryExprType {
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

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kUnary; }

  UnaryExprType unary_expr_type{};
  Rc<Expr> expr;
};

struct CallExpr : public Expr {
  using Expr::Expr;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kCall; }

  Rc<Expr> func;
  std::vector<Rc<Expr>> args;
};

struct MemberAccessExpr : public Expr {
  explicit MemberAccessExpr(const Token& token, Rc<Expr> expr,
                            std::string_view member_name)
      : Expr(token), expr(expr), member_name(member_name) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kMemberAccess; }

  Rc<Expr> expr;
  std::string_view member_name;
};

struct InitListExpr : public Expr {
  using Expr::Expr;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kInitList; }

  std::vector<Rc<Expr>> exprs;
};

enum DeclFlags {
  kDeclFlagsNone = 0,
  kDeclFlagsMut = 1 << 0,
  kDeclFlagsStatic = 1 << 1,
};

std::string DeclFlagsToString(DeclFlags type);

struct Decl : public AstNode {
  explicit Decl(const Token& name, DeclFlags flags)
      : AstNode(name), name(name.text), flags(flags) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }

  std::string_view name;
  DeclFlags flags;
  Rc<Type> type;
  Rc<Expr> expr;
};

enum class StmtType {
  kCompound,
  kUnary,
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
  using Stmt::Stmt;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kCompound; }

  std::vector<Rc<Stmt>> stmts;
};

struct UnaryStmt : public Stmt {
  using Val = std::variant<Rc<Decl>, Rc<Class>, Rc<Expr>>;

  explicit UnaryStmt(Val val)
      : Stmt(std::visit([](const auto& val) { return val->token; }, val)),
        val(val) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kUnary; }

  Val val;
};

struct IfStmt : public Stmt {
  using Stmt::Stmt;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kIf; }

  Rc<Expr> test;
  Rc<Stmt> true_stmt;
  Rc<Stmt> false_stmt;
};

struct WhileStmt : public Stmt {
  using Stmt::Stmt;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kWhile; }

  Rc<Expr> test;
  Rc<Stmt> body;
};

struct ForStmt : public Stmt {
  using Stmt::Stmt;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kFor; }

  Rc<Decl> decl;
  Rc<Expr> expr;
  Rc<Stmt> body;
};

struct SwitchStmt : public Stmt {
  struct Case {
    Rc<Expr> test;
    Rc<Stmt> stmt;
  };

  using Stmt::Stmt;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kSwitch; }

  Rc<Expr> test;
  std::vector<Case> cases;
  Rc<Expr> default_expr;
};

struct ReturnStmt : public Stmt {
  explicit ReturnStmt(Rc<Expr> expr) : Stmt(expr->token), expr(expr) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kReturn; }

  Rc<Expr> expr;
};

enum class GlobalDeclType {
  kInclude,
  kUnary,
  kFunc,
};

struct GlobalDecl : public AstNode {
  using AstNode::AstNode;
  virtual GlobalDeclType GetGlobalDeclType() const = 0;
};

enum class IncludeGlobalDeclType {
  kBracket,
  kQuote,
};

struct IncludeGlobalDecl : public GlobalDecl {
  explicit IncludeGlobalDecl(const Token& token, IncludeGlobalDeclType type,
                             std::string_view path)
      : GlobalDecl(token), type(type), path(path) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kInclude;
  }

  IncludeGlobalDeclType type;
  std::string_view path;
};

struct UnaryGlobalDecl : public GlobalDecl {
  using Val = std::variant<Rc<Decl>, Rc<Class>>;

  explicit UnaryGlobalDecl(const Token& token, Val val)
      : GlobalDecl(token), val(val) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kUnary;
  }

  Val val;
};

enum FuncSpec {
  kFuncSpecNone = 0,
  kFuncSpecConst = 1 << 0,
  kFuncSpecOverride = 1 << 1,
  kFuncSpecFinal = 1 << 2,
};

struct FuncDecl : public GlobalDecl {
  using GlobalDecl::GlobalDecl;
  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kFunc;
  }

  std::string_view name;
  std::vector<Rc<Decl>> args;
  FuncSpec spec{};
  Rc<Type> ret_type;
  Rc<CompoundStmt> body;
};

struct CompilationUnit {
  std::vector<Rc<GlobalDecl>> global_decls;
};
