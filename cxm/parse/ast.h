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
struct ReferenceType;
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
  virtual void Visit(const ReferenceType& node) = 0;
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
  explicit BaseType(Identifier id, std::vector<Rc<Type>> template_args = {})
      : Type(id.token),
        id(std::move(id)),
        template_args(std::move(template_args)) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kBase; }

  const Identifier id;
  const std::vector<Rc<Type>> template_args;
};

struct PointerType : public Type {
  explicit PointerType(const Token& token, Rc<Type> sub_type)
      : Type(token), sub_type(sub_type) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kPointer; }

  const Rc<Type> sub_type;
};

struct ReferenceType : public Type {
  explicit ReferenceType(const Token& token, Rc<Type> sub_type)
      : Type(token), sub_type(sub_type) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  TypeType GetTypeType() const override { return TypeType::kReference; }

  const Rc<Type> sub_type;
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
  ClassAccessType access = ClassAccessType::kPublic;
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
  ClassAccessType access = ClassAccessType::kPublic;
  std::vector<ClassMember> members;
};

struct Class : public AstNode {
  explicit Class(const Token& token, ClassType type, std::string_view name,
                 std::vector<ClassBase> bases,
                 std::vector<ClassSection> sections)
      : AstNode(token),
        type(type),
        name(name),
        bases(std::move(bases)),
        sections(std::move(sections)) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }

  const ClassType type;
  const std::string_view name;
  const std::vector<ClassBase> bases;
  const std::vector<ClassSection> sections;
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

  const Identifier id;
};

struct IntExpr : public Expr {
  explicit IntExpr(const Token& token) : Expr(token) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kInt; }
};

struct FloatExpr : public Expr {
  explicit FloatExpr(const Token& token) : Expr(token) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kFloat; }
};

struct StringExpr : public Expr {
  explicit StringExpr(const Token& token) : Expr(token) {}

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

  const BinExprType bin_expr_type;

  const Rc<Expr> left;
  const Rc<Expr> right;
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

  const UnaryExprType unary_expr_type;
  const Rc<Expr> expr;
};

struct CallExpr : public Expr {
  explicit CallExpr(const Token& token, Rc<Expr> func,
                    std::vector<Rc<Expr>> args)
      : Expr(token), func(func), args(std::move(args)) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kCall; }

  const Rc<Expr> func;
  const std::vector<Rc<Expr>> args;
};

struct MemberAccessExpr : public Expr {
  explicit MemberAccessExpr(const Token& token, Rc<Expr> expr,
                            std::string_view member_name)
      : Expr(token), expr(expr), member_name(member_name) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  ExprType GetExprType() const override { return ExprType::kMemberAccess; }

  const Rc<Expr> expr;
  const std::string_view member_name;
};

struct InitListExpr : public Expr {
  explicit InitListExpr(const Token& token, std::vector<Rc<Expr>> exprs)
      : Expr(token), exprs(std::move(exprs)) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
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

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }

  const DeclFlags decl_flags;
  const std::string_view name;
  const Rc<Type> type;
  const Rc<Expr> expr;
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
  explicit CompoundStmt(const Token& token, std::vector<Rc<Stmt>> stmts)
      : Stmt(token), stmts(std::move(stmts)) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kCompound; }

  const std::vector<Rc<Stmt>> stmts;
};

struct UnaryStmt : public Stmt {
  using Val = std::variant<Rc<Decl>, Rc<Class>, Rc<Expr>>;

  explicit UnaryStmt(Val val)
      : Stmt(std::visit([](const auto& val) { return val->token; }, val)),
        val(val) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kUnary; }

  const Val val;
};

struct IfStmt : public Stmt {
  explicit IfStmt(const Token& token, Rc<Expr> test, Rc<Stmt> true_stmt,
                  Rc<Stmt> false_stmt)
      : Stmt(token), test(test), true_stmt(true_stmt), false_stmt(false_stmt) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kIf; }

  const Rc<Expr> test;
  const Rc<Stmt> true_stmt;
  const Rc<Stmt> false_stmt;
};

struct WhileStmt : public Stmt {
  explicit WhileStmt(const Token& token, Rc<Expr> test, Rc<Stmt> body)
      : Stmt(token), test(test), body(body) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kWhile; }

  const Rc<Expr> test;
  const Rc<Stmt> body;
};

struct ForStmt : public Stmt {
  explicit ForStmt(const Token& token, Rc<Decl> decl, Rc<Expr> expr,
                   Rc<Stmt> body)
      : Stmt(token), decl(decl), expr(expr), body(body) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kFor; }

  const Rc<Decl> decl;
  const Rc<Expr> expr;
  const Rc<Stmt> body;
};

struct SwitchStmt : public Stmt {
  struct Case {
    Rc<Expr> test;
    Rc<Stmt> stmt;
  };
  explicit SwitchStmt(const Token& token, Rc<Expr> test,
                      std::vector<Case> cases, Rc<Expr> default_expr)
      : Stmt(token),
        test(test),
        cases(std::move(cases)),
        default_expr(default_expr) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kSwitch; }

  const Rc<Expr> test;
  const std::vector<Case> cases;
  const Rc<Expr> default_expr;
};

struct ReturnStmt : public Stmt {
  explicit ReturnStmt(Rc<Expr> expr) : Stmt(expr->token), expr(expr) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  StmtType GetStmtType() const override { return StmtType::kReturn; }

  const Rc<Expr> expr;
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

  const IncludeGlobalDeclType type;
  const std::string path;
};

struct UnaryGlobalDecl : public GlobalDecl {
  using Val = std::variant<Rc<Decl>, Rc<Class>>;

  explicit UnaryGlobalDecl(const Token& token, Val val)
      : GlobalDecl(token), val(val) {}

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kUnary;
  }

  const Val val;
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

  void Accept(AstVisitor& visitor) const override { visitor.Visit(*this); }
  GlobalDeclType GetGlobalDeclType() const override {
    return GlobalDeclType::kFunc;
  }

  const std::string_view name;
  const std::vector<Rc<Decl>> args;
  const Rc<Type> ret_type;
  const Rc<CompoundStmt> body;
};

struct CompilationUnit {
  std::vector<Rc<GlobalDecl>> global_decls;
};
