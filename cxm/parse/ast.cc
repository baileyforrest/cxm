#include "cxm/parse/ast.h"

#include "absl/strings/str_cat.h"

std::string_view ExprTypeToString(ExprType type) {
#define CASE_STR(item) \
  case ExprType::item: \
    return #item

  switch (type) {
    CASE_STR(kVariable);
    CASE_STR(kInt);
    CASE_STR(kFloat);
    CASE_STR(kString);
    CASE_STR(kChar);
    CASE_STR(kBinary);
    CASE_STR(kUnary);
    CASE_STR(kCast);
    CASE_STR(kCall);
    CASE_STR(kMemberAccessDot);
    CASE_STR(kMemberAccessArrow);
    CASE_STR(kInitList);
  }

  return "UNKNOWN ExprType";
#undef CASE_STR
}

std::string_view BinExprTypeToString(BinExprType type) {
#define CASE_STR(item)    \
  case BinExprType::item: \
    return #item

  switch (type) {
    CASE_STR(kPlus);
    CASE_STR(kMinus);
    CASE_STR(kTimes);
    CASE_STR(kDiv);
    CASE_STR(kMod);
    CASE_STR(kLt);
    CASE_STR(kLe);
    CASE_STR(kGt);
    CASE_STR(kGe);
    CASE_STR(kEq);
    CASE_STR(kNe);
    CASE_STR(kBitAnd);
    CASE_STR(kBitXor);
    CASE_STR(kBitOr);
    CASE_STR(kLShift);
    CASE_STR(kRShift);
    CASE_STR(kLogicAnd);
    CASE_STR(kLogicOr);
    CASE_STR(kSubscript);
  }

  return "UNKNOWN BinExprType";
#undef CASE_STR
}

std::string_view UnaryExprTypeToString(UnaryExprType type) {
#define CASE_STR(item)      \
  case UnaryExprType::item: \
    return #item

  switch (type) {
    CASE_STR(kUnaryMinus);
    CASE_STR(kLogicNot);
    CASE_STR(kBitNot);
    CASE_STR(kDeref);
    CASE_STR(kAddr);
    CASE_STR(kParen);
  }

  return "UNKNOWN UnaryExprType";
#undef CASE_STR
}

std::string DeclFlagsToString(DeclFlags decl_flags) {
  if (decl_flags == kDeclFlagsNone) {
    return "kDeclFlagsNone";
  }

  std::string ret;
  if (decl_flags & kDeclFlagsMut) {
    absl::StrAppend(&ret, "kDeclFlagsMut|");
  }

  if (decl_flags & kDeclFlagsStatic) {
    absl::StrAppend(&ret, "kDeclFlagsStatic|");
  }

  return ret;
}

std::string_view StmtTypeToString(StmtType type) {
#define CASE_STR(item) \
  case StmtType::item: \
    return #item

  switch (type) {
    CASE_STR(kCompound);
    CASE_STR(kUnary);
    CASE_STR(kIf);
    CASE_STR(kWhile);
    CASE_STR(kFor);
    CASE_STR(kSwitch);
    CASE_STR(kReturn);
  }

  return "UNKNOWN Stmt";
#undef CASE_STR
}
