#include "lex/token.h"

std::string_view TokenTypeToString(TokenType token_type) {
#define CASE_STR(token)  \
  case TokenType::token: \
    return #token

  switch (token_type) {
    CASE_STR(kEof);
    CASE_STR(kLBrace);
    CASE_STR(kRBrace);
    CASE_STR(kLParen);
    CASE_STR(kRParen);
    CASE_STR(kSemi);
    CASE_STR(kComma);
    CASE_STR(kLBrack);
    CASE_STR(kRBrack);
    CASE_STR(kArrow);
    CASE_STR(kDot);
    CASE_STR(kCond);
    CASE_STR(kColon);
    CASE_STR(kAssign);
    CASE_STR(kPlusEq);
    CASE_STR(kMinusEq);
    CASE_STR(kStarEq);
    CASE_STR(kDivEq);
    CASE_STR(kModEq);
    CASE_STR(kBitXorEq);
    CASE_STR(kBitOrEq);
    CASE_STR(kBitAndEq);
    CASE_STR(kRShiftEq);
    CASE_STR(kLShiftEq);
    CASE_STR(kEq);
    CASE_STR(kNe);
    CASE_STR(kLt);
    CASE_STR(kGt);
    CASE_STR(kLe);
    CASE_STR(kGe);
    CASE_STR(kRShift);
    CASE_STR(kLShift);
    CASE_STR(kLogicAnd);
    CASE_STR(kLogicOr);
    CASE_STR(kLogicNot);
    CASE_STR(kPlus);
    CASE_STR(kMinus);
    CASE_STR(kStar);
    CASE_STR(kDiv);
    CASE_STR(kMod);
    CASE_STR(kBitAnd);
    CASE_STR(kBitOr);
    CASE_STR(kBitXor);
    CASE_STR(kBitNot);
    CASE_STR(kInclude);
    CASE_STR(kFn);
    CASE_STR(kLet);
    CASE_STR(kMut);
    CASE_STR(kBreak);
    CASE_STR(kCase);
    CASE_STR(kContinue);
    CASE_STR(kDefault);
    CASE_STR(kElse);
    CASE_STR(kEnum);
    CASE_STR(kFor);
    CASE_STR(kIf);
    CASE_STR(kReturn);
    CASE_STR(kSizeof);
    CASE_STR(kStatic);
    CASE_STR(kStruct);
    CASE_STR(kSwitch);
    CASE_STR(kUsing);
    CASE_STR(kUnion);
    CASE_STR(kWhile);
    CASE_STR(kStaticAssert);
    CASE_STR(kId);
    CASE_STR(kString);
    CASE_STR(kIntLit);
    CASE_STR(kFloatLit);
  }

#undef CASE_STR

  return "UNKNOWN";
}

std::ostream& operator<<(std::ostream& os, const Token& token) {
  os << TokenTypeToString(token.type) << "(" << token.text << ")";
  return os;
}
