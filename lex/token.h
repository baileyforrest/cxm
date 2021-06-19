#pragma once

#include <ostream>
#include <string>

#include "absl/strings/string_view.h"
#include "util/location.h"

enum class TokenType {
  // Delimiters
  kLBrace,  // {
  kRBrace,  // }
  kLParen,  // (
  kRParen,  // )
  kSemi,    // ;
  kComma,   // ,
  kLBrack,  // [
  kRBrack,  // ]
  kArrow,   // ->
  kDot,     // .
  kCond,    // ?
  kColon,   // :

  // Assignment operators
  kAssign,    // =
  kPlusEq,    // +=
  kMinusEq,   // -=
  kStarEq,    // *=
  kDivEq,     // /=
  kModEq,     // %=
  kBitXorEq,  // ^=
  kBitOrEq,   // |=
  kBitAndEq,  // &=
  kRShiftEq,  // >>=
  kLShiftEq,  // <<=

  // Comparison operators
  kEq,  // ==
  kNe,  // !=
  kLt,  // <
  kGt,  // >
  kLe,  // <=
  kGe,  // >=

  // Arithmetic
  kRShift,  // >>
  kLShift,  // <<

  kLogicAnd,  // &&
  kLogicOr,   // ||
  kLogicNot,  // !

  kPlus,   // +
  kMinus,  // -
  kStar,   // *
  kDiv,    // /
  kMod,    // %

  kBitAnd,  // &
  kBitOr,   // |
  kBitXor,  // ^
  kBitNot,  // ~

  // Keywords
  kInclude,       // include
  kFn,            // fn
  kLet,           // let
  kMut,           // mut
  kBreak,         // break
  kCase,          // case
  kContinue,      // continue
  kDefault,       // default
  kElse,          // else
  kEnum,          // enum
  kFor,           // for
  kIf,            // if
  kReturn,        // return
  kSizeof,        // sizeof
  kStatic,        // static
  kStruct,        // struct
  kSwitch,        // switch
  kUsing,         // using
  kUnion,         // union
  kWhile,         // while
  kStaticAssert,  // static_assert

  // Other
  kId,        // identifier
  kString,    // string
  kIntLit,    // Integral literal
  kFloatLit,  // Float literal
};

const char* TokenTypeToString(TokenType token_type);

class Token {
 public:
  Token(TokenType type, const Location& location, absl::string_view text)
      : type_(type), location_(location), text_(text) {}

  Token(const Token&) = default;
  Token& operator=(const Token&) = default;

  TokenType type() const { return type_; }
  const Location& location() const { return location_; }
  const absl::string_view& text() const { return text_; }

 private:
  const TokenType type_;
  const Location location_;
  const absl::string_view text_;
};

std::ostream& operator<<(std::ostream& os, const Token& token);
