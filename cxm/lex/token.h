#pragma once

#include <ostream>
#include <string>

#include "cxm/util/location.h"

enum class TokenType {
  kEof,

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
  kScope,   // ::

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
  kBreak,         // break
  kCase,          // case
  kClass,         // class
  kContinue,      // continue
  kDefault,       // default
  kElse,          // else
  kEnum,          // enum
  kFn,            // fn
  kFor,           // for
  kIf,            // if
  kInclude,       // include
  kLet,           // let
  kMut,           // mut
  kPrivate,       // private
  kPublic,        // public
  kReturn,        // return
  kSizeof,        // sizeof
  kStatic,        // static
  kStaticAssert,  // static_assert
  kStruct,        // struct
  kSwitch,        // switch
  kThisType,      // This
  kUnion,         // union
  kUsing,         // using
  kWhile,         // while

  // Other
  kId,        // identifier
  kString,    // string
  kIntLit,    // Integral literal
  kFloatLit,  // Float literal
};

std::string_view TokenTypeToString(TokenType token_type);

struct Token {
  TokenType type = TokenType::kEof;
  Location location;
  std::string_view text;

  bool is_eof() const { return type == TokenType::kEof; }
};

std::ostream& operator<<(std::ostream& os, const Token& token);
