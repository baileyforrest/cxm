#include "cxm/lex/lexer.h"

#include <cctype>

#include "absl/strings/str_format.h"

namespace {

static constexpr struct {
  TokenType type;
  std::string_view str;
} kReservedKeywordToString[] = {
    {TokenType::kBreak, "break"},
    {TokenType::kCase, "case"},
    {TokenType::kClass, "class"},
    {TokenType::kContinue, "continue"},
    {TokenType::kDefault, "default"},
    {TokenType::kElse, "else"},
    {TokenType::kEnum, "enum"},
    {TokenType::kFn, "fn"},
    {TokenType::kFor, "for"},
    {TokenType::kIf, "if"},
    {TokenType::kInclude, "include"},
    {TokenType::kLet, "let"},
    {TokenType::kMut, "mut"},
    {TokenType::kPrivate, "private"},
    {TokenType::kPublic, "public"},
    {TokenType::kReturn, "return"},
    {TokenType::kSizeof, "sizeof"},
    {TokenType::kStatic, "static"},
    {TokenType::kStaticAssert, "static_assert"},
    {TokenType::kStruct, "struct"},
    {TokenType::kSwitch, "switch"},
    {TokenType::kThisType, "This"},
    {TokenType::kUsing, "using"},
    {TokenType::kUnion, "union"},
    {TokenType::kWhile, "while"},
};

}  // namespace

Lexer::Lexer(TextStream* text_stream) : text_stream_(text_stream) {
  ABSL_ASSERT(text_stream);
}

Token Lexer::NextToken() {
  // Skip spaces.
  while (text_stream_->HasChar() && std::isspace(text_stream_->Peek())) {
    text_stream_->Inc();
  }

  if (!text_stream_->HasChar()) {
    return Token{TokenType::kEof, {}, ""};
  }

  char c = text_stream_->Peek();
  const Location location = text_stream_->location();
  const char* const start_position = text_stream_->BufPosition();

  auto make_token = [&](TokenType type) -> Token {
    size_t token_length = text_stream_->BufPosition() - start_position;
    return Token{type, location, {start_position, token_length}};
  };

  auto inc_and_make_token = [&](TokenType type) -> Token {
    text_stream_->Inc();
    return make_token(type);
  };

  auto cond_make_token = [&](TokenType type1, char second_char,
                             TokenType type2) -> Token {
    text_stream_->Inc();
    TokenType type = type1;

    if (text_stream_->HasChar() && text_stream_->Peek() == second_char) {
      type = type2;
      text_stream_->Inc();
    }

    return make_token(type);
  };

  auto cond2_make_token = [&](TokenType type1, char char2, TokenType type2,
                              char char3, TokenType type3) -> Token {
    text_stream_->Inc();
    TokenType type = type1;

    if (text_stream_->HasChar()) {
      if (text_stream_->Peek() == char2) {
        type = type2;
        text_stream_->Inc();
      } else if (text_stream_->Peek() == char3) {
        type = type3;
        text_stream_->Inc();
      }
    }

    return make_token(type);
  };

  switch (c) {
    case '{':
      return inc_and_make_token(TokenType::kLBrace);
    case '}':
      return inc_and_make_token(TokenType::kRBrace);
    case '(':
      return inc_and_make_token(TokenType::kLParen);
    case ')':
      return inc_and_make_token(TokenType::kRParen);
    case ';':
      return inc_and_make_token(TokenType::kSemi);
    case ',':
      return inc_and_make_token(TokenType::kComma);
    case '[':
      return inc_and_make_token(TokenType::kLBrack);
    case ']':
      return inc_and_make_token(TokenType::kRBrack);
    case '?':
      return inc_and_make_token(TokenType::kCond);
    case ':':
      return cond_make_token(TokenType::kColon, ':', TokenType::kScope);
    case '~':
      return inc_and_make_token(TokenType::kBitNot);
    case '=':
      return cond_make_token(TokenType::kAssign, '=', TokenType::kEq);
    case '+':
      return cond_make_token(TokenType::kPlus, '=', TokenType::kPlusEq);
    case '*':
      return cond_make_token(TokenType::kStar, '=', TokenType::kStarEq);
    case '%':
      return cond_make_token(TokenType::kMod, '=', TokenType::kModEq);
    case '!':
      return cond_make_token(TokenType::kLogicNot, '=', TokenType::kNe);
    case '^':
      return cond_make_token(TokenType::kBitXor, '=', TokenType::kBitXorEq);
    case '-':
      return cond2_make_token(TokenType::kMinus, '=', TokenType::kMinusEq, '>',
                              TokenType::kArrow);
    case '|':
      return cond2_make_token(TokenType::kBitOr, '|', TokenType::kLogicOr, '=',
                              TokenType::kBitOrEq);
    case '&':
      return cond2_make_token(TokenType::kBitAnd, '&', TokenType::kLogicAnd,
                              '=', TokenType::kBitAndEq);

    case '/': {
      text_stream_->Inc();
      if (!text_stream_->HasChar()) {
        return make_token(TokenType::kDiv);
      }

      char next_char = text_stream_->Peek();

      switch (next_char) {
        // Single line comment.
        case '/': {
          while (text_stream_->HasChar() && text_stream_->Peek() != '\n') {
            text_stream_->Inc();
          }

          if (text_stream_->HasChar()) {
            text_stream_->Inc();
          }
          break;
        }

        // Multi line comment.
        case '*': {
          char last = 0;
          while (true) {
            if (!text_stream_->HasChar()) {
              throw Error("unterminated comment", location);
            }

            char cur = text_stream_->Peek();
            if (last == '*' && cur == '/') {
              text_stream_->Inc();
              break;
            }

            text_stream_->Inc();
          }
          break;
        }

        case '=':
          return inc_and_make_token(TokenType::kDivEq);

        default:
          return make_token(TokenType::kDiv);
      }

      break;
    }  // case '/'

    case '.': {
      text_stream_->Inc();

      if (text_stream_->HasChar()) {
        char next_char = text_stream_->Peek();
        if (std::isdigit(next_char)) {
          text_stream_->Dec();
          return LexNumber();
        }
      }

      return make_token(TokenType::kDot);
    }

    case '>': {
      text_stream_->Inc();
      if (!text_stream_->HasChar()) {
        return make_token(TokenType::kGt);
      }

      char next_char = text_stream_->Peek();
      switch (next_char) {
        case '=':
          return inc_and_make_token(TokenType::kGe);
        case '>':
          return cond_make_token(TokenType::kRShift, '=', TokenType::kRShiftEq);
        default:
          return make_token(TokenType::kGt);
      }
    }

    case '<': {
      text_stream_->Inc();
      if (!text_stream_->HasChar()) {
        return make_token(TokenType::kLt);
      }

      char next_char = text_stream_->Peek();
      switch (next_char) {
        case '=':
          return inc_and_make_token(TokenType::kLe);
        case '<':
          return cond_make_token(TokenType::kLShift, '=', TokenType::kLShiftEq);
        default:
          return make_token(TokenType::kLt);
      }
    }

    // Identifiers:
    // clang-format off
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':

    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
    // clang-format on
    case '_':
      return LexId();

    case '"':
      return LexString();

    case '\'':
      return LexChar();

    // clang-format off
    case '0': case '1': case '2': case '3': case '4': case '5': case '6':
    case '7': case '8': case '9':
      // clang-format on
      return LexNumber();

    default:
      break;
  }

  throw Error(absl::StrFormat("Unexpected character: %c", c), location);
}

Token Lexer::LexNumber() {
  const Location location = text_stream_->location();
  const char* const start_position = text_stream_->BufPosition();

  int num_dots = 0;
  int num_xs = 0;

  size_t length = 0;

  while (text_stream_->HasChar()) {
    char cur = text_stream_->Peek();
    if (std::isxdigit(cur)) {
    } else if (cur == '.') {
      num_dots += 1;
    } else if (cur == 'x') {
      num_xs += 1;
    } else {
      break;
    }

    ++length;
    text_stream_->Inc();
  }

  std::string_view text(start_position, length);

  bool invalid_hex = num_xs > 1 || (num_xs == 1 && text[1] != 'x');
  bool invalid_float = num_dots > 1 || (num_dots == 1 && num_xs > 0);

  if (invalid_hex || invalid_float) {
    throw Error(absl::StrCat("Invalid numeric literal: ", text), location);
  }

  if (num_dots > 0) {
    double d;
    if (!absl::SimpleAtod(text, &d)) {
      throw Error(absl::StrCat("Invalid numeric literal: ", text), location);
    }

    return Token{TokenType::kFloatLit, location, {start_position, text.size()}};
  }

  int64_t out;
  if (!absl::SimpleAtoi(text, &out)) {
    throw Error(absl::StrCat("Invalid numeric literal: ", text), location);
  }

  return Token{TokenType::kIntLit, location, {start_position, text.size()}};
}

Token Lexer::LexId() {
  Location location = text_stream_->location();
  const char* start_position = text_stream_->BufPosition();
  size_t length = 0;

  bool done = false;
  while (!done && text_stream_->HasChar()) {
    switch (text_stream_->Peek()) {
      // clang-format off
      case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
      case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
      case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
      case 'V': case 'W': case 'X': case 'Y': case 'Z':

      case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
      case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
      case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
      case 'v': case 'w': case 'x': case 'y': case 'z':
      // clang-format on
      case '_':
        length += 1;
        text_stream_->Inc();
        break;
      default:
        done = true;
        break;
    }
  }

  std::string_view text(start_position, length);
  for (const auto& item : kReservedKeywordToString) {
    if (text == item.str) {
      return Token{item.type, location, text};
    }
  }

  return Token{TokenType::kId, location, text};
}

Token Lexer::LexString() {
  ABSL_ASSERT(text_stream_->Peek() == '"');
  text_stream_->Inc();

  Location location = text_stream_->location();
  const char* start_position = text_stream_->BufPosition();
  size_t length = 0;

  bool next_escape = false;
  while (text_stream_->HasChar()) {
    char cur = text_stream_->Peek();
    if (cur == '"' && !next_escape) {
      text_stream_->Inc();
      return Token{TokenType::kString, location, {start_position, length}};
    }

    if (cur == '\\') {
      next_escape = !next_escape;
    } else {
      next_escape = false;
    }
    text_stream_->Inc();
    length += 1;
  }

  throw Error("missing terminating \" character", location);
}

Token Lexer::LexChar() {
  ABSL_ASSERT(text_stream_->Peek() == '\'');
  text_stream_->Inc();

  Location location = text_stream_->location();
  const char* start_position = text_stream_->BufPosition();
  size_t length = 0;

  bool next_escape = false;
  while (text_stream_->HasChar()) {
    char cur = text_stream_->Peek();
    if (cur == '\'' && !next_escape) {
      return Token{TokenType::kString, location, {start_position, length}};
    }

    if (cur == '\\') {
      next_escape = !next_escape;
    } else {
      next_escape = false;
    }
    length += 1;
  }

  throw Error("missing terminating \" character", location);
}
