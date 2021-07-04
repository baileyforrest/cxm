#pragma once

#include <stack>

#include "cxm/lex/token.h"
#include "cxm/util/error.h"
#include "cxm/util/status-util.h"
#include "cxm/util/text-stream.h"

class Lexer {
 public:
  explicit Lexer(TextStream* text_stream);

  Token PeekToken();
  Token PopToken();

  struct ScopedParseType {
    ScopedParseType(const ScopedParseType&) = delete;
    ScopedParseType& operator=(const ScopedParseType&) = delete;

    ~ScopedParseType() { lexer->parsing_type_count_ -= 1; }

    Lexer* lexer;
  };
  ScopedParseType EnableParseType() {
    parsing_type_count_ += 1;
    return {.lexer = this};
  }

 private:
  Token NextToken();
  Token LexNumber();
  Token LexId();
  Token LexString(char delim, TokenType type);

  TextStream* const text_stream_;

  int parsing_type_count_ = 0;
  std::optional<Token> peek_token_;
};

// Implementation:
inline Token Lexer::PeekToken() {
  if (!peek_token_.has_value()) {
    peek_token_ = NextToken();
  }

  return *peek_token_;
}

inline Token Lexer::PopToken() {
  if (!peek_token_.has_value()) {
    return NextToken();
  }

  Token token = *peek_token_;
  peek_token_ = std::nullopt;
  return token;
}
