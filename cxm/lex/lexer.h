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

  // TODO(bcf): Remove if it's not used.
  void PushToken(Token token);

 private:
  Token NextToken();
  Token LexNumber();
  Token LexId();
  Token LexString();
  Token LexChar();

  TextStream* const text_stream_;

  std::stack<Token> token_stack_;
};

// Implementation:
inline Token Lexer::PeekToken() {
  if (!token_stack_.empty()) {
    return token_stack_.top();
  }

  Token next_token = NextToken();
  if (next_token.is_eof()) {
    return next_token;
  }

  token_stack_.push(next_token);

  return next_token;
}

inline Token Lexer::PopToken() {
  if (token_stack_.empty()) {
    return NextToken();
  }

  Token token = token_stack_.top();
  token_stack_.pop();
  return token;
}

inline void Lexer::PushToken(Token token) { token_stack_.push(token); }
