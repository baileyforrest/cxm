#pragma once

#include <stack>

#include "absl/status/statusor.h"
#include "lex/token.h"
#include "util/status_util.h"
#include "util/text-stream.h"

class Lexer {
 public:
  explicit Lexer(TextStream* text_stream);

  absl::StatusOr<Token> PeekToken();
  absl::StatusOr<Token> PopToken();

  // TODO(bcf): Remove if it's not used.
  void PushToken(Token token);

 private:
  absl::StatusOr<Token> NextToken();
  absl::StatusOr<Token> LexNumber();
  absl::StatusOr<Token> LexId();
  absl::StatusOr<Token> LexString();
  absl::StatusOr<Token> LexChar();

  TextStream* const text_stream_;

  std::stack<Token> token_stack_;
};

// Implementation:
inline absl::StatusOr<Token> Lexer::PeekToken() {
  if (!token_stack_.empty()) {
    return token_stack_.top();
  }

  Token next_token = BTRY(NextToken());
  if (next_token.is_eof()) {
    return next_token;
  }

  token_stack_.push(next_token);

  return next_token;
}

inline absl::StatusOr<Token> Lexer::PopToken() {
  if (token_stack_.empty()) {
    return NextToken();
  }

  Token token = token_stack_.top();
  token_stack_.pop();
  return token;
}

inline void Lexer::PushToken(Token token) { token_stack_.push(token); }
