#pragma once

#include <stack>

#include "absl/status/statusor.h"
#include "absl/types/optional.h"
#include "lex/token.h"
#include "util/status_util.h"
#include "util/text-stream.h"

class Lexer {
 public:
  explicit Lexer(TextStream* text_stream);

  Lexer(const Lexer&) = delete;
  Lexer& operator=(const Lexer&) = delete;

  absl::StatusOr<absl::optional<Token>> PeekToken();
  absl::StatusOr<absl::optional<Token>> PopToken();

  // TODO(bcf): Remove if it's not used.
  void PushToken(Token token);

 private:
  absl::StatusOr<absl::optional<Token>> NextToken();
  absl::StatusOr<Token> LexNumber();
  absl::StatusOr<Token> LexId();
  absl::StatusOr<Token> LexString();
  absl::StatusOr<Token> LexChar();

  std::stack<Token> push_token_stack_;

  TextStream* const text_stream_;
};

// Implementation:
inline absl::StatusOr<absl::optional<Token>> Lexer::PeekToken() {
  if (!push_token_stack_.empty()) {
    return {push_token_stack_.top()};
  }

  absl::optional<Token> maybe_token = BTRY(NextToken());
  if (!maybe_token) {
    return {absl::nullopt};
  }

  push_token_stack_.push(*maybe_token);

  return maybe_token;
}

inline absl::StatusOr<absl::optional<Token>> Lexer::PopToken() {
  if (push_token_stack_.empty()) {
    return NextToken();
  }

  Token token = push_token_stack_.top();
  push_token_stack_.pop();
  return {token};
}

inline void Lexer::PushToken(Token token) { push_token_stack_.push(token); }
