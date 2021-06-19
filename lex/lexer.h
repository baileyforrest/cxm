#pragma once

#include <stack>

#include "absl/types/optional.h"
#include "bcf/err.h"
#include "lex/token.h"
#include "util/text-stream.h"

class Lexer {
 public:
  explicit Lexer(TextStream* text_stream);

  Lexer(const Lexer&) = delete;
  Lexer& operator=(const Lexer&) = delete;

  bcf::Result<absl::optional<Token>> PeekToken();
  bcf::Result<absl::optional<Token>> PopToken();

  // TODO(bcf): Remove if it's not used.
  void PushToken(Token token);

 private:
  bcf::Result<absl::optional<Token>> NextToken();
  bcf::Result<Token> LexNumber();
  bcf::Result<Token> LexId();
  bcf::Result<Token> LexString();
  bcf::Result<Token> LexChar();

  std::stack<Token> push_token_stack_;

  TextStream* const text_stream_;
};

// Implementation:
inline bcf::Result<absl::optional<Token>> Lexer::PeekToken() {
  if (!push_token_stack_.empty()) {
    return {push_token_stack_.top()};
  }

  absl::optional<Token> maybe_token = TRY(NextToken());
  if (!maybe_token) {
    return {absl::nullopt};
  }

  push_token_stack_.push(*maybe_token);

  return maybe_token;
}

inline bcf::Result<absl::optional<Token>> Lexer::PopToken() {
  if (push_token_stack_.empty()) {
    return NextToken();
  }

  Token token = push_token_stack_.top();
  push_token_stack_.pop();
  return {token};
}

inline void Lexer::PushToken(Token token) { push_token_stack_.push(token); }
