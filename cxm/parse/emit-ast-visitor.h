#pragma once

#include <ostream>
#include <stack>

#include "cxm/parse/ast.h"

class EmitAstVisitor : public AstVisitor {
 public:
  explicit EmitAstVisitor(std::ostream* stream) {
    streams_.push({.stream = stream});
  }

 protected:
  static constexpr int kMaxCol = 80;

  void Indent(int val = 1) { top().indent += val; }
  void DeIndent(int val = 1) { top().indent -= val; }

  template <typename T>
  void Emit(T val) {
    EmitOne(val);
  }

  template <typename T, typename... Args>
  void Emit(T val, Args... args) {
    EmitOne(val);
    Emit(args...);
  }

  void PushStream(std::ostream* stream) { streams_.push({.stream = stream}); }

  void PopStream() { streams_.pop(); }

  int column() const { return top().column; }

 private:
  struct StreamState {
    void Emit(std::string_view text) {
      for (auto c : text) {
        if (c == '\n') {
          indent_next = true;
        } else {
          if (indent_next) {
            column = 1;
            indent_next = false;
            for (int i = 0; i < indent; i += 1) {
              stream->put(' ');
              stream->put(' ');
              column += 2;
            }
          }
        }
        stream->put(c);
        column += 1;
      }
    }

    std::ostream* stream = nullptr;

    bool indent_next = false;
    int indent = 0;
    int column = 1;
  };

  template <typename T>
  void EmitOne(const Rc<T>& node) {
    node->Accept(*this);
  }

  void EmitOne(const Identifier& id) {
    if (id.fully_qualified) {
      Emit("::");
    }
    for (const auto& item : id.namespaces) {
      Emit(item, "::");
    }
    Emit(id.name);
  }

  void EmitOne(std::string_view text) { top().Emit(text); }

  const StreamState& top() const { return streams_.top(); }
  StreamState& top() { return streams_.top(); }

  std::stack<StreamState> streams_;
};
