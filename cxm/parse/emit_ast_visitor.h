#pragma once

#include <ostream>

#include "cxm/parse/ast.h"

class EmitAstVisitor : public AstVisitor {
 public:
  explicit EmitAstVisitor(std::ostream* ostream) : ostream_(*ostream) {}

 protected:
  void Indent(int val = 1) { indent_ += val; }
  void DeIndent(int val = 1) { indent_ -= val; }

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

  void EmitOne(std::string_view text) {
    for (auto c : text) {
      if (c == '\n') {
        indent_next_ = true;
      } else {
        if (indent_next_) {
          indent_next_ = false;
          for (int i = 0; i < indent_; i += 1) {
            ostream_.put(' ');
            ostream_.put(' ');
          }
        }
      }
      ostream_.put(c);
    }
  }

  template <typename T>
  void Emit(T val) {
    EmitOne(val);
  }

  template <typename T, typename... Args>
  void Emit(T val, Args... args) {
    EmitOne(val);
    Emit(args...);
  }

 private:
  std::ostream& ostream_;
  bool indent_next_ = false;
  int indent_ = 0;
};
