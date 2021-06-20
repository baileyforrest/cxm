#pragma once

#include <filesystem>

#include "util/location.h"

class TextStream {
 public:
  TextStream(std::string_view file_name, std::string_view buf);

  bool HasChar() const;
  char Peek() const;
  void Inc();
  void Dec();

  const char* BufPosition() const { return &buf_[offset_]; }
  const Location& location() const { return location_; }

 private:
  void SetLineText(size_t offset);

  const std::string_view buf_;
  Location location_;
  size_t offset_ = 0;
};

// Implementation:
inline TextStream::TextStream(std::string_view file_name, std::string_view buf)
    : buf_(buf) {
  location_.file_name = file_name;
  location_.line_number = 1;
  location_.column = 1;
  SetLineText(offset_);
}

inline bool TextStream::HasChar() const { return offset_ < buf_.size(); }

inline char TextStream::Peek() const {
  ABSL_ASSERT(HasChar());
  return buf_[offset_];
}

inline void TextStream::Inc() {
  ABSL_ASSERT(HasChar());

  if (ABSL_PREDICT_FALSE(buf_[offset_] == '\n')) {
    location_.line_number += 1;
    location_.column = 0;
    SetLineText(offset_ + 1);
  }

  offset_ += 1;
  location_.column += 1;
}

inline void TextStream::Dec() {
  ABSL_ASSERT(offset_ > 0);
  offset_ -= 1;

  // If we went to a previous line, the counting would be messed up.
  ABSL_ASSERT(buf_[offset_] != '\n');
}

inline void TextStream::SetLineText(size_t offset) {
  size_t line_end = offset + 1;
  while (line_end < buf_.size() && buf_[line_end] != '\n') {
    line_end += 1;
  }

  location_.line_text = buf_.substr(offset, line_end - offset);
}
