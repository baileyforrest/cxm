#pragma once

#include <string>

#include "absl/status/status.h"

// One instance of compilation associated with a file.
class Compilation {
 public:
  enum class Type {
    kLex,
    kParse,
    kGen,
  };

  explicit Compilation(std::string_view file_path);

  absl::Status Run(Type type);

 private:
  const std::string file_path_;
};
