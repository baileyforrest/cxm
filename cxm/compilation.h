#pragma once

#include <string>

#include "absl/status/status.h"

// One instance of compilation associated with a file.
class Compilation {
 public:
  explicit Compilation(std::string_view file_path);

  absl::Status Run();

 private:
  const std::string file_path_;
};
