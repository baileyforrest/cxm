#pragma once

#include <string>

#include "absl/status/status.h"
#include "absl/strings/string_view.h"

// One instance of compilation associated with a file.
class Compilation {
 public:
  explicit Compilation(absl::string_view file_path);

  Compilation(const Compilation&) = delete;
  Compilation& operator=(const Compilation&) = delete;

  absl::Status Run();

 private:
  const std::string file_path_;
};
