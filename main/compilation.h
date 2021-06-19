#pragma once

#include <string>

#include "absl/status/status.h"
#include "absl/strings/string_view.h"

// One instance of compilation associated with a file.
class Compilation {
 public:
  explicit Compilation(absl::string_view file_path);

  absl::Status Run();

 private:
  const std::string file_path_;
};
