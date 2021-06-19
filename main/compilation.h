#pragma once

#include "absl/strings/string_view.h"
#include "bcf/err.h"

// One instance of compilation associated with a file.
class Compilation {
 public:
  explicit Compilation(absl::string_view file_path);

  Compilation(const Compilation&) = delete;
  Compilation& operator=(const Compilation&) = delete;

  bcf::Result<void> Run();

 private:
  const absl::string_view file_path_;
};
