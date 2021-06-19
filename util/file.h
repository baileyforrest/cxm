#pragma once

#include <memory>
#include <string>

#include "absl/strings/string_view.h"
#include "bcf/err.h"
#include "bcf/scoped-fd.h"

class File {
 public:
  static bcf::Result<std::unique_ptr<File>> Create(absl::string_view path);

  ~File();
  File(const File&) = delete;
  File& operator=(const File&) = delete;

  absl::string_view Contents() const { return mapping_; }

 public:
  File(std::string path, absl::string_view mapping);
  const std::string path_;
  const absl::string_view mapping_;
};
