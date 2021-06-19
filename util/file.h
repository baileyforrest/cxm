#pragma once

#include <memory>
#include <string>

#include "absl/status/statusor.h"
#include "bcf/scoped-fd.h"

class File {
 public:
  static absl::StatusOr<std::unique_ptr<File>> Create(const std::string& path);
  ~File();

  std::string_view Contents() const { return mapping_; }

 public:
  File(std::string path, std::string_view mapping);
  const std::string path_;
  const std::string_view mapping_;
};
