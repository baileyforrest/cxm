#pragma once

#include <filesystem>
#include <memory>
#include <string>

#include "absl/status/statusor.h"
#include "bcf/scoped-fd.h"

class File {
 public:
  using Path = std::filesystem::path;

  static absl::StatusOr<std::unique_ptr<File>> Create(const Path& path);
  ~File();

  std::string_view Contents() const { return mapping_; }
  const Path& path() const { return path_; }

 public:
  File(const Path& path, std::string_view mapping);

  const Path path_;
  const std::string_view mapping_;
};
