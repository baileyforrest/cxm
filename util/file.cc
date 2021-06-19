#include "util/file.h"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <utility>

#include "absl/memory/memory.h"

File::~File() { munmap(const_cast<char*>(mapping_.data()), mapping_.size()); }

bcf::Result<std::unique_ptr<File>> File::Create(absl::string_view path) {
  std::string str_path(path);
  bcf::ScopedFd fd(open(str_path.c_str(), /*flags=*/0));
  if (!fd) {
    return bcf::BuildPosixErr(absl::StrCat("Failed to open ", path));
  }

  struct stat stat;
  if (fstat(*fd, &stat) < 0) {
    return bcf::BuildPosixErr(absl::StrCat("Failed to stat ", path));
  }

  void* mapping =
      mmap(nullptr, stat.st_size, PROT_READ, MAP_PRIVATE, *fd, /*offset=*/0);
  if (mapping == MAP_FAILED) {
    return bcf::BuildPosixErr(absl::StrCat("Failed to mmap ", path));
  }

  absl::string_view str_mapping(static_cast<char*>(mapping), stat.st_size);

  return absl::WrapUnique(new File(std::move(str_path), str_mapping));
}

File::File(std::string path, absl::string_view mapping)
    : path_(std::move(path)), mapping_(mapping) {}
