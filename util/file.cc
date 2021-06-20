#include "util/file.h"

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <utility>

#include "util/status-util.h"

File::~File() { munmap(const_cast<char*>(mapping_.data()), mapping_.size()); }

absl::StatusOr<std::unique_ptr<File>> File::Create(const Path& path) {
  bcf::ScopedFd fd(open(path.c_str(), /*flags=*/0));
  if (!fd) {
    return ErrnoToStatus(absl::StrCat("Failed to open ", path.c_str()), errno);
  }

  struct stat stat;
  if (fstat(*fd, &stat) < 0) {
    return ErrnoToStatus(absl::StrCat("Failed to stat ", path.c_str()), errno);
  }

  void* mapping =
      mmap(nullptr, stat.st_size, PROT_READ, MAP_PRIVATE, *fd, /*offset=*/0);
  if (mapping == MAP_FAILED) {
    return ErrnoToStatus(absl::StrCat("Failed to mmap ", path.c_str()), errno);
  }

  std::string_view str_mapping(static_cast<char*>(mapping), stat.st_size);

  return absl::WrapUnique(new File(path, str_mapping));
}

File::File(const Path& path, std::string_view mapping)
    : path_(path), mapping_(mapping) {}
