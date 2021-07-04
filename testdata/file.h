#include <filesystem>
#include <memory>
#include <string>
#include "absl/status/statusor.h"
#include "bcf/scoped-fd.h"

class File {
 public:
  using Path = std::filesystem::path;
  static absl::StatusOr<cxm::up<File>> Create(const Path& path);

  ~File();

  std::string_view Contents() const {
    return mapping_;
  }

  const Path& path() const {
    return path_;
  }

 private:
  explicit File(
      const Path& path,
      const std::string_view mapping);

  const Path path_;
  const std::string_view mapping_;
};
