#include <exception>
#include <optional>
#include <string>
#include "cxm/util/location.h"

class Error {
 public:
  explicit Error(
      const std::string_view message,
      const std::optional<Location> location = std::nullopt)
      : message_(message), location_(location) {}

  std::string ToString() const;

  const std::optional<Location>& location() const {
    return location_;
  }

 private:
  std::string message_;
  std::optional<Location> location_;
};
