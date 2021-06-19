#pragma once

#include <exception>
#include <string>

#include "util/location.h"

class Error : public std::exception {
 public:
  Error(absl::string_view message,
        absl::optional<Location> location = absl::nullopt)
      : message_(message), location_(location) {}

  std::string ToString();

  const absl::optional<Location>& location() const { return location_; }

 private:
  std::string message_;
  absl::optional<Location> location_;
};
