#pragma once

#include <exception>
#include <optional>
#include <string>

#include "util/location.h"

class Error : public std::exception {
 public:
  Error(std::string_view message,
        std::optional<Location> location = std::nullopt)
      : message_(message), location_(location) {}

  std::string ToString();

  const std::optional<Location>& location() const { return location_; }

 private:
  std::string message_;
  std::optional<Location> location_;
};
