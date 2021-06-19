#pragma once

#include <string>

#include "absl/status/status.h"
#include "absl/strings/string_view.h"

struct Location {
  absl::string_view file_name;
  absl::string_view line_text;
  int line_number = 0;
  int column = 0;
};

absl::Status MakeError(absl::string_view message, const Location& location);
