#include "util/location.h"

#include "absl/strings/str_format.h"

absl::Status MakeError(absl::string_view message, const Location& location) {
  std::string error = absl::StrFormat(
      "%s:%d:%d: error: %s\n%s\n", location.file_name, location.line_number,
      location.column, message, location.line_text);
  for (int i = 1; i < location.column; ++i) {
    error.push_back(' ');
  }
  error.push_back('^');

  return absl::InvalidArgumentError(error);
}
