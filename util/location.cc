#include "util/location.h"

bcf::Err MakeError(absl::string_view message, const Location& location) {
  std::string error;
  absl::StrAppend(&error, location.file_name, ":", location.line_number, ":",
                  location.column, ": error: ", message, "\n");
  absl::StrAppend(&error, location.line_text, "\n");
  for (int i = 1; i < location.column; ++i) {
    error.push_back(' ');
  }
  error.push_back('^');

  return bcf::Err(error);
}
