#include "util/error.h"

#include <string>

#include "absl/strings/str_format.h"

std::string Error::ToString() {
  if (!location_.has_value()) {
    return message_;
  }
  std::string error = absl::StrFormat(
      "%s:%d:%d: error: %s\n%s\n", location_->file_name, location_->line_number,
      location_->column, message_, location_->line_text);
  for (int i = 1; i < location_->column; ++i) {
    error.push_back(' ');
  }
  error.push_back('^');

  return error;
}
