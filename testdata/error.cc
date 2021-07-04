#include "cxm/util/error.h"
#include <string>
#include "absl/strings/str_format.h"
std::string Error::ToString() const {
  if (!location_.has_value()) {
    return message_;
  }

  auto error = absl::StrFormat(
      "%s:%d:%d: error: %s\n%s\n", location_->file_name, location_->line_number,
      location_->column, message_, location_->line_text);
  for (const auto i : cxm::range(1, location_->column)) {
    error.push_back(' ');
  }

  error.push_back('^');
  return error;
}

