#pragma once

#include <string>

struct Location {
  std::string_view file_name;
  std::string_view line_text;
  int line_number = 0;
  int column = 0;
};
