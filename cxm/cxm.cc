#include <cstdlib>
#include <iostream>

#include "absl/strings/str_format.h"
#include "cxm/compilation.h"

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << absl::StrFormat("Usage: %s file_name\n", argv[0]) << "\n";
    return EXIT_FAILURE;
  }

  std::string_view path = argv[1];
  auto type = Compilation::Type::kGen;
  if (path == "lex") {
    type = Compilation::Type::kLex;
    path = argv[2];
  } else if (path == "parse") {
    type = Compilation::Type::kParse;
    path = argv[2];
  }

  Compilation compilation(path);
  absl::Status result = compilation.Run(type);
  if (!result.ok()) {
    std::cerr << result.message() << "\n";
  }

  return EXIT_SUCCESS;
}
