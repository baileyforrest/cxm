#include <cstdlib>
#include <iostream>

#include "absl/strings/str_format.h"
#include "bcf/logging.h"
#include "main/compilation.h"

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cout << absl::StrFormat("Usage: %s file_name\n", argv[0]);
    return EXIT_FAILURE;
  }

  Compilation compilation(argv[1]);
  auto result = compilation.Run();
  if (!result.ok()) {
    std::cerr << result.err() << "\n";
  }

  return EXIT_SUCCESS;
}
