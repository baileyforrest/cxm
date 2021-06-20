#include <cstdlib>
#include <iostream>
#include "absl/strings/str_format.h"
#include "bcf/logging.h"
#include "main/compilation.h"
int
main(const int argc, char** const argv) {
  if (argc < 2) {
    std::cout << absl::StrFormat("Usage: %s file_name\n", argv[0]);
    return EXIT_FAILURE;
  }

  auto compilation = Compilation(argv[1]);
  const auto result = compilation.Run();
  if (!result.ok()) {
    std::cerr << result.status() << "\n";
  }

  return EXIT_SUCCESS;
}

