#include <cstdlib>
#include <iostream>

#include "absl/strings/str_format.h"
#include "main/compilation.h"

int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << absl::StrFormat("Usage: %s file_name\n", argv[0]) << "\n";
    return EXIT_FAILURE;
  }

  Compilation compilation(argv[1]);
  absl::Status result = compilation.Run();
  if (!result.ok()) {
    std::cerr << result.message() << "\n";
  }

  return EXIT_SUCCESS;
}
