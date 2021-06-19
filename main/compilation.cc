#include "main/compilation.h"

#include "bcf/logging.h"
#include "lex/lexer.h"
#include "util/file.h"

Compilation::Compilation(absl::string_view file_path) : file_path_(file_path) {}

bcf::Result<void> Compilation::Run() {
  auto file = TRY(File::Create(file_path_));
  TextStream text_stream(file_path_, file->Contents());
  Lexer lexer(&text_stream);

  while (true) {
    absl::optional<Token> token = TRY(lexer.PopToken());
    if (!token) {
      break;
    }

    std::cout << *token << "\n";
  }

  return {};
}
