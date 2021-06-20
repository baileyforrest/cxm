#include "main/compilation.h"

#include <memory>
#include <vector>

#include "lex/lexer.h"
#include "parse/parser.h"
#include "util/error.h"
#include "util/file.h"
#include "util/status_util.h"

Compilation::Compilation(std::string_view file_path) : file_path_(file_path) {}

absl::Status Compilation::Run() {
  auto file = BTRY(File::Create(file_path_));
  TextStream text_stream(file_path_, file->Contents());
  Lexer lexer(&text_stream);
  Parser parser(&lexer);

  std::vector<Rc<GlobalDecl>> decls = BTRY(parser.Parse());
  for (const auto& decl : decls) {
    AstStringBuilder string_builder;
    decl->AppendString(&string_builder);

    std::cout << string_builder.str() << "\n";
  }

  return absl::OkStatus();
}
