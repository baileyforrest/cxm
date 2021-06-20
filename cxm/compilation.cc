#include "cxm/compilation.h"

#include <memory>
#include <vector>

#include "cxm/lex/lexer.h"
#include "cxm/parse/parser.h"
#include "cxm/parse/print_ast.h"
#include "cxm/util/error.h"
#include "cxm/util/file.h"
#include "cxm/util/status-util.h"

Compilation::Compilation(std::string_view file_path) : file_path_(file_path) {}

absl::Status Compilation::Run() {
  auto file = BTRY(File::Create(file_path_));
  TextStream text_stream(file_path_, file->Contents());
  Lexer lexer(&text_stream);
  Parser parser(&lexer);

  CompilationUnit cu = BTRY(parser.Parse());
  PrintAst(cu, std::cout);

  return absl::OkStatus();
}
