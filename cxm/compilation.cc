#include "cxm/compilation.h"

#include <iostream>
#include <memory>
#include <vector>

#include "cxm/gen/code-gen.h"
#include "cxm/lex/lexer.h"
#include "cxm/parse/parser.h"
#include "cxm/parse/print-ast.h"
#include "cxm/util/error.h"
#include "cxm/util/file.h"
#include "cxm/util/status-util.h"

Compilation::Compilation(std::string_view file_path) : file_path_(file_path) {}

absl::Status Compilation::Run(Type type) {
  auto file = BTRY(File::Create(file_path_));
  TextStream text_stream(file_path_, file->Contents());
  Lexer lexer(&text_stream);
  if (type == Type::kLex) {
    while (true) {
      Token token = lexer.PopToken();
      if (token.is_eof()) {
        return absl::OkStatus();
      }

      std::cout << token << "\n";
    }
  }

  Parser parser(&lexer);
  CompilationUnit cu = BTRY(parser.Parse());
  if (type == Type::kParse) {
    PrintAst(cu, std::cout);
    return absl::OkStatus();
  }

  CodeGen gen(&std::cout);
  gen.Run(cu);

  return absl::OkStatus();
}
