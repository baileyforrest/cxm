#include <filesystem>
#include <map>

#include "absl/status/statusor.h"
#include "cxm/gen/code-gen.h"
#include "cxm/lex/lexer.h"
#include "cxm/parse/parser.h"
#include "cxm/parse/print-ast.h"
#include "cxm/util/file.h"
#include "cxm/util/status-test.h"
#include "cxm/util/status-util.h"
#include "cxm/util/text-stream.h"
#include "gtest/gtest.h"

namespace {

namespace fs = std::filesystem;

class GoldensTest : public testing::Test {
 public:
  void VerifyOutput(const fs::path& golden_path, absl::string_view processed) {
    ASSERT_OK_AND_ASSIGN(auto golden_file, File::Create(golden_path));
    EXPECT_EQ(golden_file->Contents(), processed);
  }

  absl::StatusOr<File*> GetInputFile(const fs::path& golden_path) {
    fs::path cxm_path = golden_path;
    if (cxm_path.string().back() == 'h') {
      cxm_path.replace_extension(".cxmh");
    } else {
      cxm_path.replace_extension(".cxm");
    }

    auto it = cxm_path_to_file_.find(cxm_path);
    if (it == cxm_path_to_file_.end()) {
      auto file = BTRY(File::Create(cxm_path));
      it = cxm_path_to_file_.emplace(cxm_path, std::move(file)).first;
    }
    return it->second.get();
  }

  std::map<fs::path, std::unique_ptr<File>> cxm_path_to_file_;
};

TEST_F(GoldensTest, Run) {
  for (const auto& dir_ent : fs::directory_iterator("testdata")) {
    const auto& path = dir_ent.path();

    if (path.filename() == "BUILD") {
      continue;
    }

    // Skip input files.
    if (path.extension() == ".cxm" || path.extension() == ".cxmh") {
      continue;
    }

    SCOPED_TRACE(path);

    ASSERT_OK_AND_ASSIGN(File * input_file, GetInputFile(path));
    TextStream text_stream(input_file->path().filename().c_str(),
                           input_file->Contents());
    Lexer lexer(&text_stream);

    // Lexer test.
    if (path.extension() == ".tokens" || path.extension() == ".tokensh") {
      std::ostringstream oss;
      while (true) {
        Token token = lexer.PopToken();
        if (token.is_eof()) {
          break;
        }

        oss << token << "\n";
      }
      VerifyOutput(path, oss.str());
      continue;
    }

    Parser parser(&lexer);
    ASSERT_OK_AND_ASSIGN(CompilationUnit cu, parser.Parse());

    // Parser test.
    if (path.extension() == ".ast" || path.extension() == ".asth") {
      std::ostringstream oss;
      PrintAst(cu, oss);

      VerifyOutput(path, oss.str());
      continue;
    }

    // Compile test.
    if (path.extension() == ".cc" || path.extension() == ".h") {
      std::ostringstream oss;
      CodeGen gen(&oss);
      gen.Run(cu);
      VerifyOutput(path, oss.str());
      continue;
    }

    ASSERT_TRUE(false) << "Unknown file path: " << path;
  }
}

}  // namespace
