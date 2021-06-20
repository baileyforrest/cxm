#include <filesystem>
#include <map>

#include "absl/status/statusor.h"
#include "gtest/gtest.h"
#include "lex/lexer.h"
#include "util/file.h"
#include "util/status_test.h"
#include "util/status_util.h"
#include "util/text-stream.h"

namespace {

namespace fs = std::filesystem;

class GoldensTest : public testing::Test {
 public:
  void VerifyOutput(const fs::path& golden_path, absl::string_view processed) {
    ASSERT_OK_AND_ASSIGN(auto golden_file, File::Create(golden_path));
    EXPECT_EQ(processed, golden_file->Contents());
  }

  absl::StatusOr<File*> GetInputFile(const fs::path& golden_path) {
    fs::path cxm_path = golden_path;
    cxm_path.replace_extension(".cxm");

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
    if (path.extension() == ".cxm") {
      continue;
    }

    ASSERT_OK_AND_ASSIGN(File * input_file, GetInputFile(path));

    // Lexer test.
    if (path.extension() == ".tokens") {
      TextStream text_stream(input_file->path().filename().c_str(),
                             input_file->Contents());
      Lexer lexer(&text_stream);

      std::ostringstream oss;
      while (true) {
        Token token = lexer.PopToken();
        if (token.is_eof()) {
          break;
        }

        oss << token << "\n";
      }
      VerifyOutput(path, oss.str());
    }
  }
}

}  // namespace