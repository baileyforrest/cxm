#include "lex/lexer.h"

#include <fstream>
#include <string>

#include "bcf/err-testing.h"
#include "gtest/gtest.h"
#include "util/file.h"

namespace {

bcf::Result<std::string> ReadFileToString(const std::string& path) {
  std::ifstream ifs(path);
  std::ostringstream oss;
  oss << ifs.rdbuf();
  return oss.str();
}

}  // namespace

TEST(LexerTest, Sanity) {
  static constexpr char kSrcFile[] = "testdata/cxm.cxm";
  static constexpr char kTokensFile[] = "testdata/cxm.tokens";

  ASSERT_OK_AND_ASSIGN(std::string expected_tokens,
                       ReadFileToString(kTokensFile));

  ASSERT_OK_AND_ASSIGN(auto file, File::Create(kSrcFile));
  TextStream text_stream(kSrcFile, file->Contents());
  Lexer lexer(&text_stream);

  std::ostringstream oss;
  while (true) {
    ASSERT_OK_AND_ASSIGN(absl::optional<Token> token, lexer.PopToken());
    if (!token) {
      break;
    }

    oss << *token << "\n";
  }

  EXPECT_EQ(expected_tokens, oss.str());
}
