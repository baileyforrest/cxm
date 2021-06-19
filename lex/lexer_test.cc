#include "lex/lexer.h"

#include <fstream>
#include <string>

#include "gtest/gtest.h"
#include "util/file.h"
#include "util/status_test.h"
#include "util/status_util.h"

TEST(LexerTest, Sanity) {
  constexpr char kSrcFile[] = "testdata/cxm.cxm";
  constexpr char kTokensFile[] = "testdata/cxm.tokens";

  ASSERT_OK_AND_ASSIGN(auto expected_tokens, File::Create(kTokensFile));
  ASSERT_OK_AND_ASSIGN(auto file, File::Create(kSrcFile));
  TextStream text_stream(kSrcFile, file->Contents());
  Lexer lexer(&text_stream);

  std::ostringstream oss;
  while (true) {
    Token token = lexer.PopToken();
    if (token.is_eof()) {
      break;
    }

    oss << token << "\n";
  }

  EXPECT_EQ(expected_tokens->Contents(), oss.str());
}
