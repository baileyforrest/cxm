#pragma once

#include <utility>

#define ASSERT_OK_AND_ASSIGN(dest, expr)         \
  dest = ({                                      \
    auto result = (expr);                        \
    ASSERT_TRUE(result.ok()) << result.status(); \
    std::move(*result);                          \
  });
