#pragma once

#include <utility>

#define ASSERT_OK_AND_ASSIGN(dest, expr)             \
  dest = ({                                          \
    auto __result = (expr);                          \
    ASSERT_TRUE(__result.ok()) << __result.status(); \
    std::move(*__result);                            \
  });
