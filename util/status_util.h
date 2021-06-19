#pragma once

#include <utility>

#include "absl/status/status.h"

#define RETURN_IF_ERROR(e)       \
  {                              \
    absl::Status __status = (e); \
    if (!__status.ok()) {        \
      return __status;           \
    }                            \
  }                              \
  while (0)

#define BTRY(e)                    \
  ({                               \
    auto __status_or = (e);        \
    if (!__status_or.ok()) {       \
      return __status_or.status(); \
    }                              \
    std::move(*__status_or);       \
  })

inline absl::Status ErrnoToStatus(std::string_view text, int errno_val) {
  return absl::InternalError(absl::StrCat(text, ":", strerror(errno_val)));
}
