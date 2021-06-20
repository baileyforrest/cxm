package(default_visibility = ["//visibility:public"])

cc_library(
    name = "lex",
    srcs = [
        "lexer.cc",
        "token.cc",
    ],
    hdrs = [
        "lexer.h",
        "token.h",
    ],
    deps = [
        "//cxm/util",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/types:optional",
    ],
)
