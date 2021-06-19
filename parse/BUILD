package(default_visibility = ["//visibility:public"])

cc_library(
    name = "ast",
    srcs = [
        "ast.cc",
    ],
    hdrs = [
        "ast.h",
    ],
    deps = [
        "//lex:token",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "parser",
    srcs = [
        "parser.cc",
    ],
    hdrs = [
        "parser.h",
    ],
    deps = [
        ":ast",
        "//lex:lexer",
        "@com_google_absl//absl/strings",
    ],
)
