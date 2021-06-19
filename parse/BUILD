package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse",
    srcs = [
        "ast.cc",
        "parser.cc",
    ],
    hdrs = [
        "ast.h",
        "parser.h",
    ],
    deps = [
        "//lex",
        "//util",
        "@com_google_absl//absl/strings",
    ],
)
