package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse",
    srcs = [
        "ast.cc",
        "parser.cc",
        "print_ast.cc",
    ],
    hdrs = [
        "ast.h",
        "parser.h",
        "print_ast.h",
    ],
    deps = [
        "//cxm/lex",
        "//cxm/util",
        "@com_google_absl//absl/strings",
    ],
)
