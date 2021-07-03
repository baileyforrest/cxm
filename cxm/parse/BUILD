package(default_visibility = ["//visibility:public"])

cc_library(
    name = "parse",
    srcs = [
        "ast.cc",
        "parser.cc",
        "print-ast.cc",
    ],
    hdrs = [
        "ast.h",
        "emit-ast-visitor.h",
        "parser.h",
        "print-ast.h",
    ],
    deps = [
        "//cxm/lex",
        "//cxm/util",
        "@com_google_absl//absl/strings",
    ],
)
