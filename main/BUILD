package(default_visibility = ["//visibility:public"])

cc_binary(
    name = "cxm",
    srcs = [
        "cxm.cc",
    ],
    deps = [
        ":compilation",
        "@bcf//bcf:logging",
    ],
)

cc_library(
    name = "compilation",
    srcs = [
        "compilation.cc",
    ],
    hdrs = [
        "compilation.h",
    ],
    deps = [
        "//lex:lexer",
        "//util:file",
        "@bcf//bcf:err",
        "@bcf//bcf:logging",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)
