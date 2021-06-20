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
        "//cxm/lex",
        "//cxm/parse",
        "//cxm/util",
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)
