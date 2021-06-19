package(default_visibility = ["//visibility:public"])

cc_library(
    name = "token",
    srcs = [
        "token.cc",
    ],
    hdrs = [
        "token.h",
    ],
    deps = [
        "//util:location",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "lexer",
    srcs = [
        "lexer.cc",
    ],
    hdrs = [
        "lexer.h",
    ],
    deps = [
        ":token",
        "//util:location",
        "//util:status_util",
        "//util:text-stream",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
        "@com_google_absl//absl/types:optional",
    ],
)

cc_test(
    name = "lexer_test",
    srcs = [
        "lexer_test.cc",
    ],
    data = [
        "//testdata:cxm",
    ],
    deps = [
        ":lexer",
        "//util:file",
        "//util:status_test",
        "//util:status_util",
        "@com_google_googletest//:gtest_main",
    ],
)
