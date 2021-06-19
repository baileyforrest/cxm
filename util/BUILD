package(default_visibility = ["//visibility:public"])

cc_library(
    name = "status_util",
    hdrs = ["status_util.h"],
    deps = [
        "@com_google_absl//absl/status",
    ],
)

cc_library(
    name = "status_test",
    testonly = 1,
    hdrs = ["status_test.h"],
)

cc_library(
    name = "util",
    srcs = [
        "error.cc",
        "file.cc",
    ],
    hdrs = [
        "error.h",
        "file.h",
        "location.h",
        "text-stream.h",
    ],
    deps = [
        ":status_util",
        "@bcf//bcf:scoped-fd",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)
