package(default_visibility = ["//visibility:public"])

cc_library(
    name = "status-util",
    hdrs = ["status-util.h"],
    deps = [
        "@com_google_absl//absl/status",
    ],
)

cc_library(
    name = "status-test",
    testonly = 1,
    hdrs = ["status-test.h"],
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
        "rc.h",
        "text-stream.h",
    ],
    deps = [
        ":status-util",
        "@bcf//bcf:scoped-fd",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
        "@com_google_absl//absl/strings:str_format",
    ],
)
