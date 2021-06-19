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
    name = "file",
    srcs = [
        "file.cc",
    ],
    hdrs = [
        "file.h",
    ],
    deps = [
        ":status_util",
        "@bcf//bcf:scoped-fd",
        "@com_google_absl//absl/status:statusor",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "location",
    srcs = [
        "location.cc",
    ],
    hdrs = [
        "location.h",
    ],
    deps = [
        "@com_google_absl//absl/status",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "text-stream",
    hdrs = [
        "text-stream.h",
    ],
    deps = [
        "@com_google_absl//absl/strings",
    ],
)
