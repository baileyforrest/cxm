package(default_visibility = ["//visibility:public"])

cc_library(
    name = "file",
    srcs = [
        "file.cc",
    ],
    hdrs = [
        "file.h",
    ],
    deps = [
        "@bcf//bcf:err",
        "@bcf//bcf:scoped-fd",
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
        "@bcf//bcf:err",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "text-stream",
    hdrs = [
        "text-stream.h",
    ],
    deps = [
        "@bcf//bcf:err",
        "@com_google_absl//absl/strings",
    ],
)
