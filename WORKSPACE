load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "bcf",
    sha256 = "97e5c2fd3d8ac85c9e3771edd9955135ec4180f7d937da1f6eca04e393b5019c",
    strip_prefix = "bcf-5396b3af7da08026354798acc5fbdb970abb8bd7",
    urls = ["https://github.com/baileyforrest/bcf/archive/5396b3af7da08026354798acc5fbdb970abb8bd7.zip"],
)

http_archive(
    name = "com_google_absl",
    strip_prefix = "abseil-cpp-60be12ed9822078970f05f3c560324184302df6b",
    urls = ["https://github.com/abseil/abseil-cpp/archive/60be12ed9822078970f05f3c560324184302df6b.zip"],
)

http_archive(
    name = "rules_cc",
    sha256 = "b295cad8c5899e371dde175079c0a2cdc0151f5127acc92366a8c986beb95c76",
    strip_prefix = "rules_cc-daf6ace7cfeacd6a83e9ff2ed659f416537b6c74",
    urls = ["https://github.com/bazelbuild/rules_cc/archive/daf6ace7cfeacd6a83e9ff2ed659f416537b6c74.zip"],
)

http_archive(
    name = "com_google_googletest",
    sha256 = "05e811175ab0652a4791f2db0fe4496a0dfccbc1cbc9222bc1de995680852aa4",
    strip_prefix = "googletest-7153098229e88295f9655ff1d3b0e2fa9eada5f8",
    urls = ["https://github.com/google/googletest/archive/7153098229e88295f9655ff1d3b0e2fa9eada5f8.zip"],
)

http_archive(
    name = "com_github_google_benchmark",
    strip_prefix = "benchmark-62937f91b5c763a8e119d0c20c67b87bde8eff1c",
    urls = ["https://github.com/google/benchmark/archive/62937f91b5c763a8e119d0c20c67b87bde8eff1c.zip"],
)

# buildifier is written in Go and hence needs rules_go to be built.
# See https://github.com/bazelbuild/rules_go for the up to date setup instructions.
http_archive(
    name = "io_bazel_rules_go",
    sha256 = "d1ffd055969c8f8d431e2d439813e42326961d0942bdf734d2c95dc30c369566",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.24.5/rules_go-v0.24.5.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.24.5/rules_go-v0.24.5.tar.gz",
    ],
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains()

http_archive(
    name = "bazel_gazelle",
    sha256 = "b85f48fa105c4403326e9525ad2b2cc437babaa6e15a3fc0b1dbab0ab064bc7c",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.22.2/bazel-gazelle-v0.22.2.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.22.2/bazel-gazelle-v0.22.2.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

# If you use WORKSPACE.bazel, use the following line instead of the bare gazelle_dependencies():
# gazelle_dependencies(go_repository_default_config = "@//:WORKSPACE.bazel")
gazelle_dependencies()

http_archive(
    name = "com_google_protobuf",
    sha256 = "4c93ac1afd358e8dd530f8c2b01e7f4d39a12631de3cdc554d58e5124b01c4be",
    strip_prefix = "protobuf-aedf4e63169b957dbee439bce3657562fccfd434",
    urls = ["https://github.com/protocolbuffers/protobuf/archive/aedf4e63169b957dbee439bce3657562fccfd434.zip"],
)

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()

http_archive(
    name = "com_github_bazelbuild_buildtools",
    strip_prefix = "buildtools-master",
    url = "https://github.com/bazelbuild/buildtools/archive/master.zip",
)
