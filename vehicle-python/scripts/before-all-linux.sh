#!/bin/sh

# Install GHC and Cabal inside a stock manylinux or musllinux container.
#
# Used as cibuildwheel's `before-all` step on Linux (see pyproject.toml) and by
# `scripts/docker-build-wheel.sh`.
#
# The versions are taken from the GHC_VERSION and CABAL_VERSION environment
# variables, which CI passes through from `.github/workflows/build-vehicle-python.yml`.
# The defaults below are only used for local builds and should be kept in sync
# with that workflow.

set -eu

GHC_VERSION="${GHC_VERSION:-9.12.2}"
CABAL_VERSION="${CABAL_VERSION:-3.14.2.0}"

# Install the system libraries required by the GHC binary distributions.
if command -v apk >/dev/null 2>&1; then
    # musllinux (Alpine)
    apk add --no-cache \
        curl gcc g++ make perl tar xz \
        gmp-dev libffi-dev ncurses-dev numactl-dev zlib-dev
elif command -v dnf >/dev/null 2>&1; then
    # manylinux_2_28 (AlmaLinux 8)
    dnf install -y \
        gmp-devel libffi-devel ncurses-devel numactl-devel zlib-devel xz perl
elif command -v yum >/dev/null 2>&1; then
    # manylinux2014 (CentOS 7)
    yum install -y \
        gmp-devel libffi-devel ncurses-devel numactl-devel zlib-devel xz perl
else
    echo "${0}: unsupported container: no apk, dnf, or yum found" >&2
    exit 1
fi

# Download ghcup. The ghcup binaries are statically linked, so the same binary
# works on both glibc- and musl-based images.
case "$(uname -m)" in
    x86_64) ghcup_arch="x86_64" ;;
    aarch64 | arm64) ghcup_arch="aarch64" ;;
    i386 | i686) ghcup_arch="i386" ;;
    *)
        echo "${0}: unsupported architecture: $(uname -m)" >&2
        exit 1
        ;;
esac

export GHCUP_INSTALL_BASE_PREFIX="${GHCUP_INSTALL_BASE_PREFIX:-/opt}"
ghcup_bin_dir="${GHCUP_INSTALL_BASE_PREFIX}/.ghcup/bin"
mkdir -p "${ghcup_bin_dir}"
curl --proto '=https' --tlsv1.2 -sSfL \
    "https://downloads.haskell.org/ghcup/${ghcup_arch}-linux-ghcup" \
    -o "${ghcup_bin_dir}/ghcup"
chmod +x "${ghcup_bin_dir}/ghcup"

# Install GHC and Cabal.
"${ghcup_bin_dir}/ghcup" install ghc "${GHC_VERSION}" --set
"${ghcup_bin_dir}/ghcup" install cabal "${CABAL_VERSION}" --set
"${ghcup_bin_dir}/ghcup" gc --cache

# Put the tools on the default PATH, so that later cibuildwheel steps (which
# start new shells) can find them without extra configuration.
for tool in "${ghcup_bin_dir}"/*; do
    ln -sf "${tool}" "/usr/local/bin/$(basename "${tool}")"
done

ghc --version
cabal --version

# Update the Hackage package index.
cabal update
