#!/bin/sh

# Get the manylinux image
manylinux_image="${1}"
if [ "${manylinux_image}" = '' ]; then
    echo "Usage: ${0} [manylinux_image]"
    echo
    echo "GHC and Cabal are installed in the container by scripts/before-all-linux.sh."
    echo "Set GHC_VERSION and CABAL_VERSION to override the default versions."
    echo
    echo "manylinux_2_28 (AlmaLinux 8 based)"
    echo
    echo "- quay.io/pypa/manylinux_2_28_x86_64"
    echo "- quay.io/pypa/manylinux_2_28_i686"
    echo "- quay.io/pypa/manylinux_2_28_aarch64"
    echo
    echo "musllinux_1_2 (Alpine based)"
    echo
    echo "- quay.io/pypa/musllinux_1_2_x86_64"
    echo
    echo "See: https://github.com/pypa/manylinux#readme"
    exit 1
fi

# See: https://stackoverflow.com/a/4774063
package_root="$(dirname "$(dirname "$( cd -- "$(dirname "$0")" >/dev/null 2>&1 || exit ; pwd -P )" )" )"

# Check if 'docker' is on the PATH:
docker=$(which docker || echo > "$fail")
if [ -s "$fail" ]; then
    rm "$fail"
    echo "${0} requires 'docker'."
    echo "See: https://docs.docker.com/get-docker/"
    exit 1
fi

# Create a container for the build
"${docker}" run --rm \
    -v "${package_root}:/vehicle" \
    -w "/vehicle/vehicle-python" \
    ${GHC_VERSION:+-e "GHC_VERSION=${GHC_VERSION}"} \
    ${CABAL_VERSION:+-e "CABAL_VERSION=${CABAL_VERSION}"} \
    "${manylinux_image}" \
    sh -c 'sh scripts/before-all-linux.sh && pipx run uv run --extra wheel scripts/build-wheel.sh'
