#!/bin/sh

# Get the manylinux image
manylinux_image="${1}"
if [ "${manylinux_image}" = '' ]; then
    echo "Usage: ${0} [manylinux_image]"
    echo
    echo "manylinux_2_28 (AlmaLinux 8 based)"
    echo "Toolchain: GCC 12"
    echo
    echo "- quay.io/pypa/manylinux_2_28_x86_64"
    echo "- quay.io/pypa/manylinux_2_28_aarch64"
    echo "- quay.io/pypa/manylinux_2_28_ppc64le"
    echo "- quay.io/pypa/manylinux_2_28_s390x"
    echo
    echo "manylinux2014 (CentOS 7 based)"
    echo "Toolchain: GCC 10"
    echo
    echo "- quay.io/pypa/manylinux2014_x86_64"
    echo "- quay.io/pypa/manylinux2014_i686"
    echo "- quay.io/pypa/manylinux2014_aarch64"
    echo "- quay.io/pypa/manylinux2014_ppc64le"
    echo "- quay.io/pypa/manylinux2014_s390x"
    echo
    echo "See: https://github.com/pypa/manylinux#readme"
    exit 1
fi

# See: https://stackoverflow.com/a/4774063
package_root="$(dirname "$( cd -- "$(dirname "$0")" >/dev/null 2>&1 || exit ; pwd -P )" )"
package_name="$(basename "${package_root}")"

# Check if 'docker' is on the PATH:
docker=$(which docker || echo > "$fail")
if [ -s "$fail" ]; then
    rm "$fail"
    echo "${0} requires 'docker'."
    echo "See: https://docs.docker.com/get-docker/"
    exit 1
fi

# Create a container for the build
"${docker}" run --rm -v "${package_root}:/${package_name}" -w "/${package_name}" "${manylinux_image}" \
/bin/sh -c '. /etc/os-release && sh ./scripts/before-all-$ID-$VERSION_ID.sh && pipx run tox'
