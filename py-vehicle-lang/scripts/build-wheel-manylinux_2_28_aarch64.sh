#!/bin/sh

machine="$(python3.11 -c 'import platform; print(platform.machine())')"
if [ "${machine}" = "aarch64" ] || [ "${machine}" = "arm64" ]; then
  manylinux_image="quay.io/pypa/manylinux_2_28_aarch64"
elif [ "${machine}" = "x86_64" ]; then
  manylinux_image="quay.io/pypa/manylinux_2_28_x86_64"
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
"${docker}" run \
  --rm \
  --volume="${package_root}:/vehicle" \
  --workdir="/vehicle/py-vehicle-lang" "${manylinux_image}" \
  /bin/sh -c "sh ./scripts/before-all-almalinux-8.8.sh && pipx run tox"
