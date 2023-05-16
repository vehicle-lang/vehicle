#!/bin/sh

# Usage: ./scripts/docker-build-wheels [TARGET] [GIT_REVISION]

# POSIX compliant method for 'pipefail':
fail=$(mktemp)

# See: https://stackoverflow.com/a/4774063
ROOT_DIR="$(dirname "$(dirname "$( cd -- "$(dirname "$0")" >/dev/null 2>&1 || exit ; pwd -P )" )" )"
PY_VEHICLE_LANG_DIR="${ROOT_DIR}/py-vehicle-lang"

# Validate TARGET
TARGET="${1:-jammy}"
DOCKERFILE_ROOT="${PY_VEHICLE_LANG_DIR}/dockerfiles/Dockerfile."
DOCKERFILE="${PY_VEHICLE_LANG_DIR}/dockerfiles/Dockerfile.${TARGET}"
if [ ! -f "${DOCKERFILE}" ]
then
    echo "Usage: ./scripts/docker-build-wheels.sh [TARGET] [GIT_REVISION]"
    echo "Unknown target '${TARGET}'. Supported targets:"
    set -- "${DOCKERFILE_ROOT}"*
    for TARGET
    do
        echo "- ${TARGET#"${DOCKERFILE_ROOT}"}"
    done
    exit 1
fi

# Validate GIT_REVISION
GIT_REVISION="${2:-HEAD}"
if ! git rev-parse --verify "${GIT_REVISION}" >/dev/null
then
    echo "Usage: ./scripts/docker-build-wheels [TARGET] [GIT_REVISION]"
    echo "Unknown Git revision '${GIT_REVISION}'."
    exit 1
fi

# Check if 'git' is on the PATH:
git=$(which git || echo > "$fail")
if [ -s "$fail" ]; then
    rm "$fail"
    echo "The git-build-wheels script requires 'git'."
    echo "See: https://git-scm.com/book/en/v2/Getting-Started-Installing-Git"
    exit 1
fi

# Check if 'docker' is on the PATH:
docker=$(which docker || echo > "$fail")
if [ -s "$fail" ]; then
    rm "$fail"
    echo "./scripts/docker-build-wheels requires 'docker'."
    echo "See: https://docs.docker.com/get-docker/"
    exit 1
fi

# Cache directory
CACHEDIR="${PY_VEHICLE_LANG_DIR}/.docker/${TARGET}/${GIT_REVISION}"
SNAPSHOT="snapshot.tar.gz"

# Generate a snapshot of the repository
mkdir -p "${CACHEDIR}"
(cd "${ROOT_DIR}" && $git archive --format=tar.gz --output="${CACHEDIR}/${SNAPSHOT}" "${GIT_REVISION}")

# Build the container
IMAGE_ID="$(echo "vehicle-python-${TARGET}:${GIT_REVISION}" | tr '[:upper:]' '[:lower:]')"
$docker build --tag "${IMAGE_ID}" --file "${DOCKERFILE}" --build-arg SNAPSHOT="${SNAPSHOT}" "${CACHEDIR}"

# Export the built wheels
CONTAINER_ID="$($docker container create -it "${IMAGE_ID}" )"
$docker container start "${CONTAINER_ID}"
WHEELHOUSE="${PY_VEHICLE_LANG_DIR}/wheelhouse"
mkdir -p "${WHEELHOUSE}"
$docker cp "${CONTAINER_ID}":"/vehicle/py-vehicle-lang/wheelhouse" "${CACHEDIR}/"
cp "${CACHEDIR}/wheelhouse"*".whl" "${WHEELHOUSE}/"
$docker stop "${CONTAINER_ID}"
$docker rm "${CONTAINER_ID}"
