#!/bin/sh

# See: https://stackoverflow.com/a/4774063
ROOT_DIR="$(dirname "$( cd -- "$(dirname "$0")" >/dev/null 2>&1 || exit ; pwd -P )" )"

# Parse arguments:
if [ "${1}" = "--update-goldens" ]
then
    UPDATE_GOLDENS="true"
    rm -rf "${ROOT_DIR}/tests/golden"
elif [ "${1}" = "" ]
then
    UPDATE_GOLDENS="false"
else
    echo "Usage: test_pygments [--update-goldens]"
    exit 1
fi

# POSIX compliant method for 'pipefail':
fail=$(mktemp)

find "${ROOT_DIR}/vendor/vehicle/tests/golden" -name "*.vcl" | \
while read -r file;
do
    input="${file#"${ROOT_DIR}/"}"
    golden="tests/golden/${input#"vendor/vehicle/tests/golden/"}.tokens.golden"
    if [ "${UPDATE_GOLDENS}" = "true" ]
    then
        mkdir -p "$(dirname "${ROOT_DIR}/${golden}")"
        echo "Make ${input}"
        pygmentize -l vehicle -f tokens "${ROOT_DIR}/${input}" -o "${ROOT_DIR}/${golden}"
    elif [ -f "${ROOT_DIR}/${golden}" ]
    then
        output=$(mktemp)
        pygmentize -l vehicle -f tokens "${ROOT_DIR}/${input}" -o "${output}"
        if report=$(diff "${ROOT_DIR}/${golden}" "${output}")
        then
            echo "Pass ${input}"
        else
            echo > "${fail}"
            echo "Fail ${input}"
            echo "${report}"
        fi
    else
        echo "Skip ${input}"
    fi
done

if [ -s "$fail" ]
then
    rm "${fail}"
    exit 1
else
    rm "${fail}"
    exit 0
fi
