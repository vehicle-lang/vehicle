#!/bin/sh

# See: https://stackoverflow.com/a/4774063
ROOT_DIR="$(dirname "$( cd -- "$(dirname "$0")" >/dev/null 2>&1 || exit ; pwd -P )" )"

# Get configuration from Tox
env_python="${env_python:-"python"}"
package_root="${package_root:-"${ROOT_DIR}"}"
dist_dir="${dist_dir:-"${package_root}/dist"}"
dist_tmp_dir="${dist_tmp_dir:-"${package_root}/dist/_tmp"}"

# Build the wheel
"${env_python}" -m build --wheel --outdir "${dist_tmp_dir}" "${package_root}"

# Delocate the wheel
platform=$("${env_python}" -c 'import sys; print(sys.platform)')
case "${platform}" in
  'linux')
    # Repair wheel with auditwheel
    libc_xy="$("${env_python}" -c 'import platform; print(platform.libc_ver()[1].replace(".","_"))')"
    machine="$("${env_python}" -c 'import platform; print(platform.machine())')"
    auditwheel repair --wheel-dir "${dist_dir}" --plat "manylinux_${libc_xy}_${machine}" "${dist_tmp_dir}"/*.whl
    ;;
  'darwin')
    # Repair wheel with delocate
    delocate-wheel --wheel-dir "${dist_dir}" "${dist_tmp_dir}"/*.whl
    ;;
  *)
    cp "${dist_tmp_dir}"/*.whl "${dist_dir}"/
esac
echo "The built wheel is located in ${dist_dir}"
