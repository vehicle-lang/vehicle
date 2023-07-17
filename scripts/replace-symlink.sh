#!/bin/sh
TARGET="$1"
if [ -L "$TARGET" ]; then
    SOURCE="$(realpath "$(dirname "$TARGET")/$(readlink "$TARGET")")"
    rm -rf "$TARGET"
    cp -Rf "$SOURCE" "$TARGET"
fi
