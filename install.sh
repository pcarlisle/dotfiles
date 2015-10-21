#!/bin/sh

BASEDIR="$(cd "$(dirname "$0")" && pwd)"
echo "$BASEDIR"

if $(ln --version 2>&1 | grep -q coreutils); then
    LN=ln
elif $(gln --version 2>&1 | grep -q coreutils); then
    LN=gln
else
    echo "No gnu implementation of ln found."
fi

# For use with gnu coreutils
dot_install_gnu() {
    "$LN" --backup=numbered -sv "${BASEDIR}/$1" "${HOME}/$1"
}

set -e

# Check requirements
if [ -z "${HOME}" ]; then
    echo '$HOME is unset'
    exit 1
fi

# Install
dot_install_gnu .gitignore
dot_install_gnu .spacemacs
dot_install_gnu .xmonad

# TODO: Install spacemacs
