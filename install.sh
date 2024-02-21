#!/bin/bash

BASEDIR="$(cd "$(dirname "$0")" && pwd)"
echo "$BASEDIR"

if ln --version 2>&1 | grep -q coreutils; then
    LN=ln
elif gln --version 2>&1 | grep -q coreutils; then
    LN=gln
else
    echo "No gnu implementation of ln found."
fi

# For use with gnu coreutils
dot_install_gnu() {
    local target="${BASEDIR}/$1"
    local link="${HOME}/${2:-$1}"
    if [ "$(realpath ${link} 2>/dev/null)" != "${target}" ]; then
        "$LN" --backup=numbered -sv "${target}" "${link}"
    fi
}

set -e

# Check requirements
if [ -z "${HOME}" ]; then
    echo '$HOME is unset'
    exit 1
fi

# Install
dot_install_gnu .gemrc
dot_install_gnu .gitconfig
dot_install_gnu .gitignore
dot_install_gnu .lein
dot_install_gnu .ripgreprc
dot_install_gnu .spacemacs
dot_install_gnu .xinitrc
dot_install_gnu .xmobarrc
dot_install_gnu .xmonad
dot_install_gnu .zprofile
dot_install_gnu .zshalias
dot_install_gnu .zshenv
dot_install_gnu .zshrc
dot_install_gnu .zlogin
dot_install_gnu .doom.d
dot_install_gnu .p10k.zsh
dot_install_gnu .pryrc
mkdir -p .config/kitty
dot_install_gnu kitty.conf .config/kitty/kitty.conf


if [[ -n "$CODESPACES" ]]; then
    git config --global --unset core.pager

    if ! grep -q "$(whoami).*/bin/zsh" /etc/passwd; then
        sudo chsh -s /bin/zsh $(whoami)
    fi

    export TERM=xterm
    # zsh -ils -c -- '@zinit-scheduler burst'
fi
