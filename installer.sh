#!/usr/bin/env bash

# installer - Basic installer (linker) for various files
#
# Copyright (C) 2015-2018 Peter Mosmans
#                         <support AT go-forward.net>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Define associative arrays
declare -A linkfiles
declare -A osfiles

# Files to link
linkfiles=(
    [.]=".aliases .bashrc .functions .gitconfig .gitignore_global .tmux.conf .tmux.default.theme .tmux.dracula.theme"
    [.emacs.d]="init.el"
)

# Files to link, depending on OS
osfiles=(
    [.]=".minttyrc .zshenv .zshrc"
    [.ssh]="config"
)

## Don't change anything below this line
VERSION=0.7

os=$(uname -o|sed "s/\//-/")
source=$(dirname $(readlink -f $0))
target=$1

# Check whether the script is being executed from within the source directory
if [ -z "$target" ]; then
    target=$(readlink -f .)
     if [ "${target}" == "${source}" ]; then
        echo "Usage: installer.sh [TARGET]"
        echo "       or run from within target directory"
        exit
    fi
fi

echo "[*] Installing..."

# Link files
for targetdirectory in "${!linkfiles[@]}"; do
    mkdir -p ${target}/${targetdirectory}/ &>/dev/null
    for file in ${linkfiles[$targetdirectory]}; do
        ln -fv ${source}/${targetdirectory}/${file} ${target}/${targetdirectory}/${file}
    done
done

# Link operating-system specific files
for targetdirectory in "${!osfiles[@]}"; do
    for file in ${osfiles[$targetdirectory]}; do
        if [[ -f ${source}/${os}/${file} ]]; then
            ln -fv ${source}/${os}/${targetdirectory}/${file} ${target}/${targetdirectory}/${file}
        fi
    done
done

echo "[+] Done"
