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

# Destination where the files need to be installed
DESTINATIONPATH=~
# Files and directories which need to be installed
SOURCEFILES=".aliases .bashrc .emacs.d/init.el .gitconfig .gitignore_global .tmux.conf"
# OS-specificy (uname -o) files and directories
OSSPECIFIC=".ssh/config .zshrc .zshenv"


## Don't change anything below this line
VERSION=0.6

OS=$(uname -o|sed "s/\//-/")
REALPATH=$(dirname $0)
# make sure this doesn't error out when readlink -f isn't available (OSX)
readlink -f $0 &>/dev/null && REALPATH=$(dirname $(readlink -f $0))
mkdir -p ${DESTINATIONPATH} 2>/dev/null

for link in ${SOURCEFILES}; do
    if echo ${link} | grep -q "/" ; then
        mkdir -p "$(dirname ${DESTINATIONPATH}/${link}/)"
    fi
    if [ -f ${REALPATH}/${link} ]; then
        echo hard linking ${link}
        ln --force "${REALPATH}/${link}" "${DESTINATIONPATH}/${link}"
    fi
    if [ -d ${REALPATH}/${link} ]; then
        echo hard linking directory ${link}
        pushd ${link} &>/dev/null
        for i in *; do
            ln --force "${i}" "${DESTINATIONPATH}/${link}/${i}"
        done
        popd &>/dev/null
    fi
done

# operating-system specific
for link in ${OSSPECIFIC}; do
    if [[ -f "${REALPATH}/${OS}/${link}" ]]; then
        echo hard linking ${OS}-specific ${link}
        ln -f "${REALPATH}/${OS}/${link}" "${DESTINATIONPATH}/${link}"
    fi
done
