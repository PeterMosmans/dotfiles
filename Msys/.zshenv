# MSYS2
# Always sourced first, regardless of whether the shell is interactive or not

export temp=/tmp
export tmp=/tmp

# Make sure that Python uses UTF-8 encoding for stdout / stderr
export PYTHONIOENCODING=UTF-8
export GOROOT=/mingw64/lib
export GOPATH=c:/source/private/go
export WNHOME=/mingw64

alias update="pacman --color=auto -Syudd"
chcp 65001 > /dev/null  # Change from 1252 (default) to UTF-8 codepage for output

# Interactive alias bindings
export EDITOR=emacs.sh
export BROWSER='c:/Program\ Files/Mozilla\ Firefox/firefox.exe'
export READER="c:/Program\ Files/SumatraPDF/SumatraPDF.exe"
