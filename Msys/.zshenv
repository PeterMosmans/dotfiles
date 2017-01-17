# MSYS2

# Path to your oh-my-zsh installation.
export ZSH=$HOME/repos/oh-my-zsh

# zsh settings
plugins=(git pylint tmux vagrant)

# standard programs
export BROWSER=\"$(echo "${PROGRAMFILES} (x86)/Mozilla Firefox/firefox.exe"|sed -e 's/\\/\//')\"
export PDFREADER=\"$(echo "${PROGRAMFILES}/SumatraPDF/SumatraPDF.exe"|sed -e 's/\\/\//')\"
export WRITER=\"$(echo "${PROGRAMFILES} (x86)/LibreOffice 5/program/swriter.exe"|sed -e 's/\\/\//')\"
export CALC=\"$(echo "${PROGRAMFILES} (x86)/LibreOffice 5/program/scalc.exe"|sed -e 's/\\/\//')\"
# Add some typical Windows suffix aliases
alias -s bat="c:/windows/system32/cmd.exe" '/c'
alias -s cmd="c:/windows/system32/cmd.exe" '/c'
alias emacs=$EDITOR
export GIT_EDITOR=$EDITOR
# Emacs variables
EMACSVARIABLES="-g 111x60+0+0"

# Hash subdirectories
hash -d masters=${MASTERS}
hash -d kb={KNOWLEDGEBASE}

export temp=/tmp
export tmp=/tmp

if [[ "$TERM" == "emacs" ]]; then
    # Make sure that shell-mode properly works with Emacs
    export TERM=eterm-color
fi

# update shortcut
alias update='pacman --color=auto -Syuu '
