# MSYS2

# zsh settings
plugins=(git pylint tmux vagrant)

# standard programs
export BROWSER=\"$(echo "${PROGRAMFILES} (x86)/Mozilla Firefox/firefox.exe"|sed -e 's/\\/\//')\"
export PDFREADER=\"$(echo "${PROGRAMFILES}/SumatraPDF/SumatraPDF.exe"|sed -e 's/\\/\//')\"
export WRITER=\"$(echo "${PROGRAMFILES} (x86)/OpenOffice 4/program/swriter.exe"|sed -e 's/\\/\//')\"

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
export TERM=xterm-256color

# update shortcut
alias update='pacman --color=auto -Syu '
