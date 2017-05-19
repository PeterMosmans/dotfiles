# MSYS2
# Always sourced first, regardless of the shell type
# Path to your oh-my-zsh installation.
export ZSH=$HOME/repos/oh-my-zsh

# zsh settings
plugins=(git pylint tmux vagrant)

alias emacs=$EDITOR
export GIT_EDITOR=$EDITOR
# Emacs variables
EMACSVARIABLES="-g 111x60+0+0"

# Hash subdirectories
hash -d masters=${MASTERS}
hash -d kb={KNOWLEDGEBASE}

export temp=/tmp
export tmp=/tmp


