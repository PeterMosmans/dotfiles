# MSYS2
# Always sourced first, regardless of whether the shell is interactive or not
# Path to your oh-my-zsh installation.

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

# OS-specific update shortcuts (note that these should actually be placed
# in the .rc file, instead of in the environment file)
alias emacsbuild='pushd ~emacs && gl && pushd ~emacs-builddir && make'
alias emacsinstall='rm -rf c:/programs/emacs-previous && mv c:/programs/emacs c:/programs/emacs-previous && pushd ~emacs-builddir && make install-strip prefix=c:/programs/emacs && popd'
export UPDATE="pacman --color=auto -Syuu"

# Interactive alias bindings
export BROWSER="c:/Program Files (x86)/Mozilla Firefox/firefox.exe"
export READER="c:/Program Files/SumatraPDF/SumatraPDF.exe"
