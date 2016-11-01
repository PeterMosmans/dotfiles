# GNU/Linux
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Emacs variables
export REPOSPUB=/var/git
export REPOSPRIV=
export REPOS=/var/git

# Miscellaneous settings
[ "$TERM" = "xterm" ] && export TERM="xterm-256color"

# update shortcut
alias update='sudo apt-get update && sudo apt-get -y dist-upgrade'
