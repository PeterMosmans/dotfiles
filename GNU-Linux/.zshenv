# GNU/Linux
# Always sourced first, regardless of the shell type (interactive or not)

# zsh settings
plugins=(git)

export REPOSPUB=/var/git
export REPOSPRIV=
export REPOS=/var/git

# OS-specific update shortcut
alias update='sudo apt-get update && sudo apt-get -y dist-upgrade'
