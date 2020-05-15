# GNU/Linux
# Always sourced first, regardless of the shell type (interactive or not)

ZSH_TMUX_AUTOSTART=false

# Adding aliases here allows us to use them without needing an interactive shell
alias update="sudo apt-get update -o Dpkg::Progress-Fancy="1" -o APT::Color="1" && sudo apt-get -y upgrade -o Dpkg::Progress-Fancy="1" -o APT::Color="1" && sudo apt-get -y dist-upgrade -o Dpkg::Progress-Fancy="1" -o APT::Color="1" "
