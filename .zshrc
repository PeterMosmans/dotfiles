# Path to your oh-my-zsh installation.
export ZSH=$HOME/repos/oh-my-zsh
export NCURSES_NO_UTF8_ACS=1

# Check for and load terminal-specific keybindings
[ -f ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE} ] && source ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE}

# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#History-Control

[[ -n ${key[Left]} ]] && bindkey "${key[Left]}" backward-char
[[ -n ${key[Right]} ]] && bindkey "${key[Right]}" forward-char
[[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
[[ -n ${key[Up]} ]] && bindkey "${key[Up]}" up-line-or-history
[[ -n ${key[Down]} ]] && bindkey "${key[Down]}" down-line-or-history

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="compact-grey"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

export ZSH_TMUX_AUTOSTART=true
export ZSH_TMUX_UNICODE=true

# default plugins - is overruled by .zshenv
[ -z "$plugins" ] && plugins=(git)

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

[ -f $ZSH/oh-my-zsh.sh ] && source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

## global aliases
# case insensitive matches, recursive, show filename
alias -g GW="grep -iHR"
# case insensitive matches, recursive, filename only
alias -g GF="grep -ilR"
# redirect / pipe output"
alias -g N=">/dev/null"
alias -g L="|less"
alias -g M="|more"

# Add some generic suffix aliases
alias -s txt=emacs
alias -s xml=emacs
[ ! -z ${BROWSER} ] && alias -s htm=${BROWSER}
[ ! -z ${BROWSER} ] && alias -s html=${BROWSER}
[ ! -z ${WRITER} ] && alias -s odt=${WRITER}
[ ! -z ${PDFREADER} ] && alias -s pdf=${PDFREADER}

show_colors() {
    for i in {0..255} ; do
        let j=255-$i
        printf "\x1b[38;5;${i}m ${i} \x1b[0m"
        printf "\x1b[48;5;${i}m\x1b[38;5;${j}m ${i} \x1b[0m"
    done
    printf "\n"
}

urlencode() {
    setopt localoptions extendedglob
    input=( ${(s::)1} )
    print ${(j::)input/(#b)([^A-Za-z0-9_.\!~*\'\(\)-])/%${(l:2::0:)$(([##16]#match))}}
}

# Using the AUTOCD option, you can simply type the name of a directory,
# and it will become the current directory. 
setopt AUTOCD

## History
# + HIST_VERIFY :: don't execute history command immediately
setopt HIST_VERIFY
# + HIST_IGNORE_SPACE :: don't add command to history if it starts with space
setopt HIST_IGNORE_SPACE
# + HIST_IGNORE_ALL_DUPS :: ignore duplicate entries when showing results
setopt HIST_IGNORE_ALL_DUPS
# + HIST_FIND_NO_DUPS :: ignore duplicates when match has been found
setopt HIST_FIND_NO_DUPS
# + INC_APPEND_HISTORY :: adds entries to history as they are typed (don't wait for exit)
setopt INC_APPEND_HISTORY
# + SHARE_HISTORY :: share history between different zsh processes
# setopt SHARE_HISTORY

## Line editor
setopt NO_BEEP

# create hashes for all subdirectories / repositories
for sub in ${REPOSPUB} ${REPOSPRIV}; do
    if [ -d ${sub} ]; then
        pushd ${sub} &>/dev/null
        for repo in echo *(/); do
            hash -d ${repo}=${sub}/${repo}
        done
        popd &>/dev/null
    fi
done

[ ! -z ${REPOS} ] && [ -d ${REPOS} ] && hash -d repos=${REPOS} || true

# load Bash and zsh compatible aliases
[ -f $HOME/.aliases ] && source $HOME/.aliases
