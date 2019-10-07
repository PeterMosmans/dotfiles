# OS-independent
# Only sourced for interactive shell, last

export ZPLUG_HOME=~/.zplug
ZSH_TMUX_AUTOSTART=true
export TMUX_THEME="dracula"
export GIT_EDITOR=$EDITOR
HISTFILE=~/.zsh_history
# Tell ncurses to use UTF-8
export NCURSES_NO_UTF8_ACS=1

# OS-specific update shortcuts
export UPDATE="pacman --color=auto -Syudd"

# Interactive alias bindings
export BROWSER='c:/Program\ Files/Mozilla\ Firefox/firefox.exe'
export READER="c:/Program\ Files/SumatraPDF/SumatraPDF.exe"


# Check for and load terminal-specific keybindings
[ -f ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE} ] && source ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE}
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#History-Control
[[ -n ${key[Left]} ]] && bindkey "${key[Left]}" backward-char
[[ -n ${key[Right]} ]] && bindkey "${key[Right]}" forward-char
[[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
[[ -n ${key[Up]} ]] && bindkey "${key[Up]}" up-line-or-history
[[ -n ${key[Down]} ]] && bindkey "${key[Down]}" down-line-or-history

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"


# Enable a subset of plugins and enforce terminal type while running in Emacs
if [[ -n "$INSIDE_EMACS" ]]; then
    ZSH_TMUX_AUTOSTART=false
    ZSH_TMUX_UNICODE=true
fi

## global aliases
# case insensitive matches, recursive, show filename
alias -g GW="grep -iHR"
# case insensitive matches, recursive, filename only
alias -g GF="grep -ilR"
# redirect / pipe output"
alias -g N=">/dev/null"
alias -g L="|less"
alias -g M="|more"
# show output as one horizontal line
alias -g trn="|tr '\n' ' '; echo"
# show unique second column data
alias -g ucs='| awk '\''{print $2}'\'' | sort -u'

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
# If the argument to cd is the name of a parameter whose value is a valid directory,
# it will become the current directory.
setopt CDABLEVARS
# INTERACTIVECOMMENTS turns on interactive comments; comments begin with a #.
setopt INTERACTIVECOMMENTS

show_colors() {
    for i in {0..255} ; do
        let j=255-$i
        printf "\x1b[38;5;${i}m ${i} \x1b[0m"
        printf "\x1b[48;5;${i}m\x1b[38;5;${j}m ${i} \x1b[0m"
    done
    printf "\n"
}

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
# setopt INC_APPEND_HISTORY
# + SHARE_HISTORY :: share history between different zsh processes
# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="yyyy-mm-dd"

unsetopt SHARE_HISTORY
# Don't error out on zero matches
setopt NULL_GLOB
## Line editor
setopt NO_BEEP

# load Bash and zsh compatible aliases
[[ -f $HOME/.aliases ]] && source $HOME/.aliases
# For direct links to work in zsh, replace spaces with backslash spaces
[[ -f ${BROWSER} ]] && alias -s htm="$(echo $BROWSER|sed -e 's/ /\\\ /g')"
[[ -f ${BROWSER} ]] && alias -s html="$(echo $BROWSER|sed -e 's/ /\\\ /g')"
[[ -f "${EDITOR}" ]] && alias -s txt="$(echo $EDITOR|sed -e 's/ /\\\ /g')"
[[ -f "${READER}" ]] && alias -s epub="$(echo $READER|sed -e 's/ /\\\ /g')"
[[ -f "${READER}" ]] && alias -s pdf="$(echo $READER|sed -e 's/ /\\\ /g')"
[[ ! -z "$UPDATE" ]] && alias update="${UPDATE}"


# If on Windows, change from 1252 (default) to UTF-8 codepage for output
chcp 65001 > /dev/null

source ~/.zplug/init.zsh
zplug "plugins/tmux", from:oh-my-zsh
zplug "b4b4r07/enhancd", use:init.sh
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/github", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "zsh-users/zsh-autosuggestions"
zplug "themes/agnoster", as:theme, from:oh-my-zsh
zplug "zsh-users/zsh-syntax-highlighting" #  Should be last
zplug load
# Check if this is the first run; if so, install and reload plugins
zplug check || zplug install && zplug load

