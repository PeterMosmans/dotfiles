# OS-independent
# Only sourced for interactive shell, last

export ZPLUG_HOME=~/.zplug
ZPLUG_REPO=https://github.com/PeterMosmans/zplug
# Bootstrap installation
[[ ! -d $ZPLUG_HOME ]] && git clone $ZPLUG_REPO $ZPLUG_HOME
#ZSH_TMUX_AUTOSTART=true
export TMUX_THEME="dracula"
export GIT_EDITOR=$EDITOR
HISTFILE=~/.zsh_history
SAVEHIST=1000
HISTSIZE=1000

# export LC_ALL=C.UTF-8
# export LANG=C.UTF-8  #en_US.utf8
# Tell ncurses to use UTF-8
export NCURSES_NO_UTF8_ACS=1

# Use nice colors for less
if [[ -f /usr/share/source-highlight/src-hilite-lesspipe.sh ]]; then
  export LESSOPEN="|/usr/share/source-highlight/src-hilite-lesspipe.sh %s"
  export LESS=' -R '
fi

# Check for and load terminal-specific keybindings
[ -f ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE} ] && source ~/.zkbd/$TERM-${${DISPLAY:t}:-$VENDOR-$OSTYPE}
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#History-Control
[[ -n ${key[Left]} ]] && bindkey "${key[Left]}" backward-char
[[ -n ${key[Right]} ]] && bindkey "${key[Right]}" forward-char
[[ -n ${key[Home]} ]] && bindkey "${key[Home]}" beginning-of-line
[[ -n ${key[End]} ]] && bindkey "${key[End]}" end-of-line
[[ -n ${key[Up]} ]] && bindkey "${key[Up]}" up-line-or-history
[[ -n ${key[Down]} ]] && bindkey "${key[Down]}" down-line-or-history
bindkey "\e[3~" delete-char  # Map the delete key
# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"



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

show_colors() {
    for i in {0..255} ; do
        let j=255-$i
        printf "\x1b[38;5;${i}m ${i} \x1b[0m"
        printf "\x1b[48;5;${i}m\x1b[38;5;${j}m ${i} \x1b[0m"
    done
    printf "\n"
}

# use less to open file and apply specified (or log) syntax highlighting
lsyntax() {
    source-highlight -f esc --lang-def=${2:-log}.lang -i $1 | less -RX
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

# Get pretty directory colors
LS_COLORS='bd=38;5;68:ca=38;5;17:cd=38;5;113;1:di=38;5;30:do=38;5;127:ex=38;5;208;1:pi=38;5;126:fi=0:ln=target:mh=38;5;222;1:no=0:or=48;5;196;38;5;232;1:ow=38;5;220;1:sg=48;5;3;38;5;0:su=38;5;220;1;3;100;1:so=38;5;197:st=38;5;86;48;5;234:tw=48;5;235;38;5;139;3:*LS_COLORS=48;5;89;38;5;197;1;3;4;7:*README=38;5;220;1:*README.rst=38;5;220;1:*README.md=38;5;220;1:*LICENSE=38;5;220;1:*COPYING=38;5;220;1:*INSTALL=38;5;220;1:*COPYRIGHT=38;5;220;1:*AUTHORS=38;5;220;1:*HISTORY=38;5;220;1:*CONTRIBUTORS=38;5;220;1:*PATENTS=38;5;220;1:*VERSION=38;5;220;1:*NOTICE=38;5;220;1:*CHANGES=38;5;220;1:*.log=38;5;190:*.txt=38;5;253:*.etx=38;5;184:*.info=38;5;184:*.markdown=38;5;184:*.md=38;5;184:*.mkd=38;5;184:*.nfo=38;5;184:*.pod=38;5;184:*.rst=38;5;184:*.tex=38;5;184:*.textile=38;5;184:*.bib=38;5;178:*.json=38;5;178:*.jsonl=38;5;178:*.msg=38;5;178:*.pgn=38;5;178:*.rss=38;5;178:*.xml=38;5;178:*.toml=38;5;178:*.yaml=38;5;178:*.yml=38;5;178:*.RData=38;5;178:*.rdata=38;5;178:*.cbr=38;5;141:*.cbz=38;5;141:*.chm=38;5;141:*.djvu=38;5;141:*.pdf=38;5;141:*.PDF=38;5;141:*.mobi=38;5;141:*.epub=38;5;141:*.docm=38;5;111;4:*.doc=38;5;111:*.docx=38;5;111:*.eps=38;5;111:*.ps=38;5;111:*.odb=38;5;111:*.odt=38;5;111:*.rtf=38;5;111:*.odp=38;5;166:*.pps=38;5;166:*.ppt=38;5;166:*.pptx=38;5;166:*.ppts=38;5;166:*.pptxm=38;5;166;4:*.pptsm=38;5;166;4:*.csv=38;5;78:*.tsv=38;5;78:*.ods=38;5;112:*.xla=38;5;76:*.xls=38;5;112:*.xlsx=38;5;112:*.xlsxm=38;5;112;4:*.xltm=38;5;73;4:*.xltx=38;5;73:*config=1:*cfg=1:*conf=1:*rc=1:*authorized_keys=1:*known_hosts=1:*.ini=1:*.plist=1:*.viminfo=1:*.pcf=1:*.psf=1:*.git=38;5;197:*.gitignore=38;5;240:*.gitattributes=38;5;240:*.gitmodules=38;5;240:*.awk=38;5;172:*.bash=38;5;172:*.bat=38;5;172:*.BAT=38;5;172:*.sed=38;5;172:*.sh=38;5;172:*.zsh=38;5;172:*.vim=38;5;172:*.ahk=38;5;41:*.py=38;5;41:*.ipynb=38;5;41:*.rb=38;5;41:*.gemspec=38;5;41:*.pl=38;5;208:*.PL=38;5;160:*.t=38;5;114:*.msql=38;5;222:*.mysql=38;5;222:*.pgsql=38;5;222:*.sql=38;5;222:*.tcl=38;5;64;1:*.r=38;5;49:*.R=38;5;49:*.gs=38;5;81:*.clj=38;5;41:*.cljs=38;5;41:*.cljc=38;5;41:*.cljw=38;5;41:*.scala=38;5;41:*.dart=38;5;51:*.asm=38;5;81:*.cl=38;5;81:*.lisp=38;5;81:*.lua=38;5;81:*.moon=38;5;81:*.c=38;5;81:*.C=38;5;81:*.h=38;5;110:*.H=38;5;110:*.tcc=38;5;110:*.c++=38;5;81:*.h++=38;5;110:*.hpp=38;5;110:*.hxx=38;5;110:*.ii=38;5;110:*.M=38;5;110:*.m=38;5;110:*.cc=38;5;81:*.cs=38;5;81:*.cp=38;5;81:*.cpp=38;5;81:*.cxx=38;5;81:*.cr=38;5;81:*.go=38;5;81:*.f=38;5;81:*.F=38;5;81:*.for=38;5;81:*.ftn=38;5;81:*.f90=38;5;81:*.F90=38;5;81:*.f95=38;5;81:*.F95=38;5;81:*.f03=38;5;81:*.F03=38;5;81:*.f08=38;5;81:*.F08=38;5;81:*.nim=38;5;81:*.nimble=38;5;81:*.s=38;5;110:*.S=38;5;110:*.rs=38;5;81:*.swift=38;5;219:*.sx=38;5;81:*.vala=38;5;81:*.vapi=38;5;81:*.hi=38;5;110:*.hs=38;5;81:*.lhs=38;5;81:*.agda=38;5;81:*.lagda=38;5;81:*.lagda.tex=38;5;81:*.lagda.rst=38;5;81:*.lagda.md=38;5;81:*.agdai=38;5;110:*.pyc=38;5;240:*.tf=38;5;168:*.tfstate=38;5;168:*.tfvars=38;5;168:*.css=38;5;125;1:*.less=38;5;125;1:*.sass=38;5;125;1:*.scss=38;5;125;1:*.htm=38;5;125;1:*.html=38;5;125;1:*.jhtm=38;5;125;1:*.mht=38;5;125;1:*.eml=38;5;125;1:*.mustache=38;5;125;1:*.coffee=38;5;074;1:*.java=38;5;074;1:*.js=38;5;074;1:*.mjs=38;5;074;1:*.jsm=38;5;074;1:*.jsm=38;5;074;1:*.jsp=38;5;074;1:*.php=38;5;81:*.ctp=38;5;81:*.twig=38;5;81:*.vb=38;5;81:*.vba=38;5;81:*.vbs=38;5;81:*Dockerfile=38;5;155:*.dockerignore=38;5;240:*Makefile=38;5;155:*MANIFEST=38;5;243:*pm_to_blib=38;5;240:*.nix=38;5;155:*.dhall=38;5;178:*.rake=38;5;155:*.am=38;5;242:*.in=38;5;242:*.hin=38;5;242:*.scan=38;5;242:*.m4=38;5;242:*.old=38;5;242:*.out=38;5;242:*.SKIP=38;5;244:*.diff=48;5;197;38;5;232:*.patch=48;5;197;38;5;232;1:*.bmp=38;5;97:*.dicom=38;5;97:*.tiff=38;5;97:*.tif=38;5;97:*.TIFF=38;5;97:*.cdr=38;5;97:*.flif=38;5;97:*.gif=38;5;97:*.ico=38;5;97:*.jpeg=38;5;97:*.JPG=38;5;97:*.jpg=38;5;97:*.nth=38;5;97:*.png=38;5;97:*.psd=38;5;97:*.xpm=38;5;97:*.webp=38;5;97:*.ai=38;5;99:*.eps=38;5;99:*.epsf=38;5;99:*.drw=38;5;99:*.ps=38;5;99:*.svg=38;5;99:*.avi=38;5;114:*.divx=38;5;114:*.IFO=38;5;114:*.m2v=38;5;114:*.m4v=38;5;114:*.mkv=38;5;114:*.MOV=38;5;114:*.mov=38;5;114:*.mp4=38;5;114:*.mpeg=38;5;114:*.mpg=38;5;114:*.ogm=38;5;114:*.rmvb=38;5;114:*.sample=38;5;114:*.wmv=38;5;114:*.3g2=38;5;115:*.3gp=38;5;115:*.gp3=38;5;115:*.webm=38;5;115:*.gp4=38;5;115:*.asf=38;5;115:*.flv=38;5;115:*.ts=38;5;115:*.ogv=38;5;115:*.f4v=38;5;115:*.VOB=38;5;115;1:*.vob=38;5;115;1:*.3ga=38;5;137;1:*.S3M=38;5;137;1:*.aac=38;5;137;1:*.au=38;5;137;1:*.dat=38;5;137;1:*.dts=38;5;137;1:*.fcm=38;5;137;1:*.m4a=38;5;137;1:*.mid=38;5;137;1:*.midi=38;5;137;1:*.mod=38;5;137;1:*.mp3=38;5;137;1:*.mp4a=38;5;137;1:*.oga=38;5;137;1:*.ogg=38;5;137;1:*.opus=38;5;137;1:*.s3m=38;5;137;1:*.sid=38;5;137;1:*.wma=38;5;137;1:*.ape=38;5;136;1:*.aiff=38;5;136;1:*.cda=38;5;136;1:*.flac=38;5;136;1:*.alac=38;5;136;1:*.midi=38;5;136;1:*.pcm=38;5;136;1:*.wav=38;5;136;1:*.wv=38;5;136;1:*.wvc=38;5;136;1:*.afm=38;5;66:*.fon=38;5;66:*.fnt=38;5;66:*.pfb=38;5;66:*.pfm=38;5;66:*.ttf=38;5;66:*.otf=38;5;66:*.PFA=38;5;66:*.pfa=38;5;66:*.7z=38;5;40:*.a=38;5;40:*.arj=38;5;40:*.bz2=38;5;40:*.cpio=38;5;40:*.gz=38;5;40:*.lrz=38;5;40:*.lz=38;5;40:*.lzma=38;5;40:*.lzo=38;5;40:*.rar=38;5;40:*.s7z=38;5;40:*.sz=38;5;40:*.tar=38;5;40:*.tgz=38;5;40:*.xz=38;5;40:*.z=38;5;40:*.zip=38;5;40:*.zipx=38;5;40:*.zoo=38;5;40:*.zpaq=38;5;40:*.zst=38;5;40:*.zz=38;5;40:*.apk=38;5;215:*.deb=38;5;215:*.rpm=38;5;215:*.jad=38;5;215:*.jar=38;5;215:*.cab=38;5;215:*.pak=38;5;215:*.pk3=38;5;215:*.vdf=38;5;215:*.vpk=38;5;215:*.bsp=38;5;215:*.dmg=38;5;215:*.r[0-9]{0,2}=38;5;239:*.zx[0-9]{0,2}=38;5;239:*.z[0-9]{0,2}=38;5;239:*.part=38;5;239:*.dmg=38;5;124:*.iso=38;5;124:*.bin=38;5;124:*.nrg=38;5;124:*.qcow=38;5;124:*.sparseimage=38;5;124:*.toast=38;5;124:*.vcd=38;5;124:*.vmdk=38;5;124:*.accdb=38;5;60:*.accde=38;5;60:*.accdr=38;5;60:*.accdt=38;5;60:*.db=38;5;60:*.fmp12=38;5;60:*.fp7=38;5;60:*.localstorage=38;5;60:*.mdb=38;5;60:*.mde=38;5;60:*.sqlite=38;5;60:*.typelib=38;5;60:*.nc=38;5;60:*.pacnew=38;5;33:*.un~=38;5;241:*.orig=38;5;241:*.BUP=38;5;241:*.bak=38;5;241:*.o=38;5;241:*core=38;5;241:*.rlib=38;5;241:*.swp=38;5;244:*.swo=38;5;244:*.tmp=38;5;244:*.sassc=38;5;244:*.pid=38;5;248:*.state=38;5;248:*lockfile=38;5;248:*lock=38;5;248:*.err=38;5;160;1:*.error=38;5;160;1:*.stderr=38;5;160;1:*.aria2=38;5;241:*.dump=38;5;241:*.stackdump=38;5;241:*.zcompdump=38;5;241:*.zwc=38;5;241:*.pcap=38;5;29:*.cap=38;5;29:*.dmp=38;5;29:*.DS_Store=38;5;239:*.localized=38;5;239:*.CFUserTextEncoding=38;5;239:*.allow=38;5;112:*.deny=38;5;196:*.service=38;5;45:*@.service=38;5;45:*.socket=38;5;45:*.swap=38;5;45:*.device=38;5;45:*.mount=38;5;45:*.automount=38;5;45:*.target=38;5;45:*.path=38;5;45:*.timer=38;5;45:*.snapshot=38;5;45:*.application=38;5;116:*.cue=38;5;116:*.description=38;5;116:*.directory=38;5;116:*.m3u=38;5;116:*.m3u8=38;5;116:*.md5=38;5;116:*.properties=38;5;116:*.sfv=38;5;116:*.srt=38;5;116:*.sub=38;5;116:*.theme=38;5;116:*.torrent=38;5;116:*.urlview=38;5;116:*.asc=38;5;192;3:*.bfe=38;5;192;3:*.enc=38;5;192;3:*.gpg=38;5;192;3:*.signature=38;5;192;3:*.sig=38;5;192;3:*.p12=38;5;192;3:*.pem=38;5;192;3:*.pgp=38;5;192;3:*.asc=38;5;192;3:*.enc=38;5;192;3:*.sig=38;5;192;3:*.p7s=38;5;192;3:*id_dsa=38;5;192;3:*id_rsa=38;5;192;3:*id_ecdsa=38;5;192;3:*id_ed25519=38;5;192;3:*.32x=38;5;213:*.cdi=38;5;213:*.fm2=38;5;213:*.rom=38;5;213:*.sav=38;5;213:*.st=38;5;213:*.a00=38;5;213:*.a52=38;5;213:*.A64=38;5;213:*.a64=38;5;213:*.a78=38;5;213:*.adf=38;5;213:*.atr=38;5;213:*.gb=38;5;213:*.gba=38;5;213:*.gbc=38;5;213:*.gel=38;5;213:*.gg=38;5;213:*.ggl=38;5;213:*.ipk=38;5;213:*.j64=38;5;213:*.nds=38;5;213:*.nes=38;5;213:*.sms=38;5;213:*.pot=38;5;7:*.pcb=38;5;7:*.mm=38;5;7:*.pod=38;5;7:*.gbr=38;5;7:*.scm=38;5;7:*.xcf=38;5;7:*.spl=38;5;7:*.Rproj=38;5;11:*.sis=38;5;7:*.1p=38;5;7:*.3p=38;5;7:*.cnc=38;5;7:*.def=38;5;7:*.ex=38;5;7:*.example=38;5;7:*.feature=38;5;7:*.ger=38;5;7:*.map=38;5;7:*.mf=38;5;7:*.mfasl=38;5;7:*.mi=38;5;7:*.mtx=38;5;7:*.pc=38;5;7:*.pi=38;5;7:*.plt=38;5;7:*.pm=38;5;7:*.rdf=38;5;7:*.ru=38;5;7:*.sch=38;5;7:*.sty=38;5;7:*.sug=38;5;7:*.t=38;5;7:*.tdy=38;5;7:*.tfm=38;5;7:*.tfnt=38;5;7:*.tg=38;5;7:*.vcard=38;5;7:*.vcf=38;5;7:*.xln=38;5;7:*.iml=38;5;166:*.xcconfig=1:*.entitlements=1:*.strings=1:*.storyboard=38;5;196:*.xcsettings=1:*.xib=38;5;208:'
export LS_COLORS

# Disable tmux while running in Emacs
if [[ -n "$INSIDE_EMACS" ]]; then
    ZSH_TMUX_AUTOSTART=false
    return 0
fi

source $ZPLUG_HOME/init.zsh
zplug "plugins/tmux", from:oh-my-zsh
zplug "MichaelAquilina/zsh-you-should-use"  # Check which aliases are being used
zplug "ael-code/zsh-colored-man-pages"
zplug "b4b4r07/enhancd", use:init.sh
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "themes/agnoster", as:theme, from:oh-my-zsh
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting" #  Should be last
zplug load
# Check if this is the first run; if so, install and reload plugins
zplug check || zplug install && zplug load

