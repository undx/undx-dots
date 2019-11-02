export ZSH_HOME=~/.zsh
#
# Prepend all directories in $ZSH_DIR/functions to the fpath
fpath=($ZSH_HOME/functions/**/ $ZSH_HOME/completions/**/ $fpath)
# and autoload all files in $ZSH_DIR/functions
autoload -Uz $ZSH_HOME/functions/**/*(.:t)
#
setopt histignoredups
#
typeset -U path
autoload -Uz compinit
compinit
zstyle ':completion:*' menu select
zstyle ':completion::complete:*' gain-privileges 1
setopt COMPLETE_ALIASES
#
# complete with case insensitive:
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# mpv
zstyle ':completion:*:*:mpv:*' file-patterns '*.(#i)(flv|mp4|webm|mkv|wmv|mov|avi|mp3|ogg|wma|flac|wav|aiff|m4a|m4b|m4v|gif|ifo)(-.) *(-/):directories' '*:all-files'
zstyle ':completion:*:*:mpv:*' group-name ''
#
#
#
source $ZSH_HOME/zle.zsh

# by default: export WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
# we take out the slash, period, angle brackets, dash here.
export WORDCHARS='*?_[]~=&;!#$%^(){}'

# -- coloured manuals
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}

autoload -Uz promptinit
promptinit
setprompt() {
  setopt prompt_subst

  if [[ -n "$SSH_CLIENT"  ||  -n "$SSH2_CLIENT" ]]; then
    p_host='%F{yellow}%M%f'
  else
    p_host='%F{green}%M%f'
  fi

  PS1=${(j::Q)${(Z:Cn:):-$'
    %F{cyan}[%f
    %(!.%F{red}%n%f.%F{green}%n%f)
    %F{cyan}@%f
    ${p_host}
    %F{cyan}][%f
    %F{blue}%~%f
    %F{cyan}]%f
    %(!.%F{red}%#%f.%F{green}%#%f)
    " "
  '}}

  PS2=$'%_>'
  RPROMPT=$'${vcs_info_msg_0_}'
}
setprompt
#
# plugins with zplug
# $ yay -S zplug
source /usr/share/zsh/scripts/zplug/init.zsh
#
# plugins
#
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "momo-lab/zsh-abbrev-alias"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "hlissner/zsh-autopair", defer:2
zplug "zsh-users/zsh-history-substring-search", defer:2
zplug "djui/alias-tips"
# jump quickly to directories that you have visited frequently in the past, or recently
# z et -> cd /etc
zplug "agkozak/zsh-z"
# gitgo:
# ghg - Go to the repo homepage
# ghc - The branch compare page
# ghp - Create PR from current branch in shell (very useful if you just pushed this branch)
zplug 'ltj/gitgo'
#
zplug 'marzocchi/zsh-notify'
# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi
# Then, source plugins and add commands to $PATH
zplug load # --verbose
#
# fish shell like highlight
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
# Highlight commands that contain rm -rf
ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
# Override highlighter colors
ZSH_HIGHLIGHT_STYLES[default]=none
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=009
ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=009,standout
ZSH_HIGHLIGHT_STYLES[alias]=fg=darkgreen,bold
ZSH_HIGHLIGHT_STYLES[builtin]=fg=orange,bold
ZSH_HIGHLIGHT_STYLES[function]=fg=grey,bold
ZSH_HIGHLIGHT_STYLES[command]=fg=darkgrey,bold
ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
ZSH_HIGHLIGHT_STYLES[commandseparator]=none
ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009
ZSH_HIGHLIGHT_STYLES[path]=fg=214,underline
ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=blue
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=blue,bold
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=009
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=009
ZSH_HIGHLIGHT_STYLES[assign]=none
#
# zsh-notify : send notification when long running job ends
#
#
#/usr/share/icons/Adwaita/64x64/status/weather-clear-symbolic.symbolic.png
#/usr/share/icons/Adwaita/64x64/status/weather-severe-alert-symbolic.symbolic.png
zstyle ':notify:*' activate-terminal yes
zstyle ':notify:*' error-icon    "/usr/share/icons/Adwaita/64x64/status/weather-severe-alert-symbolic.symbolic.png"
zstyle ':notify:*' error-sound   "Glass"
zstyle ':notify:*' error-title   "Command failed (in #{time_elapsed} seconds)"
zstyle ':notify:*' success-icon  "/usr/share/icons/Adwaita/64x64/status/weather-clear-symbolic.symbolic.png"
zstyle ':notify:*' success-sound "default"
zstyle ':notify:*' success-title "Command finished (in #{time_elapsed} seconds)"
#
#
# abbrev-alias as vim's abbrev
#
# type G<space> -> | rg
abbrev-alias -g G="| rg"
abbrev-alias -g rgp="| rg --passthru"
abbrev-alias -g duc="~/Dropbox/undx/config/"
abbrev-alias -g cloc="~/Code/local/"
abbrev-alias -g ctal="~/Code/Talend/"
#
# classic aliases
#
alias ls="ls --color=auto"
alias ll="ls -l"
alias la="ls -la"
alias lh="ls -lah"
alias  l="ls -1"
alias dots="cd ~/Code/local/undx-dots"
alias cse="cd ~/Code/Talend/connectors-se"
# neovim
if type nvim > /dev/null 2>&1; then
  alias vim='nvim'
fi
#
# bindings
#
#
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
#
#
#
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
export GPG_TTY=$(tty)
#
# fzf init
#
source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/fzf-extras.zsh
#
# work related
#
#
# docker
alias dckps="docker ps --all --format '[{{.ID}}] {{.Names}}\t({{.Image}})\t\t~ {{.Status}}'"
alias dckimg="docker images | head -15"
# jira
eval "$(jira --completion-script-zsh)"
# get TPD functions
. ~/Dropbox/undx/streams.sh

