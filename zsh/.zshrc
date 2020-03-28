export ZSH_HOME=~/.zsh
#
# Prepend all directories in $ZSH_DIR/functions to the fpath
fpath=($ZSH_HOME/functions/**/ $ZSH_HOME/completions/**/ $fpath)
# and autoload all files in $ZSH_DIR/functions
autoload -Uz $ZSH_HOME/functions/**/*(.:t)
#
setopt histignoredups
setopt EXTENDED_HISTORY
setopt HIST_BEEP
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
#
typeset -U path
autoload -Uz compinit
autoload -Uz vcs_info
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
#zstyle ':vcs_info:git*' formats "%s  %r/%S %b (%a) %m%u%c "
# %s The current version control system, like git or svn.
# %r The name of the root directory of the repository
# %S The current path relative to the repository root directory
# %b Branch information, like master
# %m In case of Git, show information about stashes
# %u Show unstaged changes in the repository
# %c Show staged changes in the repository
zstyle ':vcs_info:git*' formats "(%b%{$fg[grey]%}%m%u%c%)"
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

ssh_info() {
  [[ "$SSH_CONNECTION" != '' ]] && echo '%(!.%{$fg[red]%}.%{$fg[yellow]%})%n%{$reset_color%}@%{$fg[green]%}%m%{$reset_color%}:' || echo ''
}

# Echoes information about Git repository status when inside a Git repository
git_info() {
# Red (●) means there are untracked changes.
# Yellow (●) means there are unstaged changes.
# Green (●) means there are staged changes.
  # Exit if not inside a Git repository
  ! git rev-parse --is-inside-work-tree > /dev/null 2>&1 && return

  # Git branch/tag, or name-rev if on detached head
  local GIT_LOCATION=${$(git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD)#(refs/heads/|tags/)}
  local AHEAD="%{$fg[red]%}⇡NUM%{$reset_color%}"
  local BEHIND="%{$fg[cyan]%}⇣NUM%{$reset_color%}"
  local MERGING="%{$fg[magenta]%}⚡︎%{$reset_color%}"
  local UNTRACKED="%{$fg[red]%}●%{$reset_color%}"
  local MODIFIED="%{$fg[yellow]%}●%{$reset_color%}"
  local STAGED="%{$fg[green]%}●%{$reset_color%}"

  local -a DIVERGENCES
  local -a FLAGS

  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    DIVERGENCES+=( "${AHEAD//NUM/$NUM_AHEAD}" )
  fi

  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    DIVERGENCES+=( "${BEHIND//NUM/$NUM_BEHIND}" )
  fi

  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    FLAGS+=( "$MERGING" )
  fi

  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    FLAGS+=( "$UNTRACKED" )
  fi

  if ! git diff --quiet 2> /dev/null; then
    FLAGS+=( "$MODIFIED" )
  fi

  if ! git diff --cached --quiet 2> /dev/null; then
    FLAGS+=( "$STAGED" )
  fi

  local -a GIT_INFO
  GIT_INFO+=( "\033[38;5;15m±" )
  [ -n "$GIT_STATUS" ] && GIT_INFO+=( "$GIT_STATUS" )
  [[ ${#DIVERGENCES[@]} -ne 0 ]] && GIT_INFO+=( "${(j::)DIVERGENCES}" )
  [[ ${#FLAGS[@]} -ne 0 ]] && GIT_INFO+=( "${(j::)FLAGS}" )
  GIT_INFO+=( "\033[38;5;15m$GIT_LOCATION%{$reset_color%}" )
  echo "${(j: :)GIT_INFO}"

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
   ${vcs_info_msg_0_}
    " "
  '}}

# Use ❯ as the non-root prompt character; # for root
# Change the prompt character color if the last command had a nonzero exit code
PS1='
$(ssh_info)%{$fg[magenta]%}%~%u $(git_info)
%(?.%{$fg[blue]%}.%{$fg[red]%})%(!.#.❯)%{$reset_color%} '

  PS2=$'%_>'
  #RPROMPT=$'${vcs_info_msg_0_}'
}
setprompt
case $TERM in
  xterm*)
    precmd () {
      vcs_info
      print -Pn "\e]0;%~\a"
  }
    ;;
esac
#
# plugins with zplug
# $ yay -S zplug
source /usr/share/zsh/scripts/zplug/init.zsh
#
# plugins
#
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "momo-lab/zsh-abbrev-alias"
#zplug "zsh-users/zsh-syntax-highlighting", defer:2
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
# # fish shell like highlight
# ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
# # Highlight commands that contain rm -rf
# ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
# # Override highlighter colors
# ZSH_HIGHLIGHT_STYLES[default]=none
# ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=009
# ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=009,standout
# ZSH_HIGHLIGHT_STYLES[alias]=fg=darkgreen,bold
# ZSH_HIGHLIGHT_STYLES[builtin]=fg=orange,bold
# ZSH_HIGHLIGHT_STYLES[function]=fg=grey,bold
# ZSH_HIGHLIGHT_STYLES[command]=fg=darkgrey,bold
# ZSH_HIGHLIGHT_STYLES[precommand]=fg=white,underline
# ZSH_HIGHLIGHT_STYLES[commandseparator]=none
# ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=009
# ZSH_HIGHLIGHT_STYLES[path]=fg=214,underline
# ZSH_HIGHLIGHT_STYLES[globbing]=fg=063
# ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=white,underline
# ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=none
# ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=none
# ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
# ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=blue
# ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=blue,bold
# ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=009
# ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=009
# ZSH_HIGHLIGHT_STYLES[assign]=none
#
# zsh-notify : send notification when long running job ends
#
#
#/usr/share/icons/Adwaita/64x64/status/weather-clear-symbolic.symbolic.png
#/usr/share/icons/Adwaita/64x64/status/weather-severe-alert-symbolic.symbolic.png
zstyle ':notify:*' activate-terminal yes
#
zstyle ':notify:*' error-title "wow such #FAIL (in #{time_elapsed} seconds)"
zstyle ':notify:*' error-icon "/home/undx/Dropbox/Media/Pictures/System/dog-error-101.gif"
#zstyle ':notify:*' error-icon    "/usr/share/icons/Adwaita/64x64/status/weather-severe-alert-symbolic.symbolic.png"
zstyle ':notify:*' error-sound "/home/undx/Dropbox/Media/Sounds/mphg-ni.wav"
zstyle ':notify:*' success-title "very #SUCCESS. wow !!! (in #{time_elapsed} seconds)"
zstyle ':notify:*' success-icon "/home/undx/Dropbox/Media/Pictures/System/doc-succes.gif"
#zstyle ':notify:*' success-icon  "/usr/share/icons/Adwaita/64x64/status/weather-clear-symbolic.symbolic.png"
zstyle ':notify:*' success-sound "/home/undx/Dropbox/Media/Sounds/process-complete.wav"
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
alias cee="cd ~/Code/Talend/connectors-ee"
alias crt="cd ~/Code/Talend/component-runtime"
# sxiv
# never remember the correct naming...
alias i="sxiv"
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
bindkey '^[[3~' delete-char
bindkey '^[[F' end-of-line
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
alias dckps="docker ps --filter=status=running --format 'table {{.ID}}\t{{.Names}}\t({{.Image}})'"
alias dckpsl="docker ps --filter=status=running --format 'table {{.ID}}\t{{.Names}}\t({{.Image}})\t{{.Ports}}'"
alias dckimg="docker images | head -15"
alias dckimgl="docker images | head -30"
# jira
eval "$(jira --completion-script-zsh)"
# get TPD functions
. ~/Dropbox/undx/streams.sh
