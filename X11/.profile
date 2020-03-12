# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022
################
# Base exports #
################
echo Base exports
# locale
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
# terminal
export TERMINAL=st
# editor
export EDITOR="gvim --nofork"
# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"
# history
export HISTFILE=~/.history
export HISTSIZE=100000
export SAVEHIST=100000
# bash
export HISTCONTROL=ignoreboth:erasedups

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
# local binaries
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
###################
# ssh environment #
###################
echo ssh editor
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='gvim --nofork'
fi
#######
# GPG #
#######
echo "[GPG] config"
# setting default key
export GPGKEY=ACB4643F83465C41CECEBE8C2FD36EAAF03F8796
#
# In order for gpg to find gpg-agent, gpg-agent must be running, and there must be an env
# variable pointing GPG to the gpg-agent socket. This little script, which must be sourced
# in your shell's init script (ie, .bash_profile, .zshrc, whatever), will either start
# gpg-agent or set up the GPG_AGENT_INFO variable if it's already running.

# Add the following to your shell init to set up gpg-agent automatically for every shell
if [ -f ~/.gnupg/.gpg-agent-info ] && [ -n "$(pgrep gpg-agent)" ]; then
    source ~/.gnupg/.gpg-agent-info
    export GPG_AGENT_INFO
else
    eval $(gpg-agent --daemon)
fi
export GPG_DIR=~/.gnupg/
export GPG_KEYNAME=E16448E7EC79DD12245C4ADFFA5FA52B5B7B42F0
################
# Mail related #
################
export MAILDIR=$HOME/.mail
######################
# LD additional libs #
######################
echo "[LD] Additional libraries to LD_LIBRARY_PATH"
export LD_LIBRARY_PATH=/usr/lib/sap
##################
# go environment #
##################
echo "[env] golang"
if [ -d "$HOME/Code/go/bin" ] ; then
    PATH="$HOME/Code/go/bin:$PATH"
fi
export GOPATH="$HOME/Code/go/"
#########
# Plan9 #
#########
echo "[env] Plan9"
export PLAN9=/home/undx/Code/local/plan9port
export PATH=$PATH:$PLAN9/bin
#######
# fzf #
#######
echo "[env] fzf"
# Exclude those directories even if not listed in .gitignore, or if .gitignore is missing
FD_OPTIONS="--follow --exclude .git --exclude node_modules"
# Change behavior of fzf dialogue
export FZF_DEFAULT_OPTS="--no-mouse --height 50% -1 --reverse --multi --inline-info --preview='[[ \$(file --mime {}) =~ binary ]] && echo {} is a binary file || (bat --style=numbers --color=always {} || cat {}) 2> /dev/null | head -300' --preview-window='right:hidden:wrap' --bind='f3:execute(bat --style=numbers {} || less -f {}),f2:toggle-preview,ctrl-d:half-page-down,ctrl-u:half-page-up,ctrl-a:select-all+accept,ctrl-y:execute-silent(echo {+} | pbcopy)'"
# Change find backend
# Use 'git ls-files' when inside GIT repo, or fd otherwise
export FZF_DEFAULT_COMMAND="git ls-files --cached --others --exclude-standard | fd --type f --type l $FD_OPTIONS"
# Find commands for "Ctrl+T" and "Opt+C" shortcuts
export FZF_CTRL_T_COMMAND="fd $FD_OPTIONS"
export FZF_ALT_C_COMMAND="fd --type d $FD_OPTIONS"
# bat
export BAT_PAGER="less -RF"
#############
# Resources #
#############
echo "[env] compiling resources"
if [ -f ~/.Xresources ]; then
    xrdb ~/.Xresources
fi
####################################################################################
# get rid of `Couldn't connect to accessibility bus: Failed to connect to socket...`
####################################################################################
export NO_AT_BRIDGE=1
##############################
# Other stuff - Work related #
##############################
echo "[env] Talend"
export JAVA_HOME=/usr/lib/jvm/default
export SOURCES=~/Code/Talend
export TALEND_REGISTRY=registry.datapwn.com
export GOOGLE_APPLICATION_CREDENTIALS=~/Dropbox/egallois-gcloud-key.json
source ~/Dropbox/undx/private/work/tacokit-env.sh

