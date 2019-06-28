# zle : Zsh Line Editor
# zle widget eg command
# zle -la  - list all
# alt+x execute-named-cmd
#
# \e is alt or M
# ^  is ctrl or C
# \^[  is esc
# binding multiple commands:
# M-g is : do C-u (clear line), type git status <Return>
#
# emacs style
#
bindkey -e

# use: `cat -v` to see escape codes.
#
# st@rogue
# 
bindkey "^[[H"  beginning-of-line
bindkey "^[[4~" end-of-line
bindkey "^[[P"  delete-char
bindkey "^[[4h" overwrite-mode
# ShiftTab
bindkey "^[[Z"  reverse-menu-complete
# PageUp "^[[5~"
# PageDown "^[[6~"
#
#
#
#
bindkey '\ew' kill-region                             # [Esc-w] - Kill from the cursor to the mark
bindkey -s '\el' 'ls\n'                               # [Esc-l] - run command: ls
bindkey " " magic-space                               # [Space] - do history expansion
bindkey '^[[1;5C' forward-word                        # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                       # [Ctrl-LeftArrow] - move backward one word
#
#
# ESC-q push-line-or-edit
bindkey "^Q" push-line-or-edit
#
#
# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line
#
# bindkey -s '\eg' '^Ugit status^M'
# drawback just called by the binding
# create a func
function _git-status {
    zle kill-whole-line
    zle -U "git status"
    zle accept-line
}
# declare it as a widget
zle -N _git-status
# then bind it
bindkey '\eg' _git-status
#
insert-last-typed-word(){
  zle insert-last-word -- 0 -1
};
zle -N insert-last-typed-word;
bindkey "\em" insert-last-typed-word

