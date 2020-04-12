#!/bin/sh
# verbose level
VERBOSE=1
# destinattion target
TARGET=${1:-~}
if [ ! -d "$TARGET" ]; then
  echo Target $TARGET does not exist
  exit
fi
# check for stow
if ! command -v stow >/dev/null 2>&1; then
  echo stow is not installed
  exit
fi

echo "*** stowing files to $TARGET"


# dics
# emacs - create directory to avoid that private sub folder to be skipped
stow -v$VERBOSE emacs.d -t $TARGET
#
stow -v$VERBOSE git -t $TARGET
stow -v$VERBOSE wm  -t $TARGET
#
mkdir -p $TARGET/.local/share/{applications,fonts,nemo}
stow -v$VERBOSE local -t $TARGET
#
mkdir -p $TARGET/.config/mpv/scripts
mkdir -p $TARGET/.config/zathura
stow -v$VERBOSE media -t $TARGET
#
stow -v$VERBOSE shell -t $TARGET
stow -v$VERBOSE vim -t $TARGET
stow -v$VERBOSE X11 -t $TARGET
stow -v$VERBOSE zsh -t $TARGET
stow -v$VERBOSE vim -t $TARGET
mkdir -p $TARGET/.config/qutebrowser
mkdir -p $TARGET/.local/share/qutebrowser
stow -v$VERBOSE www -t $TARGET

# xkb - install my custom keyboard
sudo stow -v$VERBOSE xkb -t /usr/share/X11/xkb/symbols/

# install tools

# install packages

