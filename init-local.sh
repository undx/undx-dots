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
mkdir $TARGET/.emacs.d/
stow -v$VERBOSE emacs.d -t $TARGET
#
stow -v$VERBOSE git -t $TARGET
stow -v$VERBOSE i3 -t $TARGET
#
mkdir -p $TARGET/.local/share/{applications,fonts,nemo}
stow -v$VERBOSE local -t $TARGET
#
stow -v$VERBOSE media -t $TARGET
# mutt
stow -v$VERBOSE neomutt -t $TARGET
#
stow -v$VERBOSE shell -t $TARGET
stow -v$VERBOSE vim -t $TARGET
stow -v$VERBOSE X11 -t $TARGET
stow -v$VERBOSE zsh -t $TARGET
stow -v$VERBOSE vim -t $TARGET

# xkb - install my custom keyboard
sudo stow -v$VERBOSE xkb -t /usr/share/X11/xkb/symbols/

# install tools

# install packages

