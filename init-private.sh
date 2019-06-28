#!/bin/sh

TARGET=${1:-~}
if [ ! -d "$TARGET" ]; then
  echo Target $TARGET does not exist
  exit
fi
# symlink to sensible config files
cd ~/Dropbox/undx/private/
stow -v personal -t $TARGET
stow -v work     -t $TARGET

