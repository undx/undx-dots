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
# fix some permissions
chmod -R go-rwx ~/.authinfo
chmod -R go-rwx ~/.gnupg
chmod -R go-rwx ~/.password-store
chmod -R go-rwx ~/.emacs.d/private
chmod -R go-rwx ~/.config/autokey/data/
chmod -R go-rwx ~/.ivy2/.credentials
chmod -R go-rwx ~/.m2/settings.xml
chmod -R go-rwx ~/azure.properties
chmod -R go-rwx ~/.npmrc

