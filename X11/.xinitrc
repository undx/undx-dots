#xrandr --output VGA1 --auto --left-of DP1
xmodmap ~/.Xmodmap
xrdb ~/.Xdefaults
#setxkbmap -model pc105 -layout us,fr -variant ,oss -option grp:alt_shift_toggle -option ctrl:nocaps -option compose:ralt -option grp_led:scroll
export XCOMPOSE=~/.XCompose
setxkbmap -layout ux -option ctrl:nocaps -option compose:menu
#
#
#



[ -f ~/.xinitrc ] && . ~/.xinitrc


xcape -e 'Shift_L=Shift_L|parenleft;Shift_R=Shift_R|parenright;Control_L=Escape'
# Start a D-Bus session
# source /etc/X11/xinit/xinitrc.d/30-dbus
# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)

export PATH=~/bin:~/.local/bin:~/bin/Idea/bin/:/usr/local/bin:$PATH
export BROWSER=google-chrome
export DE=gnome
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID
export GPG_AGENT_INFO
export OOO_FORCE_DESKTOP=gnome
export SHELL=zsh
export SSH_AUTH_SOCK
export XDG_CURRENT_DESKTOP=GNOME
#
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export TERMINAL='rxvt-unicode-256color'


[ -f "$HOME/.xprofile" ] && . ~/.xprofile

#
autokey-gtk &
xscreensaver -no-splash &
nm-applet &
#exec awesome >> ~/.cache/awesome/stdout 2>> ~/.cache/awesome/stderr
# Launch i3 window manager
#    eval $(dbus-launch --sh-syntax)
#exec i3
# startx /usr/bin/gnome-session
# emacs --daemon
