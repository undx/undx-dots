#!/usr/bin/zsh

STUDIOS=$(find ~/Work/ -executable -name '*-linux-gtk-x86_64' | sort -t V -r)
STUDIO=$(echo $STUDIOS | rofi -dmenu -width 30 -i -p "Launch Talend Studio: ")
$STUDIO
