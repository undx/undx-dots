#!/bin/bash

echo "***** Razer BlackWidow setup *****"
razer=$(lsusb|rg BlackWidow)

if [ -z "$razer" ]; then
  echo "Razer BlackWidow not present on system";
  exit;
fi

echo "Setting up $razer"
pgrep openrazer||openrazer-daemon
setxkbmap -layout ux -option ctrl:nocaps -option compose:menu -option altr:altgr,altwin:swap_lalt_lwin
xmodmap -e 'keycode 134 = ISO_Level3_Shift'
xmodmap -e 'keycode 108 = Multi_key'

