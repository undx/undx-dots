#!/usr/bin/env python3

""" Adds a password to 'pass' storage.
    originaly from https://github.com/bbugyi200/scripts/blob/master/main/qute-pass-add"""

import argparse
import os
import platform
import getpass
import subprocess
import re

os.environ['EDITOR'] = '/usr/bin/vim'

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('raw_url', help="The URL of the website (before being cleaned).")

args = parser.parse_args()

url = re.sub('\.(com|edu|gov|org|io|fr|en|pt|re|it)/.*$', r'.\1', args.raw_url)
url = re.sub('https://', '', url)

pass_url_dir = '/home/' + getpass.getuser() + '/.password-store/WebSites/' + url
options = ''
if os.path.isdir(pass_url_dir):
    for filename in os.listdir(pass_url_dir):
        options += re.sub('\.gpg', '', filename) + '\n'

rofi = 'rofi -dmenu'
command = 'printf "{{options}}" | {} -p "Username to add/edit"'.format(rofi)
username = subprocess.check_output(command.format(options=options), shell=True).decode('utf-8')

url = 'WebSites/' + url + '/' + username

if username in options:
    subprocess.call(['st', '-n', 'qute-editor', '-e', 'zsh', '-c', 'gopass   edit {}'.format(url)])
else:
    subprocess.call(['st', '-n', 'qute-editor', '-e', 'zsh', '-c', 'gopass insert {}'.format(url)])


