#!/bin/bash
# Copyright (C) 2019-2022 Joe Pearson

# This file is not part of GNU Emacs.

# This file is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.

fontname="SF Mono"
fonturl="https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg"

lispdir=./lisp
destdir=~/.emacs.d

# begin installation of my configuration
mkdir -p $destdir

# copy lisp files
echo "Install lisp files to: $destdir"
cp -f ./init.el $destdir/init.el
cp -rf $lispdir $destdir/

cp -f .editorconfig ~/.editorconfig
cp -rf snippets ~/.emacs.d/
cp -rf submodules ~/.emacs.d/

# install fonts only if missing
fontfile=$(basename $fonturl)
fileext=${fontfile##*.}

if [[ -z $(fc-list | grep -i "$fontname") ]]; then
    if [[ $(uname -s) == Darwin ]]; then
        fontdest=~/Library/Fonts
    else
        fontdest=~/.local/share/fonts
    fi

    mkdir -p ./font
    curl -L $fonturl --create-dir -o ./font/$fontfile

    case $fileext in
        dmg)
            fontfilename=$(basename -s .$fileext $fontfile)
            if [[ $(uname -s) == Darwin ]]; then
	        hdiutil mount -mountpoint /Volumes/$fontfilename ./font/$fontfile
                if [[ $(whoami) != "root" ]]; then
                    echo "Failed to install '$fontname'!"
                    echo "Run this script as root."
                    exit 1
                fi
	        installer -pkg /Volumes/$fontfilename/*.pkg -target LocalSystem
	        hdiutil unmount /Volumes/$fontfilename
            else
                7z -o./font/$fontfilename x ./font/$fontfile
                7z -o./font/Payload x ./font/$fontfilename/**/*.pkg
                7z -o./font x ./font/Payload/*
                cp -rf ./font/Library/Fonts $fontdest/$fontfilename
            fi
            ;;
        zip)
            mkdir -p $fontsdest
	    unzip ./font/$fontfile -d $fontsdest/
            ;;
        *)
            echo "Can't install font. Unknown file extension: '.$fileext'";;
    esac
    fc-cache -vf
else
    echo "Skip installing '$fontname' since its already installed."
fi

if [[ $(uname -s) == Linux ]]; then
  # copy desktop file
  if [ ! -d "~/.local/share/applications" ]; then
      mkdir -p ~/.local/share/applications
  fi
  cp -f emacsclient.desktop ~/.local/share/applications/

  # copy emacs service
  if [ ! -d "~/.config/systemd/user/" ]; then
      mkdir -p ~/.config/systemd/user/
  fi
  cp -f emacs.service ~/.config/systemd/user/
fi

echo "Install Ruby gems required for the ruby-mode"
gem install bundler
bundle install
