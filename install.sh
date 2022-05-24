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
if ! [ -x "$(command -v bundler)" ]; then
  echo "Install missing bundler"
  gem install --user-install bundler
fi
bundle install
