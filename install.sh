#!/bin/bash

# copy init
if [ ! -d "~/.emacs.d" ]; then
    mkdir -p ~/.emacs.d
fi
cp -f init.el ~/.emacs.d/init.el

# copy snippets
cp -rf snippets ~/.emacs.d/

# copy submodules
cp -rf submodules ~/.emacs.d/

# copy fonts
if [[ $(uname -s) == Darwin ]]; then
    cp -rf fonts/* ~/Library/Fonts
else
    if [ ! -d "~/.local/share/fonts" ]; then
        mkdir -p ~/.local/share/fonts
    fi
    cp -rf fonts/* ~/.local/share/fonts/
fi

# copy lisp
cp -rf lisp ~/.emacs.d/

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

# copy editorconfig file
cp -f .editorconfig ~/.editorconfig
