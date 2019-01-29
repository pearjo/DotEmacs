# copy init
cp -f init.el ~/.emacs.d/init.el

# copy snippets
cp -rf snippets ~/.emacs.d/

# copy fonts
cp -rf fonts/* ~/.local/share/fonts/

# copy desktop file
cp -f emacsclient.desktop ~/.local/share/applications/
