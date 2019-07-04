:: Install all emacs files to the .emacs.d directory.
title Install .emacs.d

set emacs_d_path=%appdata%\.emacs.d

if not exist %emacs_d_path% (
   mkdir %emacs_d_path% 2> nul
)

:: copy files
copy init.el %emacs_d_path% /y
xcopy snippets %emacs_d_path% /Y /E
xcopy lisp %emacs_d_path% /Y /E

:: copy fonts
copy fonts\*.ttf %windir%\Fonts /Y
copy fonts\Inconsolata\*.ttf %windir%\Fonts /Y
