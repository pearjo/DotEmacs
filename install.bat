rem Install all emacs files to the .emacs.d directory.
title Install configuration to .emacs.d

set emacs_path=%appdata%\.emacs.d

if not exist %emacs_path% (
   mkdir %emacs_path% 2> nul
)

rem copy files
copy init.el %emacs_path% /y
xcopy snippets %emacs_path%\snippets /S /Y /E
xcopy lisp %emacs_path%\lisp /S /Y /E

rem copy fonts
rem copy fonts\*.ttf %windir%\Fonts /Y
rem copy fonts\Inconsolata\*.ttf %windir%\Fonts /Y

rem copy editorconfig
copy editorconfig C:\.editorconfig
