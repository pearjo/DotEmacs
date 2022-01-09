rem Install all emacs files to the .emacs.d directory.
title Install configuration to .emacs.d

set emacs_path=%appdata%\.emacs.d

if not exist %emacs_path% (
   mkdir %emacs_path% 2> nul
)

if not exist %appdata%\org (
   mkdir %appdata%\org 2> nul
   mkdir %appdata%\org\notes 2> nul
   copy NUL %appdata%\org\tasks.org
)

rem copy files
copy init.el %emacs_path% /y
xcopy snippets %emacs_path%\snippets /S /Y /E
xcopy submodules %emacs_path%\submodules /S /Y /E
xcopy lisp %emacs_path%\lisp /S /Y /E

rem copy fonts
copy fonts\cascadia-code\*.ttf %windir%\Fonts /Y

rem copy editorconfig
copy .editorconfig C:\.editorconfig
