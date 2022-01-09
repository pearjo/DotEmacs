;;; darwin-config.el --- Darwin specific configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: c, convenience, languages

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(defun set-exec-path-from-shell-path ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
	  "[ \t\n]*$" "" (shell-command-to-string
			  "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq-default exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-path)

;; fix key issues on a mac
(setq-default mac-option-key-is-meta t
              mac-right-option-modifier nil)
(global-set-key (kbd "M-l") "@")
;; for keyboards with no right options key
(global-set-key (kbd "M-5") "[")
(global-set-key (kbd "M-6") "]")
(global-set-key (kbd "M-7") "|")
(global-set-key (kbd "M-8") "{")
(global-set-key (kbd "M-9") "}")

(provide 'darwin-config)
;;; darwin-config.el ends here
