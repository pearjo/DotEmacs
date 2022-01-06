;;; linter.el --- Load various linter  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: c, convenience, languages

;; Package-Requires: ((use-package))

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

;;; Commentary:

;; This library loads various linter to make editing in the various
;; languages loaded by the `modules' library more convenient.

;;; Code:
(require 'use-package)

;; the linter for human language...
(if (string-equal system-type "windows-nt")
    ((message "Using hunspell.exe as spell checker")
     (setq ispell-program-name "hunspell.exe"))
  (message "Using aspell as spell checker")
  (setq ispell-program-name "aspell"))
(setq ispell-really-aspell t)
(setq ispell-extra-args '("--sug-mode=fast"))
(setq ispell-list-command "--list")
(setq ispell-dictionary "english")
(setq flyspell-issue-message-flag nil)

;; check spelling in code comments and various text modes
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1)
  (add-hook 'editorconfig-after-apply-functions
            '(lambda (props)
               (let ((max-line-length (gethash 'max_line_length props)))
                 (cond ((equal max-line-length "off")
                        (visual-line-mode 1)
                        (display-fill-column-indicator-mode -1))
                       (t
                        (visual-line-mode -1)
                        (display-fill-column-indicator-mode 1)))))))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flymake-ruby
  :ensure t
  :hook (ruby-mode . flymake-ruby-load))

(use-package qt-c-style
  :hook ((c-mode-common . qt-set-c-style)
         (c-mode-common . qt-make-newline-indent)))

(use-package rubocop
  :ensure t
  :hook ruby-mode)

(provide 'linter)
;;; linter.el ends here
