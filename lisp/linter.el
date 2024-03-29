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

;; the linter for human language...
(setq-default ispell-program-name (if (eq system-type 'windows-nt)
                                      "hunspell.exe"
                                    "aspell")
              ispell-really-aspell t
              ispell-extra-args '("--sug-mode=fast")
              ispell-list-command "--list"
              ispell-dictionary "english")

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
            #'(lambda (props)
               (let ((max-line-length (gethash 'max_line_length props)))
                 (cond
                  ((equal max-line-length "off")
                   (visual-line-mode 1)
                   (display-fill-column-indicator-mode -1))
                  (t
                   (visual-line-mode -1)
                   (display-fill-column-indicator-mode 1)))))))

(use-package flycheck
  :ensure t
  :init
  (setq-default flyspell-issue-message-flag nil)
  (global-flycheck-mode))


(use-package qt-c-style
  :hook '((c-mode-common . qt-set-c-style)
          (c-mode-common . qt-make-newline-indent)))


(provide 'linter)
;;; linter.el ends here
