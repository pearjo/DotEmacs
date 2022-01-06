;;; modes.el --- Load various modes  -*- lexical-binding: t; -*-

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

;; This library loads various modes use for various languages and
;; configures them.  All modes here are installed automatically by
;; `use-package' if not yet installed.

;;; Code:
(require 'use-package)

(use-package auctex
  :defer t
  :ensure t
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex))
  :init
  (setq-default TeX-auto-save nil
                TeX-parse-self t
                TeX-source-correlate-mode t
                TeX-master nil
                bibtex-align-at-equal-sign t)
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list
                  '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                    (lambda (name command file)
                      (TeX-run-compile name command file)
                      (TeX-process-set-variable file 'TeX-command-next
                                                TeX-command-default))
                    nil t :help "Create nomenclature file"))))


;; There is a bug in the `lsp-dart-flutter-daemon--running-p' function
;; of the lsp-dart-daemon.  Use `get-buffer' to check if a buffer
;; already exists.
(use-package dart-mode
  :ensure t
  :after eglot
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up
                 "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up
                 "BUILD"))
  (add-to-list 'eglot-server-programs
               (list 'dart-mode
                     (executable-find "dart")
                     (expand-file-name
                      "cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot"
                      (file-name-directory
                       (file-truename
                        (executable-find "dart"))))
                     "--lsp")))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :ensure t)

;; Use `flyspell-prog-mode' to check comments in Python code.  The
;; variable `pyvenv-workon' is set automatically via dir-locals.
(use-package pyvenv
  :ensure t
  :hook (python-mode . flyspell-prog-mode)
  :config
  (pyvenv-tracking-mode 1))

(use-package qml-mode
  :ensure t)

(use-package qt-pro-mode
  :ensure t
  :mode ("\\.pro\\'" "\\.pri\\'"))

(use-package reftex
  :ensure t
  :bind (("C-c t" . reftex-toc)
         ("C-c c" . reftex-citation)
         ("C-c l" . reftex-label)
         ("C-c r" . reftex-reference)
         ("C-c v" . reftex-view-crossref)
         ("C-c g" . reftex-grep-document))
  :init
  (setq-default reftex-plug-into-AUCTeX t
                reftex-save-parse-info nil
                reftex-keep-temporary-buffers nil))

(use-package ruby-mode
  :ensure t
  :mode ("\\.rb\\'" "Rakefile\\'" "Gemfile\\'" "Berksfile\\'"
         "Vagrantfile\\'")
  :interpreter "ruby")

;; use `flyspell-prog-mode' to check comments in Rust code
(use-package rust-mode
  :ensure t
  :hook (rust-mode . flyspell-prog-mode))

(use-package vbnet-mode
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)$")

;; display available keybindings in popup
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :init
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'modes)
;;; modes.el ends here
