;;; modes.el --- Load various modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joe Pearson

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
  (setq-default TeX-auto-save nil)
  (setq-default TeX-parse-self t)
  (setq-default TeX-source-correlate-mode t)
  (setq-default TeX-master nil)
  (setq-default bibtex-align-at-equal-sign t))


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

;; Language Server Protocol implementation for Emacs.
(use-package eglot
  :ensure t
  :hook
  (cc-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (dart-mode . eglot-ensure))

(use-package helm
  :ensure t
  :config
  (semantic-mode 1)
  :bind
  ("C-°" . helm-semantic-or-imenu)
  ("M-x" . helm-M-x))

(use-package helm-ag
  :ensure t
  :commands (helm-ag helm-projectile-ag)
  :init
  (setq helm-ag-insert-at-point 'symbol
        helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package helm-projectile
  :ensure t
  :bind ("M-p" . helm-projectile-find-file)
  :config (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package markdown-mode
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-globally-ignored-directories
          (append '("build" ".git")))))

;; Use `flyspell-prog-mode' to check comments in Python code.  The
;; variable `pyvenv-workon' is set automatically via dir-locals.
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-tracking-mode 1)
  :hook
  (python-mode . flyspell-prog-mode))

(use-package qml-mode
  :ensure t
  :init
  (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode)))

(use-package reftex
  :ensure t
  :bind (("C-c t" . reftex-toc)
         ("C-c c" . reftex-citation)
         ("C-c l" . reftex-label)
         ("C-c r" . reftex-reference)
         ("C-c v" . reftex-view-crossref)
         ("C-c g" . reftex-grep-document))
  :init
  (setq-default reftex-plug-into-AUCTeX t)
  (setq-default reftex-save-parse-info nil)
  (setq-default reftex-keep-temporary-buffers nil))

(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby")

;; Use `flyspell-prog-mode' to check comments in Rust code.
(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . flyspell-prog-mode))

;; display available keybindings in popup
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'modes)
;;; modes.el ends here
