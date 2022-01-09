;;; convenience.el --- My features for editing faster  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: convenience, languages

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

;; This library provides some convenience features to make editing
;; faster.  Thus, `company' is used for auto-completion and `helm' for
;; various tasks like e.g. changing buffers or finding files.

;;; Code:
(require 'use-package)

;; pairs brackets and highlights them
(use-package autopair
  :config
  (autopair-global-mode)
  (show-paren-mode 1)
  (setq-default show-paren-delay 0))

(use-package company
  :ensure t
  :defer t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-dabbrev-downcase nil)
  :config
  (setq-default company-backends
                '(company-files
                  company-keywords
                  company-dabbrev-code
                  company-dabbrev)))

(use-package company-auctex
  :ensure t
  :after company
  :hook ((LaTeX-mode . company-auctex-init)))

(use-package company-reftex
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))

;; language Server Protocol implementation for Emacs
(use-package eglot
  :ensure t
  :hook ((cc-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (dart-mode . eglot-ensure)))

(use-package helm
  :ensure t
  :bind (("C-°" . helm-semantic-or-imenu)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-projectile-try-find-file))
  :config
  (semantic-mode 1)
  (defun helm-projectile-try-find-file ()
    "Try to find file using `helm-projectile-find-file'.
If no project is found, `ido-find-file' is used instead."
    (interactive)
    (call-interactively
     (if (projectile-project-p)
         #'helm-projectile-find-file
       #'ido-find-file))))

(use-package helm-ag
  :ensure t
  :after helm
  :commands (helm-ag helm-projectile-ag)
  :init
  (setq-default helm-ag-insert-at-point 'symbol
                helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package helm-bibtex
  :ensure t
  :after helm)

(use-package helm-projectile
  :ensure t
  :after helm
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq-default projectile-enable-caching t
                projectile-globally-ignored-directories
                (append '("build" ".git" "submodules"))))

(use-package robe
  :ensure t
  :after company
  :bind ("C-M-." . robe-jump)
  :hook ruby-mode
  :init
  (push 'company-robe company-backends))

;; useful to jump faster back in time
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package yasnippet
  :ensure t
  :after company
  :config
  (yas-global-mode 1)
  (add-hook 'Snippet-mode 'require-final-newline nil)
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    "Fix things for BACKEND."
    (if (or (not company-mode/enable-yas)
	    (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend)
                  backend
                (list backend))
              '(:with company-yasnippet))))
  (setq-default yas-indent-line 'auto
                company-backends
                (mapcar #'company-mode/backend-with-yas
                        company-backends)))

(provide 'convenience)
;;; convenience.el ends here
