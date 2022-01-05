;;; init.el --- My Emacs Initialization File  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: convenience

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

;;; Code:
(require 'iso-transl) ;; dead keys

;; Custom key bindings
(global-set-key (kbd "C-#") 'comment-line)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<tab>") 'helm-buffers-list)

;; kill buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Load path
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Backup files
(setq create-lockfiles nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/")))
;; (setq make-backup-files nil)

;; Keep a list of recently opened files
(recentf-mode 1)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Editor config
(use-package editorconfig
  :ensure t
  :config
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

;; icon set
(use-package all-the-icons
  :ensure t)

;; Use the undo-tree
(use-package undo-tree
  :ensure t
  ;; :diminish undo-tree-mode
  ;; :init
  :config
  (global-undo-tree-mode 1))

;; Overwrite selected region
(delete-selection-mode 1)

;; auto complete using company
(use-package company
  :ensure t
  :custom
  (global-company-mode t)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-dabbrev-downcase nil)
  :config
  (progn
    ;; Enable company mode in every programming mode
    (add-hook 'prog-mode-hook 'company-mode)
    ;; Set my own default company backends
    (setq-default
     company-backends
     '(company-files
       company-keywords
       company-dabbrev-code
       company-dabbrev))))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-indent-line 'auto)
  (add-hook 'Snippet-mode 'require-final-newline nil))

;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Fix things for BACKEND."
  (if (or (not company-mode/enable-yas)
	  (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends
      (mapcar #'company-mode/backend-with-yas company-backends))

;; autopair brackets and highlight them
(require 'autopair)
(autopair-global-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; move over camelCase words correctly
(global-subword-mode +1)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Dired
;; use only one buffer for dired
(defadvice dired-advertised-find-file
    (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-filename)))
    ad-do-it
    (when (and (file-directory-p filename)
               (not (eq (current-buffer) orig)))
      (kill-buffer orig))))

(eval-after-load "dired"
  ;; don't remove `other-window', the caller expects it to be there
  '(defun dired-up-directory (&optional other-window)
     "Run Dired on parent directory of current directory."
     (interactive "P")
     (let* ((dir (dired-current-directory))
     	    (orig (current-buffer))
     	    (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
     	   ;; Only try dired-goto-subdir if buffer has more than one dir.
     	   (and (cdr dired-subdir-alist)
     		(dired-goto-subdir up))
     	   (progn
     	     (kill-buffer orig)
     	     (dired up)
     	     (dired-goto-file dir))))))

;; Make OS specific changes
(cond
 ((string-equal system-type "gnu/linux") ; GNU/Linux
  (setq dired-listing-switches "-lh1v --group-directories-first"))
 ((string-equal system-type "darwin") ; macOS
  (setq dired-use-ls-dired nil)))

(global-auto-revert-mode 1)

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (local-set-key (kbd "U") 'dired-up-directory)))

(use-package all-the-icons-dired
  :ensure t
  :config
  (cond
   ((string-equal system-type (or "darwin" "gnu/linux")) ; GNU/Linux
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))

;; Column length warning
(global-display-fill-column-indicator-mode 1)

(add-to-list 'default-frame-alist
	     '(height . 50))

(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;
;;
;;; Mode line
;;
;; minimal ui of mode-line
(set-face-attribute 'mode-line nil
		    :box nil
		    :overline nil
		    :underline nil)
(set-face-attribute 'mode-line-inactive nil
		    :box nil
		    :overline nil
		    :underline nil)

;; hide minor modes
(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'minor-mode-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))

(global-set-key (kbd "C-c m") 'minor-mode-blackout-mode)

(use-package my-mode-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Spell checking
;;
;; dictionary setup

(defun fd-switch-dictionary()
  "Change dictionaries.
Switch between English and German."
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key (kbd "C-d")   'fd-switch-dictionary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TeX/LaTeX
;;
;; AUCTeX

(use-package company-auctex
  :ensure t
  :defer t
  :hook ((LaTeX-mode . company-auctex-init)))

(use-package company-reftex
  :ensure t
  :init (progn
          (add-to-list 'company-backends 'company-reftex-labels)
	  (add-to-list 'company-backends 'company-reftex-citations)))

;; BibTeX for Helm
(use-package helm-bibtex
  :ensure t)

;; nomenclature for latex
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Nomenclature" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                  (lambda (name command file)
                    (TeX-run-compile name command file)
                    (TeX-process-set-variable file 'TeX-command-next
                                              TeX-command-default))
                  nil t :help "Create nomenclature file")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Ruby
;;
;; robe
;; NOTE: To install robe, pry and pry-doc needs to be installed
;;       using gem.
(use-package robe
  :ensure t
  :bind ("C-M-." . robe-jump)
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (push 'company-robe company-backends))

(load-library "linter")
(load-library "modes")
(load-library "utils")
(if (string-equal system-type "darwin")
    (load "darwin-config"))

(require 'utils)
(global-set-key "\C-c\C-k" 'copy-line)
(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-<down>") 'move-line-region-down)

;; Load my settings
(load "my-cc-mode")
(load "my-custom-ui")
(load "my-fit-frame")
(load "my-fonts")
(load "my-helm-mode")
(load "my-vbnet-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Maintain emacs
;;
;; update packages
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;;; init.el ends here
