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
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(add-to-list 'load-path "~/.emacs.d/submodules/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

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

(cond
 ((string-equal system-type "gnu/linux") ; GNU/Linux
  (setq dired-listing-switches "-lh1v --group-directories-first"))
 ((string-equal system-type "darwin") ; macOS
  (setq dired-use-ls-dired nil)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (local-set-key (kbd "U") 'dired-up-directory)))

(use-package all-the-icons-dired
  :ensure t
  :config
  (cond
   ((string-equal system-type (or "darwin" "gnu/linux"))
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))

(load-library "linter")
(load-library "modes")
(load-library "convenience")
(load-library "utils")
(load-library "frame-config")
(load-library "keys")
(if (string-equal system-type "darwin")
    (load "darwin-config"))

(load "my-fonts")

;; backup files
(setq create-lockfiles nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/")))

(recentf-mode 1) ;; keep a list of recently opened files
(delete-selection-mode 1) ;; overwrite selected region
(global-auto-revert-mode 1)
(global-subword-mode +1) ;; move over camelCase words correctly
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; init.el ends here
