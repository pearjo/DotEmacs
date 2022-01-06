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

(load-library "linter")
(load-library "modes")
(load-library "convenience")
(load-library "utils")
(load-library "frame-config")
(load-library "keys")
(if (string-equal system-type "darwin")
    (load "darwin-config"))

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
