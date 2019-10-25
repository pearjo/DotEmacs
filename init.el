;;; init.el --- emacs configuration file
;;
;; Copyright (C) 2019 Joe Pearson
;;
;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: emacs, init
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Emacs config file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;: Editor configuration
;;
;; dead keys
(require 'iso-transl)

;; Custom key bindings
(global-set-key (kbd "C-#") 'comment-line)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<tab>") 'helm-buffers-list)
(global-set-key (kbd "C-c $") 'toggle-truncate-lines)

;; Minimal UI
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq cursor-type 'box)
(column-number-mode 1)
(setq-default frame-title-format '("%f"))

;; display line numbers
(add-hook 'after-make-frame-functions
	  '(lambda (frame)
	     (select-frame frame)
	     (if (version<= "26.0.50" emacs-version)
		 (global-display-line-numbers-mode)
	       (global-linum-mode 1))))

;; line wrapping
;; (global-visual-line-mode 1)
(set-default 'truncate-lines 1)

;; kill buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; smooth scrolling
(setq scroll-margin 5
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      fast-but-imprecise-scrolling nil
      scroll-step 1
      scroll-conservatively 10
      scroll-preserve-screen-position 1)

;; Horizontal scrolling mouse events should actually scroll left and
;; right.
(global-set-key (kbd "<mouse-6>") (lambda ()
                                    (interactive)
				    (if truncate-lines
                                        (scroll-right 1))))
(global-set-key (kbd "<mouse-7>") (lambda ()
                                    (interactive)
				    (if truncate-lines
                                        (scroll-left 1))))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(let ((default-directory "~/.emacs.d/lisp/"))
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

;; Extensible vi layer for Emacs
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-c") 'copy-region-as-kill)
  (define-key evil-insert-state-map (kbd "C-v") 'yank)
  (define-key evil-insert-state-map (kbd "C-x") 'kill-region)
  (define-key evil-insert-state-map (kbd "C-z") 'undo)
  (define-key evil-insert-state-map (kbd "C-S-z") 'undo-tree-redo)
  (define-key evil-insert-state-map (kbd "C-a") 'mark-whole-buffer)
  ;; esc quits
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape]
    'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape]
    'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape]
    'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape]
    'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape]
    'minibuffer-keyboard-quit))

;; make evil and magit work together
;; (use-package evil-magit
;;   :ensure t)

;; Theme
(use-package solarized-theme
  :ensure t
  :defer
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italic t)
  (setq solarized-emphasize-indicators nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line nil)
  (setq solarized-distinct-fringe-background nil)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0))

;; load theme depending on the day time
(use-package circadian
  :ensure t
  :config
  ;; coordinates of Hamburg, HH, Germany
  (setq calendar-latitude 53.55)
  (setq calendar-longitude 9.99)
  (setq circadian-themes '((:sunrise . solarized-light)
                           (:sunset  . solarized-dark)))
  (circadian-setup))

;; Editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; icon set
(use-package all-the-icons
  :ensure t
  :config
  (require 'font-lock+))

;; Use the undo-tree
(use-package undo-tree
  :ensure t
  ;; :diminish undo-tree-mode
  ;; :init
  :config
  (global-undo-tree-mode 1))

;; Overwrite selected region
(delete-selection-mode 1)

;; Move lines and regions
(defun move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun move-region (start end n)
  "Move the current region from START to END up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current region from START to END up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current region form START to END down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  "Move a line or region from START to END up by N lines."
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up)))

(defun move-line-region-down (&optional start end n)
  "Move a line or region from START to END down by N lines."
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down)))

(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-<down>") 'move-line-region-down)

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
(subword-mode +1)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Dired
;; load dired+
(use-package dired+)

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

;; Change dired listing only for linux
(cond
 ((string-equal system-type "gnu/linux") ; GNU/Linux
  (setq dired-listing-switches "-lGh1v --group-directories-first")))
(global-auto-revert-mode 1)

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "U") 'dired-up-directory)
            (set-variable 'font-lock-maximum-decoration nil)))

(use-package all-the-icons-dired
  :ensure t
  :config
  (cond
   ((string-equal system-type "gnu/linux") ; GNU/Linux
    (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))))

;; Column length waring
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(setq whitespace-line-column nil)

(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (interactive)
             (whitespace-mode 0)
             (setq whitespace-line-column fill-column)
             (whitespace-mode 1)))

;; (defun get-frame-height ()
;;   "Calculate the frame height in lines.
;; The calculation is based on the font and display size."
;;   (unless (boundp 'font-size)
;; 	  (setq font-size 10))
;;   (round (* (display-pixel-height)
;; 	    0.75		  ; scale pixel to points
;; 	    (/ 1.0 font-size)	  ; devide by font size to get number of lines
;; 	    0.8			  ; use 80% of screen height
;; 	    )))

;; (add-hook 'after-make-frame-functions
;; 	  '(lambda (frame)
;; 	     (select-frame frame)
;; 	     ;; fit frame height to display size
;; 	     (setq frame-height (get-frame-height))
;; 	     (message "frame height is set to %s" frame-height)
;; 	     (add-to-list 'default-frame-alist '(height . 55))

;; 	     ;; fit frame to buffer
;; 	     (require 'autofit-frame)
;; 	     (setq-default fit-frame-min-height 55)))
;; (message "display size is %s" (display-pixel-height))

;; (setq frame-height (get-frame-height))
;; (message "frame height is set to %s" frame-height)

(add-to-list 'default-frame-alist
	     '(height . 50))

;; fit frame to buffer
(require 'autofit-frame)
(setq-default fit-frame-min-height 50)

;; (defun fit-frame-hook (frame)
;;   "Normal hook to fit the current FRAME using `fit-frame'.
;; The function `fit-frame' is called with a negative prefix argument to
;; set the width to `fill-column' + `fit-frame-fill-column-margin'."
;;   (interactive)
;;   (let ((current-prefix-arg -1)
;;         (select-frame frame))
;;     (call-interactively 'fit-frame)))

(defun center-frame-hook (frame)
  "Center the current FRAME on the monitor."
  (interactive)
  (set-frame-position (select-frame frame)
                      (- (/ (display-pixel-width) 2)
                         (/ (frame-pixel-width) 2))
                      (- (/ (display-pixel-height) 2)
                         (/ (frame-pixel-height) 2))))

;; (add-hook 'after-make-frame-functions 'fit-frame-hook)
(add-hook 'after-make-frame-functions 'center-frame-hook)

(put 'downcase-region 'disabled nil)

(defun sudo-save ()
  "Save file as sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:"
                          (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:"
                        buffer-file-name))))

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
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (message "Using hunspell.exe as spell checker")
  (setq ispell-program-name "hunspell.exe"))
 ((string-equal system-type "gnu/linux") ; linux
  (message "Using aspell as spell checker")
  (setq ispell-program-name "aspell")))
(setq ispell-really-aspell t)
(setq ispell-extra-args '("--sug-mode=fast"))
(setq ispell-list-command "--list")
(setq ispell-dictionary "english")
(setq flyspell-issue-message-flag nil)

;; flyspell in other modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

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
;;; Code checker
;;
;; flycheckr
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TeX/LaTeX
;;
;; AUCTeX
(use-package auctex
  :defer t
  :ensure t
  :config
  ;; RefTeX
  (use-package reftex
    :ensure t)
  ;; LaTeX hooks
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; (add-hook 'LaTeX-mode-hook 'reftex-initialize-temporary-buffers)
  :init (progn
	  (setq-default TeX-auto-save nil)
	  (setq-default TeX-parse-self t)
	  (setq-default TeX-source-correlate-mode t)
	  (setq-default TeX-master nil)
          ;; (setq-default preview-transparent-color fffff)
	  ;; BibTeX settings
	  (setq-default bibtex-align-at-equal-sign t)
	  ;; RefTeX settings
	  (setq-default reftex-plug-into-AUCTeX t)
	  (setq-default reftex-save-parse-info nil)
	  (setq-default reftex-keep-temporary-buffers nil))
  :bind
  ("C-c t" . reftex-toc)
  ("C-c c" . reftex-citation)
  ("C-c l" . reftex-label)
  ("C-c r" . reftex-reference)
  ("C-c v" . reftex-view-crossref)
  ("C-c g" . reftex-grep-document))

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
;;; Python
;;
;; elpy
(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable)))

;; PEP8 Compliance
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; check comments in Python code
(add-hook 'python-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Ruby
;;
;; robe
;; NOTE: To install robe, pry and pry-doc needs to be installed
;;       using gem.
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby")

(use-package robe
  :ensure t
  :bind ("C-M-." . robe-jump)
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (push 'company-robe company-backends))

(use-package flymake-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook #'rubocop-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; TypeScript
;;
(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Version control
;;
;; use magit
(use-package magit
  :ensure t)

(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Org mode
;;
(setq org-agenda-files (quote
                        ("~/org/tasks.org"
                         "~/org/notes/")))


;; Load my settings
(load "my-fonts")
(load "my-cc-mode")
(load "my-qml-mode")
(load "my-helm-mode")
(load "my-yaml-mode")
;; (load "my-mmm-mode")

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
