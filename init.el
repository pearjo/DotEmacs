;;; init.el --- emacs configuration file

;; Copyright (C) 2019 Joe Pearson

;; Author: Joe Pearson <joe.pearson@mail.de>
;; Keywords: emacs, init

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Emacs config file.

;;; Code:

;;; Editor configuration

;; Custom key bindings
(global-set-key (kbd "C-#") 'comment-line)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<tab>") 'helm-buffers-list)

;; Minimal UI
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq cursor-type 'box)
(column-number-mode 1)

;; Frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 90))

;; display line numbers
(add-hook 'after-make-frame-functions
	  '(lambda (frame)
	     (select-frame frame)
	     (if (version<= "26.0.50" emacs-version)
		 (global-display-line-numbers-mode)
	       (global-linum-mode 1))))

;; soft wrap lines
(global-visual-line-mode 1)

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

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Backup files
(setq create-lockfiles nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
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

;; Theme
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-use-less-bold t)
  (setq solarized-use-more-italic t)
  (setq solarized-emphasize-indicators nil)
  (setq solarized-scale-org-headlines nil)
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
                           (:sunset  . solarized-dark))))

;; load theme
(add-hook 'after-make-frame-functions
	  '(lambda (frame)
	     (select-frame frame)
	     (if (display-graphic-p)
		 (circadian-setup)
	       (add-to-list 'default-frame-alist '(tty-color-mode . -1)))))

;; set custom font
(add-hook 'after-make-frame-functions
	  '(lambda (frame)
	     (select-frame frame)
	     (when (member "Inconsolata" (font-family-list))
	       (add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
	       (set-face-attribute 'default nil :family "Inconsolata"))))

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

;; Tree view
(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :defer
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-fixed-size nil)
  (setq all-the-icons-color-icons t)
  (setq neo-smart-open t)
  ;; turn fci mode and line numbers off
  (add-hook 'neo-after-create-hook
            (lambda (&rest _)
	      (turn-off-fci-mode)
	      (display-line-numbers-mode -1)))
  (evil-set-initial-state 'neotree-mode 'normal)
  (evil-define-key 'normal neotree-mode-map
    (kbd "RET") 'neotree-enter
    (kbd "c")   'neotree-create-node
    (kbd "r")   'neotree-rename-node
    (kbd "d")   'neotree-delete-node
    (kbd "j")   'neotree-next-node
    (kbd "k")   'neotree-previous-node
    (kbd "g")   'neotree-refresh
    (kbd "C")   'neotree-change-root
    (kbd "I")   'neotree-hidden-file-toggle
    (kbd "H")   'neotree-hidden-file-toggle
    (kbd "q")   'neotree-hide
    (kbd "l")   'neotree-enter))

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

;; column indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  (add-hook 'circadian-after-load-theme-hook
	    '(lambda ()
	       turn-off-fci-mode
	       turn-on-fci-mode)))

;; make fci and company work together
(defun on-off-fci-before-company(command)
  "Show or hide fci with COMMAND before company."
  (when (string= "show" command)
    (turn-off-fci-mode))
  (when (string= "hide" command)
    (turn-on-fci-mode)))

(advice-add 'company-call-frontends :before #'on-off-fci-before-company)

;; autopair brackets and highlight them
(require 'autopair)
(autopair-global-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Helm
(use-package helm
  :ensure t
  :config
  (semantic-mode 1)
  :bind
  ("C-<dead-circumflex>" . helm-semantic))

;; move over camelCase words correctly
(subword-mode)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Dired
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(global-auto-revert-mode 1)


;;; Mode line
;; minimal ui of mode-line
(defun minimal-mode-line (frame)
  "Minimal mode-line for a FRAME."
  (select-frame frame)
  (set-face-attribute 'mode-line nil
		      :box nil
		      :overline nil
		      :underline nil)
  (set-face-attribute 'mode-line-inactive nil
		      :box nil
		      :overline nil
		      :underline nil))

(add-hook 'after-make-frame-functions 'minimal-mode-line)
(add-hook 'circadian-after-load-theme-hook 'minimal-mode-line)

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


;;; Spell checking

;; dictionary setup
(setq ispell-program-name "aspell")
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


;;--------------------------------------------------------------------
;;; Code checker

;; flycheckr
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;;--------------------------------------------------------------------
;;; TeX/LaTeX settings

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


;;--------------------------------------------------------------------
;;; C/C++

;; OpenFOAM
(c-add-style "OpenFOAM_HGW"
	     '(
     (c-basic-offset . 4)
     (c-tab-always-indent . t)
     (indent-tabs-mode . nil)
     (c-comment-only-line-offset . (0 . 0))
     (c-indent-comments-syntactically-p . t)
     (c-block-comments-indent-p nil)
     (c-cleanup-list .
	 '((defun-close-semi) (list-close-comma) (scope-operator)))
     (c-backslash-column . 48)
     (c-offsets-alist .
     (
     (c . +)                     ;; inside a multi-line C style block  comment
     (defun-open . 0)            ;; brace that opens a function definition
     (defun-close . 0)           ;; brace that closes a function definition
     (defun-block-intro . +)     ;; the first line in a top-level defun
     (class-open . 0)            ;; brace that opens a class definition
     (class-close . 0)           ;; brace that closes a class definition
     (inline-open . +)           ;; brace that opens an in-class inline method
     (inline-close . 0)          ;; brace that closes an in-class inline method
     (topmost-intro . 0)         ;; the first line in a topmost construct
				 ;; definition
     (topmost-intro-cont . 0)    ;; topmost definition continuation lines
     (member-init-intro . +)     ;; first line in a member initialization list
     (member-init-cont . 0)      ;; subsequent member initialization list lines
     (inher-intro . 0)           ;; first line of a multiple inheritance list
     (inher-cont . +)            ;; subsequent multiple inheritance lines
     (block-open . 0)            ;; statement block open brace
     (block-close . 0)           ;; statement block close brace
     (brace-list-open . 0)       ;; open brace of an enum or static array list
     (brace-list-close . 0)      ;; open brace of an enum or static array list
     (brace-list-intro . +)      ;; first line in an enum or static array list
     (brace-list-entry . 0)      ;; subsequent lines in an enum or static array
				 ;; list
     (statement . 0)             ;; a C/C++/ObjC statement
     (statement-cont . +)        ;; a continuation of a C/C++/ObjC statement
     (statement-block-intro . +) ;; the first line in a new statement block
     (statement-case-intro . +)  ;; the first line in a case `block'
     (statement-case-open . +)   ;; the first line in a case `block'
				 ;; starting with brace
     (substatement . +)          ;; the first line after an if/while/for/do/else
     (substatement-open . 0)     ;; the brace that opens a substatement block
     (case-label . +)            ;; a case or default label
     (access-label . -)          ;; C++ private/protected/public access label
     (label . -)                 ;; any non-special C/C++/ObjC label
     (do-while-closure . 0)      ;; the `while' that ends a do/while construct
     (else-clause . 0)           ;; the `else' of an if/else construct
     (comment-intro . 0)         ;; line containing only a comment introduction
     (arglist-intro . +)         ;; the first line in an argument list
     (arglist-cont . 0)          ;; subsequent argument list lines when no
				 ;; subsequent argument list lines
				 ;; when no the
				 ;; arglist opening paren
     (arglist-cont-nonempty . 0) ;; subsequent argument list lines when at
				 ;; subsequent argument list lines
				 ;; when at line
				 ;; as the arglist opening paren
     (arglist-close . 0)         ;; line as the arglist opening paren
     (stream-op . +)             ;; lines continuing a stream operator construct
     (inclass . +)               ;; the construct is nested inside a class
				 ;; definition
     (cpp-macro . +)             ;; the construct is nested inside a class
				 ;; definition
     (friend . 0)                ;; a C++ friend declaration
     (namespace-open  . 0)
     (namespace-close . 0)
     (innamespace     . 0)
     )
     )
     )
)

(defun openfoam-hgw-c-mode-hook ()
  "OpenFOAM C++ style."
  (c-set-style "OpenFOAM_HGW"))

;; check comments in C++ code
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

;;--------------------------------------------------------------------
;;; Python

;; elpy
(use-package elpy
  :ensure t
  :config
  (progn
    (elpy-enable)))

;; PEP8 Compliance
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; check comments in Python code
(add-hook 'python-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))


;;--------------------------------------------------------------------
;;; Version control

;; use magit
(use-package magit
  :ensure t)

(global-set-key (kbd "C-x g") 'magit-status)

;;; init.el ends here
