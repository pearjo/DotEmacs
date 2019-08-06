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

;; Horizontal scrolling mouse events should actually scroll left and right.
(global-set-key (kbd "<mouse-6>") (lambda () (interactive)
				    (if truncate-lines (scroll-right 1))))
(global-set-key (kbd "<mouse-7>") (lambda () (interactive)
				    (if truncate-lines (scroll-left 1))))

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

;; set default font in initial window and for any new window
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (add-to-list 'initial-frame-alist '(font . "Consolas-11"))
  (add-to-list 'default-frame-alist '(font . "Consolas-11"))
  (setq face-height 100))
 ((string-equal system-type "darwin") ; macOS
  (add-to-list 'initial-frame-alist '(font . "Menlo"))
  (add-to-list 'default-frame-alist '(font . "Menlo"))
  (setq face-height 100))
 ((string-equal system-type "gnu/linux") ; linux
  (add-to-list 'initial-frame-alist '(font . "Inconsolata-14"))
  (add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
  (setq face-height 140)))

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

;; Helm
(use-package helm
  :ensure t
  :config
  (semantic-mode 1)
  :bind
  ("C-°" . helm-semantic-or-imenu))

(use-package helm-ag
  :ensure t)

;; move over camelCase words correctly
(subword-mode +1)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; Dired
;; use only one buffer for dired
(defadvice dired-advertised-find-file (around dired-subst-directory activate)
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

(setq dired-listing-switches "-lGh1v --group-directories-first")
(global-auto-revert-mode 1)

(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "<dead-circumflex>") 'dired-up-directory)))

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Column length waring
(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(setq whitespace-line-column nil)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'LaTeX-mode-hook 'whitespace-mode)

(defun get-frame-height ()
  "Calculate the frame height in lines.
The calculation is based on the font and display size."
  (unless (boundp 'font-size)
	  (setq font-size 10))
  (round (* (display-pixel-height)
	    0.75		  ; scale pixel to points
	    (/ 1.0 font-size)	  ; devide by font size to get number of lines
	    0.8			  ; use 80% of screen height
	    )))

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

(defun fit-frame-hook (frame)
  "Normal hook to fit the current FRAME using `fit-frame'.
The function `fit-frame' is called with a negative prefix argument to
set the width to `fill-column' + `fit-frame-fill-column-margin'."
  (interactive)
  (let ((current-prefix-arg -1)
        (select-frame frame))
    (call-interactively 'fit-frame)))

(defun center-frame-hook (frame)
  "Center the current FRAME on the monitor."
  (interactive)
  (set-frame-position (select-frame frame)
                      (- (/ (display-pixel-width) 2)
                         (/ (frame-pixel-width) 2))
                      (- (/ (display-pixel-height) 2)
                         (/ (frame-pixel-height) 2))))

(add-hook 'after-make-frame-functions 'fit-frame-hook)
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

;; ;; hide minor modes
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
  (setq ispell-program-name "hunspell.exe"))
 ((string-equal system-type "gnu/linux") ; linux
  (setq ispell-program-name "aspell"))
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
;;; C/C++
;;
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
     (innamespace     . 0)))))

(defun openfoam-hgw-c-mode-hook ()
  "OpenFOAM C++ style."
  (c-set-style "OpenFOAM_HGW"))

(add-hook 'c-mode-common-hook 'openfoam-hgw-c-mode-hook)

;; check comments in C++ code
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(defun create-etags (dir)
  "Create tags file for source files in DIR."
  (interactive "Ddirectory: ")
  (eshell-command
   (format "find %s -type f -name '*.[c,cpp,c++,C,h,H]' | etags -" dir)))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config
  (flycheck-clang-analyzer-setup))

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
(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-to-list 'company-backends 'company-robe))

(use-package flymake-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Visual Basic
;;
(require 'visual-basic-mode)

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
