;;; init.el --- emacs configuration file
;;; Commentary:
;; Emacs config of J. N. P.
;;; Code:
;; Custom key bindings
(global-set-key (kbd "C-#") 'comment-line)
(global-set-key (kbd "C-c z") 'undo)

;; Minimal UI
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

  ;; Theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

;; Icons
(use-package all-the-icons
  :ensure t)

;; Tree view
(use-package neotree
  :ensure t
  :bind (("C-<tab>" . neotree-toggle))
  :defer
  :config
  (setq neo-theme 'icons)
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

;; AUCTeX
(use-package auctex
  :defer t
  :ensure t
  :config
  ;; RefTeX
  (use-package reftex
    :ensure t
    :defer t
    :config
    (setq reftex-cite-prompt-optional-args t))
  ;; LaTeX setup
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; auto-complete
(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-modes 'latex-mode)
  ;; LaTeX setup
  (use-package ac-math
    :ensure t
    :config
    (defun custom-ac-latex-mode ()
      (setq ac-sources
	    (append '(ac-source-math-unicode
		      ac-source-math-latex
		      ac-source-latex-commands)
		    ac-sources)))
    (add-hook 'LaTeX-mode-hook 'custom-ac-latex-mode)
    (setq ac-math-unicode-in-math-p t)
    (ac-flyspell-workaround)
    (add-to-list 'ac-modes 'org-mode)
    (require 'auto-complete-config)
    (ac-config-default)
    (setq ac-auto-start t)
    (setq ac-auto-show-menu t))
  (global-auto-complete-mode t))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; column indicator
(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook 'fci-mode))

;; set custom font
;; (when (member "Droid Sans Mono" (font-family-list))
;;   (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-10"))
;;   (set-face-attribute 'default nil :family "Droid Sans Mono-10"))
(when (member "Hack" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Hack-12"))
  (set-face-attribute 'default nil :family "Hack"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for OpenFOAM
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
;; (add-hook 'c-mode-common-hook 'openfoam-hgw-c-mode-hook)

;; backup files in emacs dict
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; smooth scrolling
(setq scroll-conservatively 10)
(setq scroll-margin 5)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;; init.el ends here
