;;; init.el --- emacs configuration file
;;; Code:
;; Custom key bindings
(global-set-key (kbd "C-#") 'comment-line)
(global-set-key (kbd "C-c z") 'undo)

;; Minimal UI
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
  :bind (("C-c t" . neotree-toggle))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-complete-config ac-math auto-complete helm use-package neotree general evil doom-themes auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
