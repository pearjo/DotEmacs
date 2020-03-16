;;; my-helm-mode.el --- My helm mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: helm

;;; Commentary:

;;; Code:
(require'use-package)

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
  :init (setq helm-ag-insert-at-point 'symbol
              helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-globally-ignored-directories
          (append '("build"
                    ".git")))))

(use-package helm-projectile
  :ensure t
  :bind ("M-p" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

(defun try-helm-projectile-find-file ()
  "Try to find file using `helm-projectile-find-file'.
If no project is found, `ido-find-file' is used instead."
  (interactive)
  (call-interactively
   (if (projectile-project-p)
       #'helm-projectile-find-file
       #'ido-find-file)))

(global-set-key (kbd "C-x C-f") 'try-helm-projectile-find-file)

(provide 'my-helm-mode)
;;; my-helm-mode.el ends here
