;;; my-helm-mode.el --- My helm mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: helm

;;; Commentary:

;;; Code:
(require'use-package)

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
