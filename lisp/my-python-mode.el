;;; my-python-mode.el --- My python mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: python

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package pyvenv
  :ensure t
  :config
  ;; (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

;; check comments in Python code
(add-hook 'python-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

(provide 'my-python-mode)
;;; my-python-mode.el ends here
