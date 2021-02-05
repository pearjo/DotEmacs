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

;; (use-package elpy
;;   :ensure t
;;   :commands elpy-enable
;;   :init (with-eval-after-load 'python (elpy-enable)))

;; ;; PEP8 Compliance
;; (use-package py-autopep8
;;   :ensure t
;;   :config
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; check comments in Python code
(add-hook 'python-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

(provide 'my-python-mode)
;;; my-python-mode.el ends here
