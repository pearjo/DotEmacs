;;; my-cc-mode.el --- My CC mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: cc

;;; Commentary:

;;  Use `irony' for code completion, real-time syntax checking and
;;  live documentation.

;;; Code:
(require 'use-package)
(require 'cc-mode)

(use-package qt-c-style
  :init
  (defun qt-c-mode-hook ()
    "Set the Qt code style."
    (c-set-style "Qt"))
  (add-hook 'c-mode-common-hook 'qt-c-mode-hook)
  (add-hook 'c-mode-common-hook 'qt-set-c-style)
  (add-hook 'c-mode-common-hook 'qt-make-newline-indent))

(require 'qt-pro)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

;; check comments in C++ code
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

(provide 'my-cc-mode)
;;; my-cc-mode.el ends here
