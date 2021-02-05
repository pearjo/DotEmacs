;;; my-eglot-mode.el --- My eglot mode settings

;; Copyright (C) 2020  Joe Pearson

;; Author: Joe Pearson
;; Keywords: eglot

;;; Commentary:

;; Language Server Protocol implementation for Emacs

;;; Code:
(require 'use-package)

(use-package eglot
  :ensure t
  :hook
  (python-mode . eglot-ensure)
  (dart-mode . eglot-ensure))

(provide 'my-eglot-mode)
;;; my-eglot-mode.el ends here
