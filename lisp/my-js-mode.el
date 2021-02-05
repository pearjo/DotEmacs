;;; my-js-mode.el --- My JavaScript mode settings

;; Copyright (C) 2020  Joe Pearson

;; Author: Joe Pearson
;; Keywords: js

;;; Commentary:

;; My configuration for JavaScript and React development.

;;; Code:
(require 'use-package)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(add-to-list
 'js-server-programs
 '((js-mode typescript-mode)
   . ("typescript-language-server" "--stdio"))))

(provide 'my-js-mode)
;;; my-js-mode.el ends here
