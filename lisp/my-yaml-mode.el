;;; my-yaml-mode.el --- My yaml mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: yaml

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'my-yaml-mode)
;;; my-yaml-mode.el ends here
