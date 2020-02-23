;;; my-dart-mode.el --- My dart mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: dart

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package dart-mode
  :ensure t)

(use-package pos-tip
  :ensure t)

(use-package company-dart)

(add-hook 'dart-mode-hook (lambda ()
 (set (make-local-variable 'company-backends)
  '(company-dart (company-dabbrev company-yankpad)))))

(provide 'my-dart-mode)
;;; my-dart-mode.el ends here
