;;; my-dart-mode.el --- My dart mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: dart

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package dart-mode
  :ensure t
  :init
  (add-hook 'dart-mode-hook 'eglot-ensure))

(use-package flutter
  :ensure t
  :bind ("C-M-x" . #'flutter-run-or-hot-reload))

(provide 'my-dart-mode)
;;; my-dart-mode.el ends here
