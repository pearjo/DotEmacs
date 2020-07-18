;;; my-dart-mode.el --- My dart mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: dart

;;; Commentary:
;;
;;  Note that there is a bug in the
;;  `lsp-dart-flutter-daemon--running-p' function of the
;;  lsp-dart-daemon.  Use `get-buffer' to check if a buffer already
;;  excists.

;;; Code:
(require 'use-package)

(use-package lsp-mode :ensure t)
(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp)
  :custom
  (lsp-dart-sdk-dir "/home/joe/flutter/bin/cache/dart-sdk"))

;; Optional Flutter packages
(use-package hover :ensure t) ;; run app from desktop without emulator

(use-package dart-mode
  :ensure t)

(use-package flutter
  :ensure t
  :bind ("C-M-x" . #'flutter-run-or-hot-reload)
  :custom
  (flutter-sdk-path "/home/joe/flutter"))

(provide 'my-dart-mode)
;;; my-dart-mode.el ends here
