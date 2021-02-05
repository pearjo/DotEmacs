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
(require 'eglot)
(require 'use-package)

(add-to-list 'eglot-server-programs
             (list 'dart-mode
                   (executable-find "dart")
                   (expand-file-name
                    "snapshots/analysis_server.dart.snapshot"
                    (file-name-directory
                     (file-truename
                      (executable-find "dart"))))
                   "--lsp"))

(use-package dart-mode
  :ensure t
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up
                 "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up
                 "BUILD")))

(provide 'my-dart-mode)
;;; my-dart-mode.el ends here
