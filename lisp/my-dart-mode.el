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


(use-package dart-mode
  :ensure t
  :after eglot
  :init
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up
                 "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up
                 "BUILD"))
  (add-to-list 'eglot-server-programs
               (list 'dart-mode
                     (executable-find "dart")
                     (expand-file-name
                      "cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot"
                      (file-name-directory
                       (file-truename
                        (executable-find "dart"))))
                     "--lsp")))

(provide 'my-dart-mode)
;;; my-dart-mode.el ends here
