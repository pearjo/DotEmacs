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

(use-package irony
  :ensure t
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

    ;; Optimize irony on windows
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))

    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

    ;; Use compilation database first, clang_complete as fallback.
    (setq-default irony-cdb-compilation-databases
                  '(irony-cdb-libclang irony-cdb-clang-complete))))

(use-package company-irony
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :after company
;;   :init
;;   (add-to-list 'company-backends
;;                '(company-irony-c-headers company-irony)))

;; (use-package helm-rtags
;;   :ensure t
;;   :init
;;   (setq rtags-display-result-backend 'helm))

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
