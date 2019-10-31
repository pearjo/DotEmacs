;;; my-qml-mode.el --- My QML mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: qt, qml

;;; Commentary:

;;; Code:
(require'use-package)

;; (use-package company-qml
;;   :ensure t
;;   :after company
;;   :config
;;   (add-to-list 'company-backends 'company-qml))

(use-package qml-mode
  :ensure t
  :init
  (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
  (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode)))

(provide 'my-qml-mode)
;;; my-qml-mode.el ends here
