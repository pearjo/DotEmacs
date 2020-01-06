;;; my-vbnet-mode.el --- My VB.Net mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: vbnet

;;; Commentary:

;;; Code:
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                                 vbnet-mode)) auto-mode-alist))

(provide 'my-vbnet-mode)
;;; my-vbnet-mode.el ends here
