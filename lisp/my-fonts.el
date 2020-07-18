;;; my-fonts.el --- My font settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: fonts

;;; Commentary:

;;  For different operation systems, different fonts are loaded and
;;  installed.

;;; Code:
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (add-to-list 'initial-frame-alist '(font . "Consolas-11"))
  (add-to-list 'default-frame-alist '(font . "Consolas-11")))
 ((string-equal system-type "darwin") ; macOS
  (add-to-list 'initial-frame-alist '(font . "Monaco-11"))
  (add-to-list 'default-frame-alist '(font . "Monaco-11")))
 ((string-equal system-type "gnu/linux") ; linux
  (add-to-list 'initial-frame-alist
               '(font . "Ubuntu Mono-14"))
  (add-to-list 'default-frame-alist
               '(font . "Ubuntu Mono-14"))))

(provide 'my-fonts)
;;; my-fonts.el ends here
