;;; my-fonts.el --- My font settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: fonts

;;; Commentary:

;;  For different operation systems, different fonts are loaded and
;;  installed.

;;; Code:
(cond
 ((string-equal system-type "darwin")
  (add-to-list 'initial-frame-alist '(font . "Monaco"))
  (add-to-list 'default-frame-alist '(font . "Monaco")))
 (t
  (add-to-list 'initial-frame-alist '(font . "Fira Code"))
  (add-to-list 'default-frame-alist '(font . "Fira Code"))))

(provide 'my-fonts)
;;; my-fonts.el ends here
