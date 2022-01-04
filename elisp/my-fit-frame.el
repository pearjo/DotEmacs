;;; my-fit-frame.el --- My CC mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: fit-frame

;;; Commentary:

;;; Code:

;; fit frame to buffer
(require 'autofit-frame)
(setq-default fit-frame-min-height 50)
(setq-default fit-frame-fill-column-margin 20)

(defun center-frame-hook (frame)
  "Center the current FRAME on the monitor."
  (interactive)
  (set-frame-position (select-frame frame)
                      (- (/ (display-pixel-width) 2)
                         (/ (frame-pixel-width) 2))
                      (- (/ (display-pixel-height) 2)
                         (/ (frame-pixel-height) 2))))

;; (add-hook 'after-make-frame-functions 'fit-frame-hook)
(add-hook 'after-make-frame-functions 'center-frame-hook)

(provide 'my-fit-frame)
;;; my-fit-frame.el ends here
