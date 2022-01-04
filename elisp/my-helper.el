;;; my-helper.el --- My helper functions

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: helper functions

;;; Commentary:

;;; Code:

(defun toggle-camelcase-snakecase ()
  "Toggle between camelcase and snakecase notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(provide 'my-helper)
;;; my-helper.el ends here
