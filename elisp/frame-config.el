;;; frame-config.el --- Frame and window configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: frames

;; Package-Requires: ((use-package))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'use-package)

(set-face-attribute 'mode-line nil
		    :box nil
		    :overline nil
		    :underline nil)
(set-face-attribute 'mode-line-inactive nil
		    :box nil
		    :overline nil
		    :underline nil)
(global-display-fill-column-indicator-mode 1)
(add-to-list 'default-frame-alist '(height . 50))

(use-package all-the-icons
  :ensure t)

;; fit the frame to the `fill-column' and center it on the screen
(use-package autofit-frame
  :init
  (setq-default fit-frame-min-height 50)
  (setq-default fit-frame-fill-column-margin 20))

(defun autofit-frame-center-hook (frame)
  "Center the current FRAME on the monitor."
  (interactive)
  (set-frame-position (select-frame frame)
                      (- (/ (display-pixel-width) 2)
                         (/ (frame-pixel-width) 2))
                      (- (/ (display-pixel-height) 2)
                         (/ (frame-pixel-height) 2))))

;; (add-hook 'after-make-frame-functions 'fit-frame-hook)
(add-hook 'after-make-frame-functions 'autofit-frame-center-hook)

;; style the mode line
(define-minor-mode mode-line-minor-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'mode-line-minor-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))

(defun mode-line-my-github-vc ()
  "An `all-the-icons' segment for the Git Version Control icon."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (cond
      ((string-equal system-type "gnu/linux") ; GNU/Linux
       (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1 :family ,(all-the-icons-octicon-family))
                   'display '(raise 0)))
      ((string-equal system-type "windows-nt") ; Microsoft Windows
       (propertize (format "Git"))))
     (propertize (format " %s" branch))
     (propertize " "))))

(defun mode-line-my-svn-vc ()
  "An `all-the-icons' segment for the SVN Version Control icon."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format "SVN")
                 'face `(:height 1)
                 'display '(raise 0))
     (propertize (format " %s" revision) 'face `(:height 0.9)))))

(defvar mode-line-my-vc
  '(:propertize
    (:eval (when vc-mode
             (cond
              ((string-match "Git[:-]" vc-mode) (mode-line-my-github-vc))
              ((string-match "SVN-" vc-mode) (mode-line-my-svn-vc))
              (t (format "%s" vc-mode)))))
    face mode-line-directory))

(setq-default mode-line-format
              (list
               " "
               mode-line-front-space
               mode-line-mule-info
               mode-line-modified
               mode-line-frame-identification
               mode-line-buffer-identification
               "  "
               mode-line-position
               mode-line-my-vc
               "   "
               mode-line-modes
               mode-line-misc-info))

(provide 'frame-config)
;;; frame-config.el ends here
