;;; my-mode-line.el --- My mode-line settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'all-the-icons)
(require 'evil)

(defun -custom-modeline-github-vc ()
  "An `all-the-icons' segment for the Git Version Control icon."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1 :family ,(all-the-icons-octicon-family))
                 'display '(raise 0))
     (propertize (format " %s" branch))
     (propertize " "))))

(defun -custom-modeline-svn-vc ()
  "An `all-the-icons' segment for the SVN Version Control icon."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud"))
                 'face `(:height 1)
                 'display '(raise 0))
     (propertize (format " %s" revision) 'face `(:height 0.9)))))

(defvar my-mode-line-vc
  '(:propertize
    (:eval (when vc-mode
             (cond
              ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
              ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
              (t (format "%s" vc-mode)))))
    face mode-line-directory))

(defvar my-mode-line-modes-icon
  '(:propertize
    (:eval (let ((icon (all-the-icons-icon-for-buffer)))
             (unless (symbolp icon) ;; This implies it's the major mode
               (propertize icon
                           'help-echo (format "Major-mode: `%s`"
                                              major-mode)
                           'display '(raise 0.0)
                           'face `(:height 0.9 :family ,
                                           (all-the-icons-icon-family-for-buffer)
                                           :inherit)))))
    face mode-line-directory))

(setq-default mode-line-format
              (list
               " "
               '(:eval (propertize (cond ((equal (format "%s" evil-state) "insert") "I")
	                                 ((equal (format "%s" evil-state) "normal") "N")
	                                 ((equal (format "%s" evil-state) "visual") "V")
                                         ((equal (format "%s" evil-state) "emacs") "E")
	                                 (t (format "%s" evil-state)))
                                   'help-echo (format "Evil mode: %s" evil-state)))
               mode-line-front-space
               mode-line-mule-info
               mode-line-modified
               mode-line-frame-identification
               mode-line-buffer-identification
               "  "
               mode-line-position
               my-mode-line-vc
               "   "
               mode-line-modes
               mode-line-misc-info))

(provide 'my-mode-line)

;;; my-mode-line.el ends here
