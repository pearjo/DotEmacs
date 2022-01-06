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

;; For macOS use "Monaco" since its the most beautiful font, but not
;; available on all platforms.
(cond
 ((eq system-type 'darwin)
  (add-to-list 'initial-frame-alist '(font . "Monaco"))
  (add-to-list 'default-frame-alist '(font . "Monaco")))
 (t
  (add-to-list 'initial-frame-alist '(font . "Fira Code"))
  (add-to-list 'default-frame-alist '(font . "Fira Code"))))

;; minimal frame UI settings
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq cursor-type 'box)
(column-number-mode 1)
(setq-default frame-title-format '("%f"))

;; display line numbers
(add-hook 'after-make-frame-functions
	  '(lambda (frame)
	     (select-frame frame)
	     (if (version<= "26.0.50" emacs-version)
		 (global-display-line-numbers-mode)
	       (global-linum-mode 1))))

(if (version<= "26.0.50" emacs-version)
		 (global-display-line-numbers-mode)
	       (global-linum-mode 1))

;; line wrapping
(set-default 'truncate-lines 1)

;; smooth scrolling
(setq scroll-margin 5
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      fast-but-imprecise-scrolling nil
      scroll-step 1
      scroll-conservatively 10
      scroll-preserve-screen-position 1)

;; Horizontal scrolling mouse events should actually scroll left and
;; right.
(global-set-key (kbd "<mouse-6>") (lambda ()
                                    (interactive)
				    (if truncate-lines
                                        (scroll-right 1))))
(global-set-key (kbd "<mouse-7>") (lambda ()
                                    (interactive)
				    (if truncate-lines
                                        (scroll-left 1))))

(use-package dracula-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil))

;; load theme depending on the day time
(use-package circadian
  :ensure t
  :init
  ;; coordinates of Hamburg, HH, Germany
  (setq calendar-latitude 53.55)
  (setq calendar-longitude 9.99)
  (setq circadian-themes '((:sunrise . solarized-light)
                           (:sunset  . dracula)))
  (circadian-setup)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (circadian-setup)))))

(provide 'frame-config)
;;; frame-config.el ends here
