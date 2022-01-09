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

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :init
  (unless (string-search "all-the-icons"
                         (shell-command-to-string (format "fc-list")))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :ensure t
  :if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
  :hook ((dired-mode . all-the-icons-dired-mode)))

;; Use a dark and light theme and change them corresponding to the
;; daylight.
(use-package dracula-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :init
  (setq-default solarized-use-variable-pitch nil))

;; load theme depending on the day time
(use-package circadian
  :ensure t
  :init
  ;; coordinates of Hamburg, HH, Germany
  (setq-default calendar-latitude 53.55
                calendar-longitude 9.99
                circadian-themes '((:sunrise . solarized-light)
                                   (:sunset  . dracula)))
  (circadian-setup)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (circadian-setup)))))

;; fit the frame to the `fill-column' and center it on the screen
(use-package autofit-frame
  :init
  (setq-default fit-frame-min-height 50
                fit-frame-fill-column-margin 20))

(defun autofit-frame-center-hook (frame)
  "Center the current FRAME on the monitor."
  (interactive)
  (set-frame-position (select-frame frame)
                      (- (/ (display-pixel-width) 2)
                         (/ (frame-pixel-width) 2))
                      (- (/ (display-pixel-height) 2)
                         (/ (frame-pixel-height) 2))))

(add-hook 'after-make-frame-functions 'autofit-frame-center-hook)

;; Use dired to browse directories and make it prettier.
(defadvice dired-advertised-find-file
    (around dired-subst-directory activate)
  "Replace current buffer if file is a directory."
  (interactive)
  (let ((orig (current-buffer))
        (filename (dired-get-filename)))
    ad-do-it
    (if (and (file-directory-p filename)
             (not (eq (current-buffer) orig)))
        (kill-buffer orig))))

(eval-after-load "dired"
  ;; don't remove `other-window', the caller expects it to be there
  '(defun dired-up-directory (&optional other-window)
     "Run Dired on parent directory of current directory."
     (interactive "P")
     (let* ((dir (dired-current-directory))
     	    (orig (current-buffer))
     	    (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
     	   ;; Only try dired-goto-subdir if buffer has more than one dir.
     	   (and (cdr dired-subdir-alist)
     		(dired-goto-subdir up))
     	   (progn
     	     (kill-buffer orig)
     	     (dired up)
     	     (dired-goto-file dir))))))

;; ls has different arguments on gnu/linux than on darwin
(cond
 ((eq system-type 'gnu/linux)
  (setq dired-listing-switches "-h1v --group-directories-first"))
 ((eq system-type 'darwin)
  (setq dired-use-ls-dired nil)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            (local-set-key (kbd "U") 'dired-up-directory)))

(define-minor-mode mode-line-minor-blackout-mode
  "Hides minor modes from the mode line."
  :init-value t)

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
      ((eq system-type 'windows-nt)
       (propertize (format "Git")))
      (t
       (propertize (format " %s" (all-the-icons-octicon "git-branch"))
                   'face `(:height 1 :family ,(all-the-icons-octicon-family))
                   'display '(raise 0))))
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
              ((string-match "Git[:-]" vc-mode)
               (mode-line-my-github-vc))
              ((string-match "SVN-" vc-mode)
               (mode-line-my-svn-vc))
              (t
               (format "%s" vc-mode)))))
    face mode-line-directory))

(setq-default mode-line-format (list
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
  (add-to-list 'initial-frame-alist '(font . "SF Mono Medium"))
  (add-to-list 'default-frame-alist '(font . "SF Mono Medium")))
 (t
  (add-to-list 'initial-frame-alist '(font . "Fira Code"))
  (add-to-list 'default-frame-alist '(font . "Fira Code"))))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(column-number-mode 1)
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (global-linum-mode 1))

(setq-default scroll-margin 5
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              mouse-wheel-follow-mouse 't
              mouse-wheel-progressive-speed nil
              fast-but-imprecise-scrolling nil
              scroll-step 1
              scroll-conservatively 10
              scroll-preserve-screen-position 1)
(setq-default inhibit-startup-screen t)
(setq-default cursor-type 'box)
(setq-default frame-title-format '("%f"))
(setq-default truncate-lines 1)

(set-face-attribute 'mode-line nil
		    :box nil
		    :overline nil
		    :underline nil)
(set-face-attribute 'mode-line-inactive nil
		    :box nil
		    :overline nil
		    :underline nil)

(add-to-list 'default-frame-alist '(height . 50))

(provide 'frame-config)
;;; frame-config.el ends here
