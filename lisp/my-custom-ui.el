;;; my-custom-ui.el --- My custom UI settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson

;;; Commentary:

;;; Code:
(require 'use-package)

;; minimal UI settings
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

(use-package doom-themes
  :ensure t
  :defer
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package solarized-theme
  :ensure t)

;; load theme depending on the day time
(use-package circadian
  :ensure t
  :config
  ;; coordinates of Hamburg, HH, Germany
  (setq calendar-latitude 53.55)
  (setq calendar-longitude 9.99)
  (setq circadian-themes '((:sunrise . solarized-light)
                           (:sunset  . solarized-dark)))
  (circadian-setup))

(add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (circadian-setup))))

(provide 'my-custom-ui)
;;; my-custom-ui.el ends here
