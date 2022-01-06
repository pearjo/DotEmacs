;;; keys.el --- My keys bindings  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: languages

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
(require 'iso-transl) ;; dead keys

(global-set-key (kbd "C-#") 'comment-line)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<tab>") 'helm-buffers-list)
(global-set-key (kbd "C-c C-k") 'copy-line)
(global-set-key (kbd "C-c m") 'minor-mode-blackout-mode)
(global-set-key (kbd "M-<down>") 'move-line-region-down)
(global-set-key (kbd "M-<up>") 'move-line-region-up)

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

(provide 'keys)
;;; keys.el ends here
