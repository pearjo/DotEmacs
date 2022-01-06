;;; utils.el --- Utility functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021,2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>
;; Keywords: convenience, tools

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

;;; Code:

(defun copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun create-etags ()
  "Create tag file."
  (defvar output-directory)
  (setq output-directory
        (if (projectile-project-p)
            (projectile-project-p)
          (default-directory)))
   (cond
    ((string-equal system-type "windows-nt") ; Microsoft Windows
     (defvar win-directory)
     (setq win-directory
           (replace-regexp-in-string "/" "\\" output-directory t t))
     (async-shell-command
      (concat
       "dir " win-directory " /s/b | "
       "findstr \""
       ".*ad[absm]$ .*[CFHMSacfhlmpsty]$ .*def$ .*in[cs]$ .*s[as]$ "
       ".*src$ .*cc$ .*hh$ .*[chy]++$ .*[ch]pp$ .*[chy]xx$ .*pdb$ "
       ".*[ch]s$ .*[Cc][Oo][Bb]$ .*[eh]rl$ .*f90$ .*for$ .*java$ "
       ".*[cem]l$ .*clisp$ .*lisp$ .*[Ll][Ss][Pp]$ [Mm]akefile "
       ".*pas$ .*[Pp][LlMm]$ .*psw$ .*lm$ .*pc$ .*prolog$ .*oak$ "
       ".*p[sy]$ .*sch$ .*scheme$ .*[Ss][Cc][Mm]$ .*[Ss][Mm]$ .*bib$ "
       ".*cl[os]$ .*ltx$ .*sty$ .*TeX$ .*tex$ .*texi$ .*texinfo$ "
       ".*txi$ .*x[bp]m$ .*yy$ .*[Ss][Qq][Ll]$\" | "
       "etags.exe - -a -o " win-directory "TAGS")))
    ((string-equal system-type "gnu/linux") ; linux
     (eshell-command
      (concat
       "find " output-directory " -type f -regextype posix-extended "
       "-regex '^.*\\.("
       "ad[absm]|[CFHMSacfhlmpsty]|def|in[cs]|s[as]|src|cc"
       "hh|[chy]++|[ch]pp|[chy]xx|pdb|[ch]s|[Cc][Oo][Bb]|"
       "[eh]rl|f90|for|java|[cem]l|clisp|lisp|[Ll][Ss][Pp]"
       "[Mm]akefile*|pas|[Pp][LlMm]|psw|lm|pc|prolog|oak|"
       "p[sy]|sch|scheme|[Ss][Cc][Mm]|[Ss][Mm]|bib|cl[os]|"
       "ltx|sty|TeX|tex|texi|texinfo|txi|x[bp]m|yy|"
       "[Ss][Qq][Ll])$' -print | xargs etags -a -o "
       output-directory "TAGS")))))

;; Move lines and regions
(defun move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun move-region (start end n)
  "Move the current region from START to END up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current region from START to END up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current region form START to END down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
  "Move a line or region from START to END up by N lines."
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up)))

(defun move-line-region-down (&optional start end n)
  "Move a line or region from START to END down by N lines."
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down)))

(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun sudo-save ()
  "Save file as sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:"
                          (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:"
                        buffer-file-name))))

(defun toggle-camelcase-snakecase ()
  "Toggle between camelcase and snakecase notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (cond
       (currently-using-underscores-p
        (upcase-initials-region start end)
        (replace-string "_" "" nil start end)
        (downcase-region start (1+ start)))
       (t
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol))))))))

(defadvice xref-find-definitions (before c-tag-file activate)
  "Automatically create tags file."
  (let ((tag-file (if (projectile-project-p)
                      (concat (projectile-project-p) "TAGS")
                    (concat default-directory "TAGS"))))
    (unless (file-exists-p tag-file)
      (create-etags))
    (visit-tags-table tag-file)))

(provide 'utils)
;;; utils.el ends here
