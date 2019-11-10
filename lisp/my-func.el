;;; my-func.el --- My CC mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: defun

;;; Commentary:

;;; Code:

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
       "dir " win-directory " /s | "
       "findstr /i \""
       "\\.ad[absm] \\.[CFHMSacfhlmpsty] \\.def \\.in[cs] \\.s[as] "
       "\\.src \\.cc \\.hh \\.[chy]++ \\.[ch]pp \\.[chy]xx \\.pdb "
       "\\.[ch]s \\.[Cc][Oo][Bb] \\.[eh]rl \\.f90 \\.for \\.java "
       "\\.[cem]l \\.clisp \\.lisp \\.[Ll][Ss][Pp] [Mm]akefile* "
       "\\.pas \\.[Pp][LlMm] \\.psw \\.lm \\.pc \\.prolog \\.oak "
       "\\.p[sy] \\.sch \\.scheme \\.[Ss][Cc][Mm] \\.[Ss][Mm] \\.bib "
       "\\.cl[os] \\.ltx \\.sty \\.TeX \\.tex \\.texi \\.texinfo "
       "\\.txi \\.x[bp]m \\.yy \\.[Ss][Qq][Ll]\" | "
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

(defadvice xref-find-definitions (before c-tag-file activate)
  "Automatically create tags file."
  (let ((tag-file (if (projectile-project-p)
                      (concat (projectile-project-p) "TAGS")
                    (concat default-directory "TAGS"))))
    (unless (file-exists-p tag-file)
      (create-etags))
    (visit-tags-table tag-file)))

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

(global-set-key (kbd "M-<up>") 'move-line-region-up)
(global-set-key (kbd "M-<down>") 'move-line-region-down)

(defun sudo-save ()
  "Save file as sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:"
                          (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:"
                        buffer-file-name))))

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

;; optional key binding
(global-set-key "\C-c\C-k" 'copy-line)

(provide 'my-func)
;;; my-func.el ends here
