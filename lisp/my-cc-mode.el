;;; my-cc-mode.el --- My CC mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: cc

;;; Commentary:

;;  Use `irony' for code completion, real-time syntax checking and
;;  live documentation.

;;; Code:
(require 'use-package)
(require 'cc-mode)

(use-package irony
  :ensure t
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

    ;; Optimize irony on windows
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))

    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

    ;; Use compilation database first, clang_complete as fallback.
    (setq-default irony-cdb-compilation-databases
                  '(irony-cdb-libclang irony-cdb-clang-complete))))

(use-package company-irony
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package company-irony-c-headers
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends
               '(company-irony-c-headers company-irony)))

;; (use-package rtags
;;   :ensure t
;;   :init
;;   (progn
;;     (setq rtags-path "~/.emacs.d/modules/rtags/bin")
;;     (unless '(rtags-executable-find "rc")
;;       (message "Binary rc is not installed!"))
;;     (unless '(rtags-executable-find "rdm")
;;       (message "Binary rdm is not installed!"))
;;     (rtags-start-process-unless-running)
;;     (define-key c-mode-base-map (kbd "M-.")
;;       'rtags-find-symbol-at-point)
;;     (define-key c-mode-base-map (kbd "M-,")
;;       'rtags-find-references-at-point)
;;     (define-key c-mode-base-map (kbd "M-?")
;;       'rtags-display-summary)
;;     (rtags-enable-standard-keybindings)

;;     (setq rtags-use-helm t)
;;     ;; Shutdown rdm when leaving emacs.
;;     (add-hook 'kill-emacs-hook 'rtags-quit-rdm)))

;; (use-package company-rtags
;;   :ensure t
;;   :after company
;;   :config
;;   (progn
;;     (define-key c-mode-base-map (kbd "M-.")
;;       (function rtags-find-symbol-at-point))
;;     (define-key c-mode-base-map (kbd "M-,")
;;       (function rtags-find-references-at-point))
;;     (setq rtags-completions-enabled t)
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (push 'company-rtags company-backends)
;;     (global-company-mode)))

;; (use-package flycheck-rtags
;;   :ensure t
;;   :after flycheck
;;   :init
;;   ;; Optional explicitly select the RTags Flycheck checker for c or
;;   ;; c++ major mode.  Turn off Flycheck highlighting, use the RTags
;;   ;; one.  Turn off automatic Flycheck syntax checking rtags does this
;;   ;; manually.
;;   (defun my-flycheck-rtags-setup ()
;;     "Configure flycheck-rtags for better experience."
;;     (flycheck-select-checker 'rtags)
;;     (setq-local flycheck-highlighting-mode nil)
;;     (setq-local flycheck-check-syntax-automatically nil)
;;     (rtags-set-periodic-reparse-timeout 2.0))
;;   (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;;   (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
;;   (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup))

;; (use-package helm-rtags
;;   :ensure t
;;   :init
;;   (setq rtags-display-result-backend 'helm))

(defun create-etags (dir)
  "Create tags file for source files in DIR."
  (interactive "Ddirectory: ")
  (eshell-command
   (format "find %s -type f -name '*.[c,cpp,c++,C,h,H]' | etags -" dir)))

(use-package qt-c-style
  :init
  (defun qt-c-mode-hook ()
    "Set the Qt code style."
    (c-set-style "Qt"))
  (add-hook 'c-mode-common-hook 'qt-c-mode-hook)
  (add-hook 'c-mode-common-hook 'qt-set-c-style)
  (add-hook 'c-mode-common-hook 'qt-make-newline-indent))

(require 'qt-pro)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

(defface doxygen-verbatim-face
  '((default :inherit default))
  "Face used to show Doxygen block regions"
  :group 'font-lock-faces)

(defface doxygen-match-face
  '((default :inherit default)
    (t :underline t))
  "Face used to show Doxygen region start end commands"
  :group 'font-lock-faces)

(defconst custom-font-lock-doc-comments
  `(
    ;; Highlight Doxygen special commands,
    ;;   \cmd or @cmd
    ;; and the non [a-z]+ commands
    ;;   \\ \@ \& \# \< \> \% \" \. \| \-- \--- \~[LanguageId]
    (,(concat
       "\\(?:"
       "[\\@][a-z]+"     ;; typical word Doxygen special @cmd or \cmd
       "\\|"
       ;; non-word commands, e.g. \\ or @\
       "[\\@]\\(?:\\\\\\|@\\|&\\|#\\|<\\|>\\|%\\|\"\\|\\.\\|::\\||\\|---?\\|~[a-z]*\\)"
       "\\)")
     0 ,c-doc-markup-face-name prepend nil)
    ;; Highlight autolinks. These are referring to functions, so we use a different font face
    ;; from the Doxygen special commands.
    (,(concat
       "\\(?:"
       ;; function() or function(int, std::string&, void*) or more complex where we only
       ;; match the first paren, function(x->(), 2*(y+z)).
       "[A-Za-z_0-9]+(\\([A-Za-z_0-9:&*, ]*)\\)?"
       ;; ClassName::memberFcn or the destructor ClassName::~ClassName. Can also do unqualified
       ;; references, e.g. ::member. The parens are optional, ::member(int, int), ::member(a, b).
       ;; We only require matching of first paren to make cases like ::member(x->(), 2*(y+z))
       ;; work. We don't want \::thing to be highlighed as a function, hence reason to look for
       ;; class::member or space before ::member.  Note '#' can be used instead of '::'
       "\\|"
       "\\(?:[A-Za-z_0-9]+\\|\\s-\\)\\(?:::\\|#\\)~?[A-Za-z_0-9]+(?\\(?:[A-Za-z_0-9:&*, \t]*)\\)?"
       ;; file.cpp, foo/file.cpp, etc. Don't want to pickup "e.g." or foo.txt because
       ;; these are not autolinked so look for common C++ extensions.
       "\\|"
       "[A-Za-z_0-9/]+\\.\\(?:cpp\\|cxx\\|cc\\|c\\|hpp\\|hxx\\|hh\\|h\\)"
       "\\)")
     0 font-lock-function-name-face prepend nil)
    ;; Highlight URLs, e.g. http://doxygen.nl/autolink.html note we do this
    ;; after autolinks highlighting (we don't want nl/autolink.h to be file color).
    ("https?://[^[:space:][:cntrl:]]+"
     0 font-lock-keyword-face prepend nil)
    ;; Highlight HTML tags - these are processed by Doxygen, e.g. <b> ... </b>
    (,(concat "</?\\sw"
                "\\("
                (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
                        "\"[^\"]*\"\\|'[^']*'")
                "\\)*>")
     0 ,c-doc-markup-face-name prepend nil)
    ;; E-mails, e.g. first.last@domain.com. We don't want @domain to be picked up as a Doxygen
    ;; special command, thus explicitly look for e-mails and given them a different face than the
    ;; Doxygen special commands.
    ("[A-Za-z0-9.]+@[A-Za-z0-9_]+\\.[A-Za-z0-9_.]+"
     0 font-lock-keyword-face prepend nil)
    ;; Quotes: Doxygen special commands, etc. can't be in strings when on same line, e.g.
    ;; "foo @b bar line2 @todo foobar" will not bold or create todo's.
    ("\"[^\"[:cntrl:]]+\""
     0 ,c-doc-face-name prepend nil)

    ("[^\\@]\\([\\@]f.+?[\\@]f\\$\\)"  ;; single line formula but an escaped formula, e.g. \\f[
     1 'doxygen-verbatim-face prepend nil)

    ;; Doxygen verbatim/code/formula blocks should be shown using doxygen-verbatim-face, but
    ;; we can't do that easily, so for now flag the block start/ends
    (,(concat
       "[^\\@]"  ;; @@code shouldn't be matched
       "\\([\\@]\\(?:verbatim\\|endverbatim\\|code\\|endcode\\|f{\\|f\\[\\|f}\\|f]\\)\\)")
     1 'doxygen-match-face prepend nil)

    ;; Here's an attempt to get blocks shown using doxygen-verbatim-face. However, font-lock doesn't
    ;; support multi-line font-locking by default and I'm not sure the best way to make these work.
    ;;
    ;; Doxygen special commands, etc. can't be in verbatim/code blocks
    ;;   @verbatim
    ;;      @cmd  -> not a Doxygen special command
    ;;   @endverbatim
    ;; so set verbatim/code to a different font.  Verbatim/code blocks spans multiple lines and thus
    ;; a refresh of a buffer after editing a verbatim/code block may be required to have the font
    ;; updated.
    ;;("[^\\@][\\@]\\(verbatim\\|code\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]end\\1"
    ;; 2 'doxygen-verbatim-face prepend nil)
    ;; Doxygen formulas are link verbatim blocks, but contain LaTeX, e.g.
    ;;("[^\\@][\\@]f.+[\\@f]\\$"  ;; single line formula
    ;; 0 'doxygen-verbatim-face prepend nil)
    ;; multi-line formula,
    ;;   \f[ ... \f]     or    \f{ ... \}
    ;;("[^\\@][\\@]f\\(?:{\\|\\[\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]f\\(?:}\\|\\]\\)"
    ;; 1 'doxygen-verbatim-face prepend nil)

    ))

;; Matches across multiple lines:
;;   /** doxy comments */
;;   /*! doxy comments */
;;   /// doxy comments
;; Doesn't match:
;;   /*******/
(defconst custom-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
            limit custom-font-lock-doc-comments)))))

(setq-default c-doc-comment-style (quote (custom)))

;; check comments in C++ code
(add-hook 'c++-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

(provide 'my-cc-mode)
;;; my-cc-mode.el ends here
