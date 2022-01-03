;;; my-rust-mode.el --- My rust mode settings

;; Copyright (C) 2019  Joe Pearson

;; Author: Joe Pearson
;; Keywords: rust

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package rust-mode
  :ensure t)

;; check comments in Rust code
(add-hook 'rust-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)))

(provide 'my-rust-mode)
;;; my-rust-mode.el ends here
