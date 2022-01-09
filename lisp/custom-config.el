;;; custom-config.el --- My custom configurations  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joe Pearson

;; Author: Joe Pearson <pearjo@protonmail.com>

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
(setq-default create-lockfiles nil
              backup-directory-alist '(("" . "~/.emacs.d/backup/")))

(recentf-mode 1) ;; keep a list of recently opened files
(delete-selection-mode 1) ;; overwrite selected region
(global-auto-revert-mode 1)
(global-subword-mode 1) ;; move over camelCase words correctly
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'custom-config)
;;; custom-config.el ends here
