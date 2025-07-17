;;; pkg-feel.el --- General look and feel -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures the general look and feel of Emacs.

;;; Code:

(require-package 'diminish)
(use-package diminish
  :defer t)

(require-package 'minimap)
(use-package minimap
  :defer t)

(require-package 'expand-region)
(use-package expand-region
  :defer t)

(require-package 'operate-on-number)
(use-package operate-on-number
  :defer t)

(require-package 'volatile-highlights)
(use-package volatile-highlights
  :defer t
  :config
  (volatile-highlights-mode t))

;; Variables
(defvar main-savefile-dir
  (concat user-emacs-directory
          (convert-standard-filename "savefile")) )

(defvar whitespace-line-column 80
  "Maximum line length.")

;; Hippie expand config
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" main-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode +1)

;; Highlight the current line
(global-hl-line-mode +1)

;; Whitespace-mode config
(require 'whitespace)
(setq whitespace-style '(face tabs empty trailing))
(global-whitespace-mode +1)

;; Compilation from Emacs
(add-hook 'compilation-filter-hook 'main-colorize-compilation-buffer)


;; Ensure server visit files in the correct format
(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
  Most of console-based utilities prints filename in format
  'filename:linenumber'. So you may wish to open filename in that format.
  Just call:
  emacsclient filename:linenumber
  and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
  (mapcar (lambda (fn)
            (let ((name (car fn)))
              (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                  (cons
                   (match-string 1 name)
                   (cons (string-to-number (match-string 2 name))
                         (string-to-number (or (match-string 3 name) ""))))
                fn))) files)))

(provide 'pkg-feel)
