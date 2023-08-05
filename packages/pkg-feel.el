;; Install necessary packages
(require-package 'diminish)
(require-package 'minimap)
(require-package 'flx-isearch)
(require-package 'expand-region)
(require-package 'easy-kill)
(require-package 'operate-on-number)
(require-package 'smartrep)

;; Load smartrep
(require 'smartrep)

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
(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; Whitespace-mode config
(require 'whitespace)
(setq whitespace-style '(face tabs empty trailing))
(global-whitespace-mode +1)

;; Compilation from Emacs
(add-hook 'compilation-filter-hook 'main-colorize-compilation-buffer)

;; Easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; Operate-on-number
(smartrep-define-key global-map "C-c ."
'(("+" . apply-operation-to-number-at-point)
("-" . apply-operation-to-number-at-point)
("*" . apply-operation-to-number-at-point)
("/" . apply-operation-to-number-at-point)
("\\" . apply-operation-to-number-at-point)
("^" . apply-operation-to-number-at-point)
("<" . apply-operation-to-number-at-point)
(">" . apply-operation-to-number-at-point)
("#" . apply-operation-to-number-at-point)
("%" . apply-operation-to-number-at-point)
("'" . operate-on-number-at-point)))

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
