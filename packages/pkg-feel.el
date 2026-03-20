;; -*- lexical-binding: t; -*-
;;; pkg-feel.el --- Various "look and feel" packages and settings

(require 'use-package)

;; Diminish mode line clutter
(use-package diminish
  :ensure t)

;; Minimap for code overview
(use-package minimap
  :ensure t
  :defer t)

;; Expand region semantically
(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))

;; Operate on numbers easily
(use-package operate-on-number
  :ensure t
  :defer t)

;; Recent Files configuration
(use-package recentf
  :ensure nil ; Built-in
  :init
  (setq recentf-save-file (expand-file-name "recentf" michael-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1))

;; Highlight current line globally
(use-package hl-line
  :ensure nil ; Built-in
  :hook (after-init . global-hl-line-mode))

;; Highlight changes briefly
(use-package volatile-highlights
  :ensure t
  :demand t
  :config
  (volatile-highlights-mode 1))

;; Whitespace visualization configuration
(use-package whitespace
  :ensure nil ; Built-in
  :defer t
  :custom
  (whitespace-line-column 80 "Maximum line length.")
  (whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (global-whitespace-mode 1))

;; Hippie Expand configuration
(use-package hippie-exp
  :ensure nil ; Built-in
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;; Compilation buffer colorization
(require 'ansi-color)
(defun michael-colorize-compilation-buffer ()
  "Apply ANSI color codes to compilation buffer."
  (when (derived-mode-p 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook #'michael-colorize-compilation-buffer)

;; --- Modernized Advice for server-visit-files ---
(defun michael-parse-file-line-col (filename)
  "Parse FILENAME formatted as 'name:line:col' or 'name:line'."
  (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" filename)
      (let ((name (match-string 1 filename))
            (line (string-to-number (match-string 2 filename)))
            (col-str (match-string 3 filename)))
        (cons name (cons line (when col-str (string-to-number col-str)))))
    filename))

(defun michael-server-visit-files-advice (orig-fun files proc &optional nowait)
  "Advice for `server-visit-files` to handle 'name:line:col'."
  (let ((parsed-files (mapcar #'michael-parse-file-line-col files)))
    (apply orig-fun parsed-files proc (when nowait (list nowait)))))

(advice-add 'server-visit-files :around #'michael-server-visit-files-advice)

(provide 'pkg-feel)
;;; pkg-feel.el ends here
