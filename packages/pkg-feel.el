;; -*- lexical-binding: t; -*-

;; This file configures various "look and feel" packages and settings.

;; Ensure use-package is available
(require 'use-package)

;; Diminish mode line clutter
(use-package diminish
  :ensure t
  ;; Configuration for diminishing specific modes can go here or where those modes are loaded
  ;; Example: (diminish 'eldoc-mode)
  )

;; Minimap for code overview
(use-package minimap
  :ensure t
  :defer t ; Defer loading until explicitly called
  ;; Add keybindings or hooks as needed
  ;; :bind ("C-c m" . minimap-mode)
  )

;; Expand region semantically
(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region)) ; Common binding

;; Operate on numbers easily
(use-package operate-on-number
  :ensure t
  :defer t
  ;; Bind keys if desired, e.g., for incrementing/decrementing
  ;; :bind (("C-c +" . #'on/add)
  ;;        ("C-c -" . #'on/subtract))
  )

;; Recent Files configuration
(use-package recentf
  :ensure nil ; Built-in
  :init
  (setq main-savefile-dir (expand-file-name "savefile" user-emacs-directory)) ; Define ONCE globally
  (setq recentf-save-file (expand-file-name "recentf" main-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)) ; Enable the mode

;; Highlight current line globally
(use-package hl-line
  :ensure nil ; Built-in
  :hook (after-init . global-hl-line-mode)) ; Enable globally after init

;; Highlight changes briefly
(use-package volatile-highlights
  :ensure t
  :hook (after-init . volatile-highlights-mode) ; Enable globally after init
  ;; :config
  ;; (diminish 'volatile-highlights-mode) ; Uncomment to hide from mode line
  )

;; Whitespace visualization configuration
(use-package whitespace
  :ensure nil ; Built-in
  :defer t ; Usually loaded when needed or via global mode
  :custom
  (whitespace-line-column 80 "Maximum line length.") ; Use :custom for customization
  (whitespace-style '(face tabs empty trailing lines-tail)) ; Added lines-tail visualization
  :config
  ;; Enable globally if desired, or use hooks for specific modes
  (global-whitespace-mode 1)
  ;; Optional: Diminish mode line indicator
  ;; (require 'diminish) ; Ensure diminish is loaded if using here
  ;; (diminish 'whitespace-mode)
  )

;; Hippie Expand configuration (alternative/complement to Corfu/Cape)
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
;; Assumes 'main-colorize-compilation-buffer' function is defined elsewhere
(add-hook 'compilation-filter-hook #'main-colorize-compilation-buffer)

;; --- Modernized Advice for server-visit-files ---
;; Function to parse filename:line:col format
(defun my-parse-file-line-col (filename)
  "Parse FILENAME formatted as 'name:line:col' or 'name:line'.
Return (cons PARSED-NAME (cons LINE COL)) or original FILENAME."
  (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" filename)
      (let ((name (match-string 1 filename))
            (line (string-to-number (match-string 2 filename)))
            (col-str (match-string 3 filename)))
        (cons name (cons line (when col-str (string-to-number col-str)))))
    filename))

;; Advice function to modify the file list argument
(defun my-server-visit-files-advice (orig-fun files proc &optional nowait)
  "Advice for `server-visit-files` to handle 'name:line:col'.
Transforms the FILES argument before calling ORIG-FUN."
  (let ((parsed-files (mapcar #'my-parse-file-line-col files)))
    (apply orig-fun parsed-files proc (when nowait (list nowait)))))

;; Add the advice using the modern advice system
(advice-add 'server-visit-files :around #'my-server-visit-files-advice)

;; Mark this file as provided
(provide 'pkg-feel)
;;; pkg-feel.el ends here
