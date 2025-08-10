;; -*- lexical-binding: t; -*-
;; core-feel.el --- Core UI, interaction, and basic editing modes configuration

(require 'use-package)

;; Define savefile directory (ensure this is defined *once* globally, e.g., in init.el)
;; (defvar main-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; --- Mouse Configuration ---
(use-package mouse
  :ensure nil ; Built-in
  :config
  ;; Enable mouse reporting in terminal (e.g., for iTerm2 compatibility)
  (when (not (display-graphic-p))
    (xterm-mouse-mode 1)))

;; Enable pixel-level precision scrolling (Emacs 29+)
(use-package pixel-scroll
  :ensure nil
  :config
  (pixel-scroll-precision-mode 1))

;; --- Flymake (Built-in Diagnostics) ---
(use-package flymake
  :ensure nil ; Built-in
  :config
  (with-eval-after-load 'flymake
    (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)))
  ;; Optional: Diminish mode-line indicator
  ;; :diminish flymake-mode
  )

;; --- Window Management ---

;; Window Dividers (Visual preference)
(use-package window-divider
  :ensure nil ; Built-in (Emacs 29+)
  :hook (after-init . window-divider-mode))

;; Windmove configuration
(use-package windmove
  :ensure nil ; Built-in
  :config
  ;; Enable moving between windows using Meta + Arrow keys
  (windmove-default-keybindings 'meta))

;; Global Keybindings for Window Manipulation
(global-set-key (kbd "S-C-<left>") #'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") #'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") #'shrink-window)
(global-set-key (kbd "S-C-<up>") #'enlarge-window)

;; Global Keybindings for Scrolling (Consider if needed, mouse wheel often sufficient)
;; (global-set-key [mouse-4] #'scroll-down-command) ; Usually default
;; (global-set-key [mouse-5] #'scroll-up-command)   ; Usually default
;; (global-set-key [mouse-6] #'scroll-left)         ; Horizontal scroll
;; (global-set-key [mouse-7] #'scroll-right)        ; Horizontal scroll

;; --- Editing Enhancements ---

;; Line Numbers
(setq-default display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Electric Pair Mode (Auto-pairing delimiters)
(use-package electric
  :ensure nil ; Built-in
  :hook (prog-mode . electric-pair-mode) ; Enable for programming modes
  ;; Consider enabling for text modes too if desired
  ;; :hook (text-mode . electric-pair-mode)
  )

;; Show Matching Parentheses
(use-package paren
  :ensure nil ; Built-in
  :custom
  (show-paren-style 'mixed) ; Highlight matching paren/expression
  (show-paren-delay 0.1)    ; Slight delay
  :config
  (show-paren-mode 1))

;; Save Cursor Position in Files

(use-package saveplace
  :ensure nil ; Built-in
  ;; Removed the empty/invalid :custom keyword
  :config
  ;; Enable saving place in files globally
  (setq-default save-place t)
  ;; Optionally configure the save file location (ensure variable is defined)
  ;; (setq save-place-file (expand-file-name "saveplace" michael-savefile-dir))
  )

;; Auto Revert Buffers When Files Change
(use-package autorevert
  :ensure nil ; Built-in
  :custom
  (global-auto-revert-non-file-buffers t) ; Revert non-file buffers too (like dired)
  :config
  (global-auto-revert-mode 1))

;; Eldoc Mode (Documentation in echo area)
(use-package eldoc
  :ensure nil ; Built-in
  :custom
  (eldoc-idle-delay 0.2) ; Show info slightly faster (default is 0.5)
  ;; Enable eldoc globally or via major-mode hooks where supported
  ;; Example for Emacs Lisp:
  ;; :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  ;; LSP clients often provide their own enhanced documentation popups
  )

;; Optional: Terminal-specific Word Navigation Keys
;; Only needed if Ctrl+Left/Right don't work OOTB in your terminal
;; (global-set-key (kbd "M-[1;5C") #'forward-word)  ; Ctrl+right
;; (global-set-key (kbd "M-[1;5D") #'backward-word) ; Ctrl+left

;; --- Emacs Built-in Completion Settings ---
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates (handled better by Corfu/Vertico cycle)
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  (tab-always-indent 'complete)

  ;; Emacs 30+: Disable Ispell completion function. Use `cape-dict` or `cape-ispell` instead.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28+: Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; --- Custom Variables File ---
;; Load custom variables (ensure path is correct)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;; Mark this file as provided
(provide 'core-feel)
;;; core-feel.el ends here
