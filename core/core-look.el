;; -*- lexical-binding: t; -*-
;; core-look.el --- Core look & feel settings (UI, mode line, interaction)

(require 'use-package)

;; --- Basic UI Settings ---

;; Disable blinking cursor (can also be in early-init.el)
(blink-cursor-mode -1)

;; Fringe width (visual preference)
(use-package fringe
  :ensure nil ; Built-in
  :config
  ;; Check if fringe-mode function exists (older Emacs might not have it)
  (when (fboundp 'fringe-mode)
    (fringe-mode 4))) ; Adjust width as desired (pixels)

;; Window Dividers (visual preference - ensure loaded once)
;; If already enabled in core-feel.el or elsewhere, remove this.
;; (use-package window-divider
;;   :ensure nil ; Built-in (Emacs 29+)
;;   :hook (after-init . window-divider-mode))

;; --- Mode Line Configuration ---

;; Basic mode line indicators (built-in)
(use-package simple
  :ensure nil ; Built-in package providing modes below
  :config
  ;; These enable minor modes globally. Consider using hooks (e.g., prog-mode-hook)
  ;; for line/column numbers if you don't want them everywhere.
  ;; Alternatively, use a dedicated mode-line package for more control.
  (line-number-mode t)
  (column-number-mode t)
  ;; size-indication-mode is usually part of the default mode-line format
  ;; (size-indication-mode t)
  )

;; --- Interaction Settings ---

;; Use y/n instead of yes/no prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable UI dialog boxes for prompts
(setq use-dialog-box nil)

;; Enable recursive minibuffers (if not set elsewhere)
;; (setq enable-recursive-minibuffers t)

;; --- Scrolling Configuration ---
(setq scroll-margin 0 ; No margin triggering scroll near top/bottom
      scroll-conservatively 10000 ; Scroll full screen if possible
      scroll-preserve-screen-position t) ; Try to keep point stationary on screen

;; --- Buffer Name Handling ---
(use-package uniquify
  :ensure nil ; Built-in
  :custom
  ;; Make buffer names unique, e.g., foo<bar/baz/>
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*") ; Ignore special buffers
  (uniquify-min-dir-content 1)
  (uniquify-separator "/"))

;; --- Frame Title ---
(setq frame-title-format
      '("" invocation-name " eMikes - " ; Customize title prefix
        ;; Display file name (abbreviated) or buffer name
        (:eval (if buffer-file-name
                   (abbreviate-file-name buffer-file-name)
                 "%b"))))

;; --- Keybindings ---
;; Bind C-x C-b to ibuffer (built-in buffer list alternative)
(use-package ibuffer
  :ensure nil ; Built-in
  :bind ("C-x C-b" . ibuffer))

;; --- Optional: AnsiColor for Shell/Comint Modes ---
;; Uncomment and configure if needed
;; (use-package comint
;;   :ensure nil
;;   :hook ((shell-mode . ansi-color-for-comint-mode-on)
;;          (comint-mode . ansi-color-for-comint-mode-on))
;;   :config
;;   (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))


;; Mark this file as provided
(provide 'core-look)
;;; core-look.el ends here
