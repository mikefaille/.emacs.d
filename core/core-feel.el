;;; core-feel.el ---

;; Mouse Integration
(require 'mouse)  ; needed for iterm2 compatibility
(xterm-mouse-mode t)

(require  'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(pixel-scroll-precision-mode)

;; Mouse Scrolling
(global-set-key [mouse-4] 'scroll-down-command)
(global-set-key [mouse-5] 'scroll-up-command)
(global-set-key [mouse-6] 'scroll-left)
(global-set-key [mouse-7] 'scroll-right)

;; Window divider mode
(window-divider-mode t)

;; Window Manipulation
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Change window using meta arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings 'meta)

;; Disable Bell Function
(setq ring-bell-function 'ignore)

;; Electric Pair Mode
(require 'electric)
(add-hook 'prog-mode-hook #'electric-pair-mode)

;; Word Navigation
(global-set-key "\M-[1;5C" 'forward-word)  ; Ctrl+right => forward word
(global-set-key "\M-[1;5D" 'backward-word)  ; Ctrl+left => backward word

;; Auto Revert Mode
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "saveplace" main-savefile-dir))

;; Eldoc Mode
(setq eldoc-idle-delay 1)

;; Show Paren Mode
(setq show-paren-style 'mixed)
(show-paren-mode 1)

;; Custom Vars
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; LSP Performance
(setq read-process-output-max (* 1024 1024))  ; required for lsp performance


(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'core-feel)
