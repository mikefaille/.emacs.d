;;; core-feel.el ---

;; Mouse Integration
(require 'mouse)  ; needed for iterm2 compatibility
(xterm-mouse-mode t)

;; Mouse Scrolling
(global-set-key [mouse-4] 'scroll-down-command)
(global-set-key [mouse-5] 'scroll-up-command)
(global-set-key [mouse-6] 'scroll-left)
(global-set-key [mouse-7] 'scroll-right)

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

(provide 'core-feel)
