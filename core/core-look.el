;;; core-look.el ---

;; License:
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3 of the License, or any later version.

;;; Code:

;; AnsiColor for Shell Mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; UI Settings
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))        ; disable toolbar
(menu-bar-mode -1)           ; disable menubar
(blink-cursor-mode -1)       ; disable blinking cursor

;; Startup Settings
(setq inhibit-startup-screen t)  ; disable startup screen

;; Scrolling Settings
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Mode Line Settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Fringe Settings
(when (fboundp 'fringe-mode)
  (fringe-mode 4))  ; set fringe width to 4 pixels

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Frame Title Settings
(setq frame-title-format
      '("" invocation-name " eMikes - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))  ; display either file or buffer name

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Uniquify Settings
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "^\\*"
      uniquify-min-dir-content 1
      uniquify-separator "/")  ; set separator for buffer name components

;; Keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Dialog Settings
(setq use-dialog-box nil)  ; prevent UI dialog for prompt

(provide 'core-look)
;;; core-look.el ends here
