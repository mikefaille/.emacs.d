;;; pkg-mouvement.el --- Mouvement configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures various packages for efficient navigation.

;;; Code:

(use-package ace-window
  :defer t)
(use-package avy
  :defer t)
(use-package zop-to-char
  :defer t)
(use-package ace-jump-buffer
  :defer t)

;; Avy configuration for word or subword navigation
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)

;; Ace-window configuration for window navigation
(global-set-key (kbd "s-w") 'ace-window)

;; Ace-jump-buffer configuration for buffer navigation
(global-set-key (kbd "C-c k") 'ace-jump-buffer)
(global-set-key (kbd "s->") 'ace-jump-buffer)

;; Zop-to-char configuration to replace zap-to-char functionality
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

(provide 'pkg-mouvement)
