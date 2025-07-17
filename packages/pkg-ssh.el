;;; pkg-ssh.el --- SSH configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures SSH mode for editing SSH config files.

;;; Code:

(require-package 'ssh-config-mode)
(use-package ssh-config-mode
  :defer t
  :config
  ;; Autoload ssh-config-mode.
  (autoload 'ssh-config-mode "ssh-config-mode" t)
  ;; Associate ssh config files with ssh-config-mode.
  (add-to-list 'auto-mode-alist '(".ssh/config\\'"       . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("sshd?_config\\'"      . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("known_hosts\\'"       . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("authorized_keys2?\\'" . ssh-authorized-keys-mode))
  ;; Enable font lock in ssh-config-mode.
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  ;; Set the tramp chunksize to improve performance over slow connections.
  (setq tramp-chunksize 1300))

(provide 'pkg-ssh)
