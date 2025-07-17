;;; pkg-discover.el --- Discover key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures discover to help find key bindings.

;;; Code:

(require-package 'discover)
(use-package discover
  :defer t
  :config
  (require-package 'discover-my-major)
  (use-package discover-my-major
    :defer t)
  ;; Bind discover-my-major to "C-h C-m".
  (global-set-key (kbd "C-h C-m") 'discover-my-major)
  ;; Bind discover-my-mode to "C-h M-m".
  (global-set-key (kbd "C-h M-m") 'discover-my-mode))

(provide 'pkg-discover)
