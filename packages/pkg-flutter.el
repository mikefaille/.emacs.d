;; -*- lexical-binding: t; -*-
;;; pkg-flutter.el --- Flutter & Dart support

(require 'use-package)

;; Dart mode for syntax support
(use-package dart-mode
  :ensure t
  :defer t)

;; Note: LSP integration for Dart is handled in pkg-lsp.el via lsp-dart

;; Configure Flutter paths
(setq exec-path (append '("/opt/flutter/bin") exec-path))

(provide 'pkg-flutter)
;;; pkg-flutter.el ends here
