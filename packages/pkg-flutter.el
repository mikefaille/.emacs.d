;;; pkg-flutter.el --- Flutter configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures Flutter support with lsp-dart.

;;; Code:

(require-package 'dart-mode)
(use-package dart-mode
  :defer t
  :after lsp-mode
  :hook (dart-mode-hook . lsp))

(require-package 'lsp-mode)
(use-package lsp-mode
  :defer t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . (lambda ()
                 (define-key lsp-mode-map (kbd "TAB") #'completion-at-point))))
  :custom
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-file-watch-threshold 1500)
  (lsp-enable-file-watchers nil)
  (lsp-enable-symbol-highlighting nil)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024)))

(require-package 'lsp-ui)
(use-package lsp-ui
  :defer t
  :commands (lsp-ui-mode lsp-ui-doc-mode)
  :after lsp-mode
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-show-code-actions nil))

(add-hook 'dart-mode-hook #'lsp-deferred)
(add-hook 'dart-mode-hook #'lsp-ui-mode)

(setq lsp-disabled-clients '(semgrep-ls))

(require-package 'lsp-ivy)
(use-package lsp-ivy
  :defer t
  :commands lsp-ivy-workspace-symbol)

(require-package 'lsp-treemacs)
(use-package lsp-treemacs
  :defer t
  :commands lsp-treemacs-errors-list)

(require-package 'dap-mode)
(use-package dap-mode
  :defer t)

(require-package 'which-key)
(use-package which-key
  :defer t
  :config
  (which-key-mode))

(require-package 'lsp-dart)
(use-package lsp-dart
  :defer t
  :hook (dart-mode . lsp))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(setq lsp-dart-sdk-dir "/opt/flutter/")
(add-to-list 'exec-path (concat lsp-dart-sdk-dir "bin/"))

(provide 'pkg-flutter)
