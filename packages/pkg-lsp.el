;;; pkg-lsp.el --- Language Server Protocol Configuration -*- lexical-binding: t; -*-

(require 'use-package)

;; --- LSP Mode (Modern & Performance-tuned) ---
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; Languages
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (kcl-mode . lsp-deferred)
         (dart-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (bash-ts-mode . lsp-deferred)
         ;; Core
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-headerline-breadcrumb-enable nil) ; Clean UI
  (lsp-completion-provider :capf)        ; Use Corfu/Cape
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)                       ; Performance
  (lsp-warn-no-libjson nil)
  :config
  ;; Automatically install missing LSP servers
  (setq lsp-enable-suggest-server-download t)
  
  ;; Bash Language Server configuration
  (setq lsp-bash-explainshell-endpoint "https://explainshell.com")

  ;; Multi-LSP Configuration
  (setq lsp-keep-workspace-alive nil)

  ;; --- LSP Booster Integration ---
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)
               (not (file-remote-p default-directory))
               (not (string-match-p "emacs-lsp-booster" (car orig-result)))
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" (car orig-result))
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; --- LSP UI ---
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t))

;; --- KCL Support ---
(use-package kcl-mode
  :ensure nil
  :mode "\\.k\\'")

;; --- Dart/Flutter Support ---
(use-package lsp-dart
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk")
  (setq lsp-dart-flutter-sdk-dir "/opt/flutter"))

(provide 'pkg-lsp)
;;; pkg-lsp.el ends here
