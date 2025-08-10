;; -*- lexical-binding: t; -*-
;;; pkg-lsp2.el --- Configuration for LSP Mode, related modes, and UI

(require 'use-package)

;; --- TypeScript Mode ---
(use-package typescript-ts-mode
  :ensure t
  ;; Associate .ts/.tsx files with this mode
  :mode (
				 ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
				 )
  :custom
  ;; Set indentation (consider using editorconfig for project-specific settings)
  (typescript-ts-mode-indent-offset 2)
  )

;; --- LSP Mode (Language Server Protocol Client) ---
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred) ; Ensure lsp and lsp-deferred are autoloaded
  :pin melpa ; Optional: Pin to MELPA if preferred over other archives
  :init
  ;; Configure Language ID mapping (Crucial for LSP)
  (setq lsp-language-id-configuration
        '(;; Add mappings for all languages you intend to use with LSP
          ;; (typescript-ts-mode . "typescript")
          ;; (tsx-ts-mode . "typescriptreact")
          ;; (js-ts-mode . "javascript")
          ;; (python-mode . "python")
          ;; (rust-mode . "rust")
          ;; (go-mode . "go")
          ;; KCL mapping added later after kcl-mode is defined/loaded
          ))
  ;; :hook (;; Activate LSP automatically but defer server start for these modes
  ;;        ;; (typescript-ts-mode . lsp-deferred)
  ;;        ;; (tsx-ts-mode . lsp-deferred)
  ;;        ;; (js-ts-mode . lsp-deferred)
  ;;        ;; (python-mode . lsp-deferred)
  ;;        ;; (rust-mode . lsp-deferred)
  ;;        ;; (go-mode . lsp-deferred)
  ;;        ;; KCL hook added later
  ;;        )
  :custom
  ;; Performance & Logging Settings
  (lsp-log-io nil)
  (lsp-print-performance nil)
  (lsp-idle-delay 0.500)
  (lsp-enable-file-watchers nil)
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable nil)
  )

;; --- LSP UI Enhancements ---
(use-package lsp-ui
  :ensure t
  :after lsp-mode ; Ensure lsp-mode is loaded first
  :commands lsp-ui-mode ; Autoload the mode command
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-peek-enable t)
  :config
  (lsp-ui-mode 1))


;; --- KCL Mode Definition and LSP Integration ---

;; Define potential paths for the KCL language server executable
(defvar my/kcl-server-paths
  '("/usr/local/kclvm/bin/kcl-language-server"
    "/usr/bin/kcl-language-server"
    "kcl-language-server")
  "Possible paths for KCL language server executable.")

;; Helper function to find the executable server path
(defun my/find-kcl-server ()
  "Find KCL language server executable in `my/kcl-server-paths`."
  (seq-find #'executable-find my/kcl-server-paths))

;; Define the major mode itself (if not provided by an external package)
(unless (fboundp 'kcl-mode)
  (define-derived-mode kcl-mode prog-mode "KCL"
    "Major mode for editing KCL files."
    (setq-local comment-start "//")
    ;; Add other mode-specific settings if needed
    ))

;; Now configure kcl-mode using use-package
(use-package kcl ; Use a different symbol for use-package if kcl-mode is defined above
                 ; Or keep kcl-mode if it IS an external package with :ensure t
  :ensure nil ; Set to t ONLY if kcl-mode is a separate package to install
  :mode ("\\.k\\'" . kcl-mode) ; Associate .k files with the defined kcl-mode
  :hook (kcl-mode . lsp) ; Hook LSP into the defined kcl-mode
  :config
  ;; Register KCL language server with lsp-mode
  ;; Ensure lsp-mode is loaded before this runs
  (with-eval-after-load 'lsp-mode
    ;; Add language ID mapping
    (add-to-list 'lsp-language-id-configuration '(kcl-mode . "kcl"))
    ;; Register the client configuration
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection #'my/find-kcl-server)
      :major-modes (list 'kcl-mode) ; Reference the defined kcl-mode
      :server-id 'kcl-ls
      ))))


;; Mark this file as provided
(provide 'pkg-lsp)
;;; pkg-lsp2.el ends here
