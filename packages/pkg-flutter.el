;; (setq lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk")

;; (add-to-list 'exec-path (concat lsp-dart-sdk-dir "bin/")):
;; (use-package dart-mode
;;   :after lsp-mode
;;   :ensure t  ; Ensure dart-mode is installed
;;   :hook (dart-mode-hook . lsp)) ; Activate LSP automatically


;; (use-package lsp-mode
;;   :commands lsp                ; Make the `lsp` command available
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")   ; Define your preferred prefix key (e.g., "C-c l")

;;   :hook
;;   ((lsp-mode . lsp-enable-which-key-integration) ; Enable which-key integration for LSP commands
;;    (lsp-mode . (lambda ()
;;                  ;; Customize LSP keybindings to work seamlessly with Corfu
;;                  (define-key lsp-mode-map (kbd "TAB") #'completion-at-point))))  ; Use `TAB` for completion in LSP mode

;;   :custom
;;   (lsp-completion-provider :none)           ; Use Corfu for completion
;;   (lsp-enable-snippet nil)                 ; Disable LSP snippets (optional, if they cause issues)
;;   (lsp-completion-show-detail t)           ; Show details in completion candidates
;;   (lsp-completion-show-kind t)             ; Show the kind of completion candidate
;;   (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-file-watch-threshold 1500)           ; Raise file-watching threshold
;;   (lsp-enable-file-watchers nil)            ; Disable file watchers
;;   (lsp-enable-symbol-highlighting nil)      ; Disable symbol highlighting
;;   (gc-cons-threshold 100000000)             ; Increase garbage collection threshold
;;   (read-process-output-max (* 1024 1024))   ; Increase max output from LSP servers
;; )

;; ;; (use-package lsp-mode
;; ;;   :init
;; ;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;; ;;   (setq lsp-keymap-prefix "C-c l")

;; ;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;; ;;          ;; if you want which-key integration
;; ;;          (lsp-mode . lsp-enable-which-key-integration))
;; ;;   :commands lsp)
;; (use-package lsp-ui
;;   :commands (lsp-ui-mode lsp-ui-doc-mode)
;;   :after lsp-mode
;;   :custom
;;   (lsp-ui-sideline-enable t)                   ; Keep sideline enabled for other info
;;   (lsp-ui-sideline-show-hover t)               ; Enable hover for quick info (non-intrusive)
;;   (lsp-ui-doc-enable nil)                     ; Disable the separate doc buffer
;;   (lsp-ui-sideline-show-code-actions nil))     ; Disable code actions in sideline
;; ;; (use-package lsp-ui         ; Enhanced LSP UI elements
;; ;;   :commands (lsp-ui-mode lsp-ui-doc-mode)
;; ;;   :after lsp-mode           ; Load after `lsp-mode` is loaded
;; ;;   :custom
;; ;;   (lsp-ui-sideline-show-hover nil)
;; ;;   (lsp-ui-doc-enable nil)
;; ;;   (lsp-ui-sideline-show-code-actions nil))

;; ;; (add-hook 'dart-mode-hook #'lsp-deferred)
;; ;; (add-hook 'dart-mode-hook #'lsp-ui-mode)
;; ;; optionally

;; (setq lsp-disabled-clients '(semgrep-ls))
;; ;; ;; Ensure the cl-generic package is required
;; ;; (require 'cl-generic)

;; ;; (cl-defgeneric lsp-clients-extract-signature-on-hover (signature)
;; ;;   "Extract signature on hover in LSP clients.")


;; ;; ;; Re-declare lsp-process-id as a generic function
;; ;; (cl-defgeneric lsp-process-id (workspace)
;; ;;   "Return the process id for the LSP workspace.")

;; ;; if you are helm user
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp) ; Make 'lsp' command available

;; (use-package lsp-treemacs
;;   :ensure t
;;   :after lsp-mode)

;; (use-package flycheck
;;   :ensure t)

;; ; 🔹 Configure projectile to find the package pubspec.yaml and set the folder as project root:
;; (with-eval-after-load 'projectile
;;   (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
;;   (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (exec-path-from-shell-initialize))

;; ;; (setq lsp-dart-server-command "/opt/flutter/bin/dart")
;; ;; (setq lsp-dart-sdk-dir "/opt/flutter")


;; Dart mode for syntax support
(use-package dart-mode
  :ensure t)  ;; Start LSP in Dart files

;; LSP mode core
(use-package lsp-mode
  :ensure t
  :commands lsp) ; Ensure lsp command is available

;; LSP Dart integration
(use-package lsp-dart
  :ensure t
  ;; :after lsp-mode  ;; Ensure lsp-mode is loaded first
	:pin "melpa"
  :config
  (setq lsp-dart-sdk-dir "/opt/flutter/bin/cache/dart-sdk")
	(setq lsp-dart-flutter-sdk-dir "/opt/flutter")

	)

;; Explicitly set language ID for Dart
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(dart-mode . "dart")))
(setq exec-path (append '("/opt/flutter/bin") exec-path))


(provide 'pkg-flutter)
