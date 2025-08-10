;; -*- lexical-binding: t; -*-

(require 'use-package)

;; Marginalia - Rich Annotations
(use-package marginalia
  :ensure t
  :defer t ; Defer loading until needed
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; :init ;; (marginalia-mode) moved from here
  :config ;; Moved (marginalia-mode) here to run after package loads
  ;; Marginalia must be activated for annotations to appear.
  (marginalia-mode))

;; Corfu - Core Completion Framework

;; Helper function to disable Corfu in specific minibuffer scenarios if needed
;; Note: global-corfu-mode handles most minibuffer cases automatically.
(defun my/maybe-disable-corfu-in-minibuffer ()
  "Selectively disable Corfu in the minibuffer, e.g., for password prompts."
  (when (minibufferp)
    (if (eq (current-local-map) read-passwd-map)
        (corfu-mode -1))))

;; Function to transfer Corfu completion to the minibuffer via Consult
;; Requires 'consult' package to be installed and configured elsewhere.
(defun corfu-move-to-minibuffer ()
  "Transfer completion state from Corfu popup to minibuffer using Consult."
  (interactive)
  ;; Wrap the call in with-eval-after-load for compiler safety
  (with-eval-after-load 'consult
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (require 'consult) ; Ensure consult is loaded at runtime
         (consult-completion-in-region beg end table pred)))
      (_ (message "No active completion-in-region data found")))))

(use-package corfu
  :ensure t
  ;; Ensure consult is loaded before corfu setup that depends on it
  ;; Assumes 'consult' is configured elsewhere in your init files.
  :after consult
  ;; :hook (minibuffer-setup . my/maybe-disable-corfu-in-minibuffer) ; Optional override hook
  :custom
  (corfu-cycle t)                  ; Enable cycling for completion
  (corfu-auto t)                   ; Enable auto completion
  (corfu-auto-delay 0.2)           ; Delay for auto-completion (adjust as needed)
  (corfu-preselect 'first)         ; Preselect the first candidate
  (corfu-on-exact-match #'corfu-insert) ; Insert if exact match
  (corfu-preview-current nil)      ; Disable Corfu's preview if using separate preview package or none
  (corfu-popupinfo-delay '(0.5 . 0.1)) ; Delay for documentation popup (initial . subsequent)
  (corfu-popupinfo-mode t)         ; Enable documentation popup
  :bind (:map corfu-map
              ("<tab>" . corfu-next)        ; Use Tab for navigation
              ("TAB" . corfu-next)
              ("<backtab>" . corfu-previous)
              ("S-<tab>" . corfu-previous)
              ("M-<return>" . #'corfu-move-to-minibuffer) ; Transfer to minibuffer (needs Consult)
              ("<return>" . corfu-insert)   ; Use Enter to insert selection
              ("RET" . corfu-insert)
              ("<escape>" . corfu-quit)     ; Quit completion
              ("ESC" . corfu-quit))
  :init
  ;; Enable Corfu globally. Handles most buffer types including the minibuffer.
  (global-corfu-mode)
  ;; Register the command that should not interrupt Corfu completion
  ;; Wrap in eval-after-load for safety
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)))

;; Optional: Corfu History - Cycle through previous completion inputs
;; Uncomment the following block if desired.
;; (use-package corfu-history
;;   :ensure t
;;   :after corfu
;;   :hook (corfu-mode . corfu-history-mode) ; Enable with corfu-mode
;;   :custom
;;   (corfu-history-length 25)) ; Number of history items to keep

;; Orderless - Flexible Completion Matching
(use-package orderless
  :ensure t
  :custom
  ;; Define matching styles
  (orderless-matching-styles '(orderless-literal orderless-initialism orderless-flex orderless-regexp))
  ;; Set Orderless as the primary completion style
  (completion-styles '(orderless partial-completion flex basic))
  ;; Override styles for specific categories for fine-tuning
  (completion-category-overrides '((file (styles partial-completion orderless flex))
                                   (consult-location (styles orderless))
                                   (symbol (styles orderless-flex orderless-literal orderless-initialism))))
  ;; Use space as the component separator
  (orderless-component-separator "[ ]+"))

;; Cape - Completion At Point Extensions (Provides additional completion backends)
(use-package cape
  :ensure t
  :defer t ; Defer loading until needed
  ;; Ensure lsp-mode is loaded *before* configuring cape (if using lsp-mode)
  :after lsp-mode
  :config
  ;; Add LSP completion backend (ensure function name matches your LSP client)
  (with-eval-after-load 'lsp-mode
    ;; Add LSP capf first (higher priority)
    (add-to-list 'completion-at-point-functions #'lsp-completion-at-point 100)) ; Add with higher priority

  ;; Add other desired Cape backends
  (add-to-list 'completion-at-point-functions #'cape-file) ; File paths
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ; Dynamic abbreviations (Recommended)
  ;; Optional backends (uncomment if needed):
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword) ; Language keywords
  ;; (add-to-list 'completion-at-point-functions #'cape-dict) ; Dictionary words
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell) ; Spell-checker words
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev) ; Emacs abbrevs
  ;; (add-to-list 'completion-at-point-functions #'cape-history) ; Minibuffer history
  ;; (add-to-list 'completion-at-point-functions #'cape-line) ; Whole lines

  ;; Custom settings for Cape backends (if needed)
  (setq cape-dabbrev-min-length 2) ; Minimum length for dabbrev completion
  (setq cape-dabbrev-check-other-buffers t) ; Check all buffers for dabbrev candidates
  )

;; Kind Icon - Icons for Completion Candidates
(use-package kind-icon
  :ensure t
  :after corfu ; Ensure corfu is loaded first
  :config
  ;; Add the formatter to Corfu's margin formatters list
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Match Corfu's styling
  )


(use-package eglot
  :ensure t
  :defer t
  ;; Add hooks for all modes where you want Eglot LSP support
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (kcl-mode . eglot-ensure)
         (astro-ts-mode . eglot-ensure) ; Hook for Astro mode
         ;; Add other modes as needed
         ;; (python-mode . eglot-ensure)
         ;; (rust-mode . eglot-ensure)
         ;; (go-mode . eglot-ensure)
         )
  :config
  ;; Configure server programs
  ;; Eglot often finds common servers automatically if they're in PATH.
  ;; Add explicit configurations for servers Eglot might not find,
  ;; or to override the default command/arguments.

  ;; Configuration for Astro (using vscode-html-language-server as requested)
  ;; NOTE: Is vscode-html-language-server the correct server for Astro?
  ;; The official Astro LSP is usually invoked via `npx astro-ls --stdio` or similar.
  ;; Please double-check which server you intend to use.
  (add-to-list 'eglot-server-programs
               '(astro-ts-mode . ("vscode-html-language-server" "--stdio")))

  ;; Add configuration for KCL if Eglot doesn't find it automatically
  ;; (add-to-list 'eglot-server-programs
  ;;              '(kcl-mode . ("/path/to/kcl-language-server"))) ; Use absolute path or ensure it's in PATH

  ;; Add configurations for other servers if needed...

	(lsp-headerline-breadcrumb-mode)
  ;; Optional: Customize Eglot behavior
  ;; (setq eglot-connect-timeout 30)
  ;; (setq eglot-events-buffer-size 0) ; Disable events buffer if desired
  ;; (setq eglot-autoshutdown t) ; Shutdown server when last buffer is closed
  )


(provide 'pkg-corfu3)
;;; pkg-corfu3.el ends here
