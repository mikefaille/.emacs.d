(require 'use-package)
(require 'orderless)
(require 'pkg-corfu2)

;; ... other non-completion related config (all-the-icons, prog-mode-hook)

;; Orderless configuration for LSP
(defun pkg-lsp/orderless-flex-first (_pattern index _total)
  (and (eq index 0) 'orderless-flex))

(defun pkg-lsp/lsp-mode-setup-completion ()
  (when (fboundp 'lsp-completion-at-point)
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (add-hook 'orderless-style-dispatchers
              #'pkg-lsp/orderless-flex-first nil 'local)
    (setq-local completion-at-point-functions
                (append '(lsp-completion-at-point) completion-at-point-functions))))

;; Smart Tab Completion
(defun pkg-lsp/smart-tab-completion ()
  (interactive)
  (cond
   ((or (bolp)
        (looking-back "^[[:space:]]+")
        (looking-back "\\s-"))
    (indent-for-tab-command))
   (t (completion-at-point))))  ; Use completion-at-point directly

;; Setup Corfu with LSP
(defun pkg-lsp/setup-corfu ()
  (corfu-mode))

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (dart-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . pkg-lsp/setup-corfu))
  :custom
  (lsp-completion-provider :none)  ; Use Corfu for completion
  (lsp-keymap-prefix "C-c l")
  :config
  ;; Bind TAB for smart completion (only once)
  (define-key lsp-mode-map (kbd "<tab>") #'pkg-lsp/smart-tab-completion)
  (define-key lsp-mode-map (kbd "TAB") #'pkg-lsp/smart-tab-completion)
  ;; Setup completion after lsp-completion-mode is activated
  (add-hook 'lsp-completion-mode-hook #'pkg-lsp/lsp-mode-setup-completion))

;; Configure lsp-dart for Dart Support
(use-package lsp-dart
  :ensure t
  :after lsp-mode
  :hook (dart-mode . lsp))

;; Configure lsp-ui for Enhanced LSP UI
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil) ; Disable hover to prevent conflict with Corfu
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'line)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

;; Configure lsp-treemacs for Tree-based UI
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-treemacs-sync-mode 1))

;; Configure consult-lsp for Enhanced LSP Integration with Consult
(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)
              ("C-c l d" . consult-lsp-diagnostics)
              ("C-c l s" . consult-lsp-symbols)
              ("C-c l f" . consult-lsp-file-symbols)))

;; Define Additional Setup Function (Currently Empty)
(defun pkg-lsp-setup ()
  "Set up LSP for supported modes."
  ;; Placeholder for additional custom configurations
  )

;;;###autoload
(defun pkg-lsp-init ()
  "Initialize pkg-lsp configuration."
  (pkg-lsp-setup))

(provide 'pkg-lsp)
