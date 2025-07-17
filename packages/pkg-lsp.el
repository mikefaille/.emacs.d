;;; pkg-lsp.el --- LSP configuration with Corfu, Cape, and Kind Icon -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures LSP in Emacs with enhanced completion UI using Corfu,
;; Cape, and Kind Icon. It integrates with Dart and other supported languages,
;; providing syntax coloring and icons in completions.

;;; Code:
(require-package 'lsp-mode)
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (dart-mode . lsp-deferred)
         (lsp-completion-mode . pkg-lsp/lsp-mode-setup-completion)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . pkg-lsp/setup-corfu))
  :custom
  ;; Use Corfu for completion instead of the default LSP completion UI.
  (lsp-completion-provider :none)
  ;; Set the keymap prefix for LSP commands.
  (lsp-keymap-prefix "C-c l")
  :config
  ;; Use TAB for smart completion.
  (define-key lsp-mode-map (kbd "<tab>") #'pkg-lsp/smart-tab-completion)
  (define-key lsp-mode-map (kbd "TAB") #'pkg-lsp/smart-tab-completion))

(require-package 'lsp-dart)
(use-package lsp-dart
  :after lsp-mode
  :hook (dart-mode . lsp))

(require-package 'lsp-ui)
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  ;; Enable the documentation panel.
  (lsp-ui-doc-enable t)
  ;; Set the position of the documentation panel.
  (lsp-ui-doc-position 'bottom)
  ;; Show the documentation panel with the cursor.
  (lsp-ui-doc-show-with-cursor t)
  ;; Show diagnostics in the sideline.
  (lsp-ui-sideline-show-diagnostics t)
  ;; Don't show hover information in the sideline to prevent conflicts with Corfu.
  (lsp-ui-sideline-show-hover nil)
  ;; Show code actions in the sideline.
  (lsp-ui-sideline-show-code-actions t)
  ;; Set the update mode for the sideline.
  (lsp-ui-sideline-update-mode 'line)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

(require-package 'lsp-treemacs)
(use-package lsp-treemacs
  :after lsp-mode
  :config
  ;; Enable treemacs synchronization.
  (setq lsp-treemacs-sync-mode 1))

(require-package 'consult-lsp)
(use-package consult-lsp
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)
              ("C-c l d" . consult-lsp-diagnostics)
              ("C-c l s" . consult-lsp-symbols)
              ("C-c l f" . consult-lsp-file-symbols)))

(defun pkg-lsp/orderless-flex-first (_pattern index _total)
  "Prioritize flex matching for the first word in completion.
_PATTERN: The search pattern.
_INDEX: The index of the current candidate.
_TOTAL: The total number of candidates."
  (and (eq index 0) 'orderless-flex))


(defun pkg-lsp/lsp-mode-setup-completion ()
  "Set up completion with orderless and Cape."
  (when (fboundp 'lsp-completion-at-point)
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (add-hook 'orderless-style-dispatchers
              #'pkg-lsp/orderless-flex-first nil 'local)
    (setq-local completion-at-point-functions
                (list #'lsp-completion-at-point
                      (cape-capf-super
                       #'cape-file
                       #'cape-dabbrev)))))

(defun pkg-lsp/setup-corfu ()
  "Setup Corfu for LSP."
  ;; No additional setup needed since global-corfu-mode is active
  )

(defun pkg-lsp/smart-tab-completion ()
  "Use TAB for indentation at beginning of line or after whitespace, completion elsewhere."
  (interactive)
  (if (or (bolp) ; At beginning of line
          (looking-back "^[[:space:]]+") ; After whitespace at start
          (looking-back "\\s-")) ; After any whitespace
      (indent-for-tab-command)
    (completion-at-point)))

(provide 'pkg-lsp)
