;;; pkg-lsp.el --- LSP configuration with Corfu, Cape, and Kind Icon -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures LSP in Emacs with enhanced completion UI using Corfu,
;; Cape, and Kind Icon. It integrates with Dart and other supported languages,
;; providing syntax coloring and icons in completions.

;;; Code:

(require 'use-package)
(require 'orderless)


(add-hook 'prog-mode-hook 'my-prog-mode-function)

(defun my-prog-mode-function ()
  "This function will run in all prog-mode derived modes."
  (message "Programming mode activated!")
  ;; Add your customizations here, for example:
  (display-line-numbers-mode 1)
  (electric-indent-mode 1)
  ;; (dart-indent-level 2)
)

;; **1. Disable Automatic Package Initialization**
;; Prevent Emacs from initializing packages automatically to avoid conflicts.
(setq package-enable-at-startup nil)

;; ;; **2. Configure Package Archives and Initialize Package System**
;; (require 'package)
;; (setq package-archives
;;       '(("melpa" . "https://melpa.org/packages/")
;;         ("org"   . "https://orgmode.org/elpa/")
;;         ("gnu"   . "https://elpa.gnu.org/packages/")))
;; (package-initialize)

;; **4. Install and Configure `all-the-icons`**
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))  ;; Only load in GUI Emacs

;; Run this command after installation to install fonts:
;; M-x all-the-icons-install-fonts RET
;; Follow the prompts to install the necessary fonts.

;; **5. Configure Corfu for Completion**
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)  ;; Enable Corfu globally
  ;; Enable Corfu in the minibuffer as well
  (add-hook 'minibuffer-setup-hook #'corfu-mode)
  :config
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                  ;; Enable auto completion
  (setq corfu-auto-delay 0.0)          ;; No delay for auto completion
  (setq corfu-min-width 40)            ;; Minimum width of Corfu popup
  (setq corfu-max-width 100))          ;; Maximum width of Corfu popup

;; **6. Install and Configure `cape` for Enhanced Completion**
(use-package cape
  :ensure t
  :after corfu)



;; **9. Define Custom Functions for Completion Behavior**

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


(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (dart-mode . lsp-deferred)

         (lsp-completion-mode . pkg-lsp/lsp-mode-setup-completion)  ; Remove quote
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . pkg-lsp/setup-corfu))
  :custom
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  :config
  (define-key lsp-mode-map (kbd "<tab>") #'pkg-lsp/smart-tab-completion)
  (define-key lsp-mode-map (kbd "TAB") #'pkg-lsp/smart-tab-completion))


;; **11. Configure `lsp-dart` for Dart Support**
(use-package lsp-dart
  :ensure t
  :after lsp-mode
  :hook (dart-mode . lsp))

;; **12. Configure `lsp-ui` for Enhanced LSP UI**
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)      ;; Disable hover to prevent conflict with Corfu
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'line)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

;; **13. Configure `lsp-treemacs` for Tree-based UI**
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-treemacs-sync-mode 1))

;; **14. Configure `consult-lsp` for Enhanced LSP Integration with Consult**
(use-package consult-lsp
  :ensure t
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)
              ("C-c l d" . consult-lsp-diagnostics)
              ("C-c l s" . consult-lsp-symbols)
              ("C-c l f" . consult-lsp-file-symbols)))

;; **15. Define Additional Setup Function**
(defun pkg-lsp-setup ()
  "Set up LSP for supported modes."
  ;; Placeholder for additional custom configurations
  )

;;;###autoload
(defun pkg-lsp-init ()
  "Initialize pkg-lsp configuration."
  (pkg-lsp-setup))

;; **16. Bind TAB Key Within `lsp-mode`**
;; This ensures that the key bindings are set after `lsp-mode` is loaded
(add-hook 'lsp-mode-hook
          (lambda ()
            (define-key lsp-mode-map (kbd "<tab>") #'pkg-lsp/smart-tab-completion)
            (define-key lsp-mode-map (kbd "TAB") #'pkg-lsp/smart-tab-completion)))

(provide 'pkg-lsp)


;;; pkg-lsp.el ends here
;; ;;; pkg-lsp.el --- LSP configuration compatible with general completion -*- lexical-binding: t -*-

;; ;;; Commentary:
;; ;; This package provides LSP configuration that works alongside
;; ;; the general completion setup in pkg-corfu2.el

;; ;;; Code:

;; (require 'use-package)


;; (use-package lsp-mode
;;   :ensure t
;;   :defer t
;;   :commands (lsp lsp-deferred)
;;   :hook ((python-mode go-mode) . lsp-deferred)
;;   :custom
;;   ;; (lsp-completion-provider :none) ; we use Corfu!
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-auto-configure t)
;;   (lsp-idle-delay 0.500)
;;   (lsp-keep-workspace-alive nil)
;;   (lsp-auto-guess-root t)
;;   (lsp-prefer-flymake nil)
;;   (lsp-enable-completion-at-point t)
;;   (lsp-enable-indentation t)
;;   (lsp-enable-on-type-formatting t)
;;   (lsp-enable-snippet nil)

;;   :init
;;   (defun my/orderless-dispatch-flex-first (_pattern index _total)
;;     (and (eq index 0) 'orderless-flex))

;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))

;;     ;; Configure the first word as flex filtered.
;;     (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

;;     ;; Use lsp-completion-at-point directly for LSP completion
;;     (setq-local completion-at-point-functions '(lsp-completion-at-point)))

;;   :config
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)
;;   (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
;;   )

;; (defun corfu-lsp-setup ()

;;   ;; Configure python-lsp-server (pylsp)
;;   (with-eval-after-load 'lsp-pylsp
;;     (setq lsp-pylsp-plugins-ruff-enabled t
;;           lsp-pylsp-plugins-ruff-select ["I" "E" "F" "UP" "B" "SIM"]
;;           lsp-pylsp-plugins-ruff-format ["E" "F"]
;;           lsp-pylsp-plugins-ruff-unsafe-fixes "UP034"
;;           lsp-pylsp-plugins-ruff-formatEnabled t

;;           ;; Disable other linters in favor of Ruff
;;           lsp-pylsp-plugins-pycodestyle-enabled nil
;;           lsp-pylsp-plugins-mccabe-enabled nil
;;           lsp-pylsp-plugins-pyflakes-enabled nil
;;           lsp-pylsp-plugins-autopep8-enabled nil
;;           lsp-pylsp-plugins-yapf-enabled nil

;;           ;; Enhance Jedi settings
;;           lsp-pylsp-plugins-jedi-completion-enabled t
;;           lsp-pylsp-plugins-jedi-completion-include-params t
;;           lsp-pylsp-plugins-jedi-completion-include-class-objects t
;;           lsp-pylsp-plugins-jedi-completion-fuzzy t
;;           lsp-pylsp-plugins-jedi-completion-auto-import-modules t
;;           lsp-pylsp-plugins-jedi-hover-enabled t
;;           lsp-pylsp-plugins-jedi-signature-help-enabled t
;;           lsp-pylsp-plugins-jedi-symbols-enabled t
;;           lsp-pylsp-plugins-jedi-use-pyenv-environment t
;;           lsp-pylsp-plugins-jedi-completion-max-diagnostics 1000)))

;; (use-package lsp-ui
;;   :ensure t
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-position 'bottom)
;;   (lsp-ui-doc-show-with-cursor t)
;;   (lsp-ui-sideline-show-diagnostics t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-sideline-update-mode 'line)
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references)))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :after lsp-mode
;;   :config
;;   (setq lsp-treemacs-sync-mode 1))

;; (use-package consult-lsp
;;   :ensure t
;;   :after (lsp-mode consult)
;;   :bind (:map lsp-mode-map
;;               ([remap xref-find-apropos] . consult-lsp-symbols)
;;               ("C-c l d" . consult-lsp-diagnostics)
;;               ("C-c l s" . consult-lsp-symbols)
;;               ("C-c l f" . consult-lsp-file-symbols)))

;; (defun pkg-lsp-setup ()
;;   "Set up LSP for supported modes."
;;   ;; Additional setup if needed
;;   )

;; ;;;###autoload
;; (defun pkg-lsp-init ()
;;   "Initialize pkg-lsp configuration."
;;   (pkg-lsp-setup))

;; (provide 'pkg-lsp)

;; ;;; pkg-lsp.el ends here
