;; -*- lexical-binding: t; -*-
;;; init.el --- Main Emacs configuration file for Michaël Faille

;;; Commentary:
;; This file loads core configuration, sets up the theme, tree-sitter,
;; loads package-specific setups, and applies final customizations.
;; It assumes early-init.el has run.

;;; Code:

;; --- Load Path & Directory Setup ---
(defvar michael-core-dir (expand-file-name "core" user-emacs-directory)
  "Directory for core configuration files.")
(defvar michael-packages-dir (expand-file-name "packages" user-emacs-directory)
  "Directory for package-specific configuration files.")

(defvar michael-savefile-dir (expand-file-name "var" user-emacs-directory) ; Use 'var' convention
  "Directory for savefiles (recentf, savehist, etc.).")
(defvar michael-theme-dir (expand-file-name "theme" user-emacs-directory) ; If you have custom themes
  "Directory for custom theme files.")

;; Ensure savefile directory exists
(unless (file-directory-p michael-savefile-dir)
  (make-directory michael-savefile-dir t))

;; Add custom configuration directories to the load-path
(add-to-list 'load-path michael-core-dir)
(add-to-list 'load-path michael-packages-dir)
;; Add custom theme directory if it exists and you use it
;; (add-to-list 'custom-theme-load-path michael-theme-dir)


;; --- Load Core Configuration ---

;; Load package system setup (archives, use-package defaults)
;; This MUST come before files that use use-package or require packages.
(require 'core-packages)

;; Load other core modules (adjust order if dependencies exist)
(require 'core-util)          ; Utility functions/macros
(require 'core-font)          ; Font setup (load early for appearance)
(require 'core-feel)          ; Basic editing feel (keys, modes)
(require 'core-look)          ; Visual appearance (mode line, UI elements)
(require 'core-native-comp)   ; Native compilation settings


;; --- Theme Loading ---
;; Load desired theme after core setup but before most packages
(use-package solarized-theme
  :ensure t ; Ensure it's installed
  :config
  ;; Load theme, handling daemon case if necessary (using logic from early-init is better)
  ;; Simple version:
  (when (display-graphic-p)
     (load-theme 'solarized-dark t))
  ;; Consider moving the daemon-aware loading logic from early-init here if preferred
  )

;; In init.el
(use-package markdown-mode
  :ensure t
  :defer t) ; Defer unless you use markdown-mode directly often
;; --- Tree-sitter Setup ---
;; Configure Tree-sitter and related packages
(use-package tree-sitter
  :ensure t
  :defer t
  :config
  ;; Add language mappings if needed (often handled by major modes or treesit-auto)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . typescript))
  )

(use-package tree-sitter-langs ;; Installs grammar bundles
  :ensure t
  :defer t
  :after tree-sitter)

(use-package treesit-auto          ;; Auto-install grammars and setup modes
  :ensure t
  :defer t
  :after (tree-sitter tree-sitter-langs) ; Ensure base packages are loaded
  :custom
  (treesit-auto-install 'prompt) ; Or t to install automatically
  (treesit-auto-modes 'all)      ; Automatically configure modes for installed languages
  (treesit-auto-exclude-modes '(fundamental-mode markdown-mode org-mode)) ; Exclude modes
  :config
  ;; Set the directory for storing downloaded grammars
  (setq treesit-extra-load-path (list (expand-file-name "tree-sitter" michael-savefile-dir)))
  (global-treesit-auto-mode)) ; Enable the auto-configuration globally

;; Major modes using Tree-sitter
(use-package typescript-ts-mode
  :ensure t
  :defer t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; Optional: Indentation bars
(use-package indent-bars
  :ensure t
  :defer t
  :hook (prog-mode . indent-bars-mode))


;; --- Load Package-Specific Configurations ---
;; Load configurations for individual packages or groups of packages.
;; These files should contain the `use-package` blocks for the actual packages.
(message "Loading package configurations...")

(require 'pkg-ido)            ; Ido completion setup (or Vertico/other)
(require 'pkg-corfu3)         ; Corfu completion setup
(require 'pkg-org)            ; Org mode configuration
(require 'pkg-discover)       ; discover.el configuration
(require 'pkg-git)            ; Git integration (e.g., Magit)
(require 'pkg-flycheck)       ; Flycheck setup
(require 'pkg-lsp)           ; LSP Mode / Eglot setup
(require 'pkg-projectile)     ; Project management
(require 'pkg-web)            ; Web development modes/tools
(require 'pkg-docker)         ; Docker integration
(require 'pkg-eshell)         ; Eshell enhancements
(require 'pkg-multipleCursor) ; multiple-cursors setup
(require 'pkg-search)         ; Search tools (Consult, etc.)
;; ... add require lines for all other pkg-*.el files you use ...
;; (require 'pkg-ssh)
;; (require 'pkg-latex)
;; (require 'pkg-yaml)
;; (require 'pkg-php)
;; (require 'pkg-bash)
;; (require 'pkg-rust)
;; (require 'pkg-emms)
;; (require 'pkg-systemd)
;; (require 'pkg-mouvement)
;; (require 'pkg-irc)
;; (require 'pkg-flutter)
;; (require 'pkg-terraform)

(message "Package configurations loaded.")


;; --- Other Settings ---

;; Winner mode (Undo/redo window configurations)
(use-package winner
  :ensure nil ; Built-in
  :config
  (winner-mode 1))

;; Save History (Minibuffer history persistence)
(use-package savehist
  :ensure nil ; Built-in
  :custom
  (savehist-file (expand-file-name "savehist" michael-savefile-dir))
  (history-length 1000) ; Increase history size
  :config
  (savehist-mode 1))

;; General Emacs behavior tweaks
(use-package emacs
  :ensure nil
  :config
  ;; Hook to delete .elc files (Consider downsides: hides compile errors, interaction with native-comp)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (let ((elc-file (concat buffer-file-name "c")))
                            (when (file-exists-p elc-file)
                              (message "Deleting old elc file: %s" elc-file)
                              (delete-file elc-file))))
                        nil t))) ; t makes hook buffer-local

  ;; Auto-revert log files
  (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode)))


;; --- Custom Settings ---
;; Load customizations saved via M-x customize (or manually edited)
;; Place this at the end to ensure it overrides other settings if necessary.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(when (and custom-file (file-exists-p custom-file))
  (load custom-file 'noerror 'nomessage))


;; --- Finalization ---
;; Remove misplaced theme loading code from original snippet
;; :config
(message "[Theme Debug] Attempting to load solarized-dark...")
(load-theme 'solarized-dark t)
(message "[Theme Debug] Theme load attempt finished.")

(message "Michaël's Emacs configuration loaded successfully.")

;;; init.el ends here
