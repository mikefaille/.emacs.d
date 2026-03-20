;; -*- lexical-binding: t; -*-
;;; init.el --- Main Emacs configuration file for Michaël Faille

;;; Commentary:
;; This file loads core configuration, sets up the theme, tree-sitter,
;; and handles lazy loading of package configurations via hooks/mode-triggers.

;;; Code:

;; --- Load Path & Directory Setup ---
(defvar michael-core-dir (expand-file-name "core" user-emacs-directory))
(defvar michael-packages-dir (expand-file-name "packages" user-emacs-directory))
(defvar michael-savefile-dir (expand-file-name "var" user-emacs-directory))
(defvar michael-theme-dir (expand-file-name "theme" user-emacs-directory))

(unless (file-directory-p michael-savefile-dir) (make-directory michael-savefile-dir t))

(add-to-list 'load-path michael-core-dir)
(add-to-list 'load-path michael-packages-dir)

;; --- Load Core Configuration (Always Needed) ---
(require 'core-packages)
(require 'core-util)
(require 'core-font)
(require 'core-feel)
(require 'core-look)
(require 'core-native-comp)

;; --- Theme & UI (Always Needed) ---
(use-package solarized-theme
  :ensure t
  :demand t
  :config (load-theme 'solarized-dark t))
(elpaca-wait)

;; --- Modern Stack (Core Experience) ---
(require 'pkg-completion)     ; Vertico, Corfu, etc.
(require 'pkg-workspaces)     ; Perspective
(require 'pkg-lsp)            ; Loads hooks, but lsp-mode itself is deferred

;; --- Package Configurations (Hooks & Mode Triggers) ---
;; We use 'require' here because the pkg-*.el files themselves use use-package
;; with :defer t, :mode, or :hook, which handles the lazy loading properly.

;; AI Tools
(require 'pkg-ai)

;; QoL & Feel
(require 'pkg-feel)

;; Tree-sitter & Astro
(require 'pkg-treesiter)

;; Auto Compilation
(require 'pkg-auto-compile)

;; Project Management
(require 'pkg-projectile)

;; Org Mode
(require 'pkg-org)

;; Git
(require 'pkg-git)

;; Languages (Lazy via their own files)
(require 'pkg-bash)
(require 'pkg-go)
(require 'pkg-haskell)
(require 'pkg-rust)
(require 'pkg-latex)
(require 'pkg-yaml)
(require 'pkg-terraform)
(require 'pkg-markdown)

;; Other Integrations
(require 'pkg-docker)
(require 'pkg-eshell)
(require 'pkg-multipleCursor)
(require 'pkg-gemini)
(require 'pkg-ssh)
(require 'pkg-systemd)

;; --- Optional / Switchable Features (Commented Out) ---
;; (require 'pkg-chrome)
;; (require 'pkg-combobulate)
;; (require 'pkg-mu4e)

(message "Michaël's Emacs configuration loaded successfully.")
;;; init.el ends here
