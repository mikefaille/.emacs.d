;; -*- lexical-binding: t; -*-
;; pkg-ido.el --- Optimized Ido configuration using ido-completing-read+

(require 'use-package)

;; --- Base Ido Setup (Prerequisite for ido-completing-read+) ---
(use-package ido
  :ensure nil ; Built-in
  :config
  ;; Basic ido settings needed before ido-ubiquitous/ido-completing-read+
  (setq ido-enable-flex-matching t) ; Enable flexible matching
  (setq ido-use-faces t)            ; Use faces for highlighting
  (setq ido-create-new-buffer 'always)
  (setq ido-use-filename-at-point 'guess)
  ;; Add other base ido customizations here if needed

  ;; Enable ido-mode itself (required by ido-completing-read+)
  (ido-mode 1)
  ;; Enable ido-everywhere (required by ido-completing-read+)
  (ido-everywhere 1))

;; --- Core Ido Enhancement (ido-completing-read+) ---
;; Provides robust ido integration across Emacs
(use-package ido-completing-read+
  :ensure t ; Make sure this installs successfully!
  :after ido ; Ensure base ido is configured first
  :config
  ;; Enable the enhanced ubiquitous mode
  (ido-ubiquitous-mode 1)
  ;; Review customizations if upgrading from older ido-ubiquitous
  ;; M-x customize-group ido-completing-read+
  )

;; --- Optional Ido Enhancements (Recommended by ido-completing-read+ README) ---

;; Use AMX (Alternative M-x) for enhanced command execution
;; Integrates with Ido and provides frequency sorting.
(use-package amx
  :ensure t
  :after ido-completing-read+ ; Load after main ido setup
  :config
  (amx-mode 1)) ; Enable amx globally

;; Use ido for yes/no prompts (Optional)
;; (use-package ido-yes-or-no
;;   :ensure t
;;   :after ido-completing-read+
;;   :config
;;   (ido-yes-or-no-mode 1))

;; Use ido for commands using completing-read-multiple (Optional)
;; (use-package crm-custom
;;   :ensure t
;;   :after ido-completing-read+
;;   :config
;;   (crm-custom-mode 1))

;; --- Other Related Packages ---

;; Configure browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :bind (("M-y" . browse-kill-ring)) ; Example binding (adjust if M-y used elsewhere)
  )

;; Notes:
;; - This setup relies on successfully installing 'ido-completing-read+' from MELPA.
;; - It uses ido-ubiquitous-mode for broad Ido integration.
;; - AMX handles M-x.
;; - Optional packages ido-yes-or-no and crm-custom are included but commented out.
;; - Ensure previous errors (require-package, duplicate marginalia, icomplete-mode) are resolved elsewhere.

;; Mark this file as provided
(provide 'pkg-ido)
;;; pkg-ido.el ends here
