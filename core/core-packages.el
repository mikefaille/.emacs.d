;; -*- lexical-binding: t; -*-
;;; core-packages.el --- Package system setup and use-package defaults

;;; Code:

;; Note: Elpaca is bootstrapped in early-init.el and handles most things.
;; These settings are for general use-package defaults and legacy support.

(require 'use-package)

;; Ensure foundational dependencies are available
(use-package cond-let
  :ensure (:host github :repo "tarsius/cond-let"))
(elpaca-wait)

;; Configure use-package defaults
(setq use-package-always-ensure t   ; Default to installing missing packages (via Elpaca)
      use-package-always-defer t    ; Default to deferring package loading
      use-package-always-demand nil ; Don't demand loading by default
      use-package-expand-minimally t ; Use faster macroexpansion
      )

;; --- require-package Definition (for compatibility) ---
(defvar package-refreshed-this-session nil
  "Flag to avoid refreshing package contents multiple times.")

(defun require-package (package)
  "Ensure PACKAGE is installed, using use-package. For compatibility."
  (unless (package-installed-p package)
    (condition-case err
        (eval `(use-package ,package :ensure t))
      (error (message "Failed to install %s via require-package: %S" package err)))))

(provide 'core-packages)
;;; core-packages.el ends here
