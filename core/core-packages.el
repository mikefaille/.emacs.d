;; -*- lexical-binding: t; -*-
;;; core-packages.el --- Package system setup and use-package defaults

;;; Code:

;; Ensure the 'package' system is loaded (usually done by default, but safe)
(require 'package)

;; Configure package archives and priorities
;; Ensure Elpaca (if used) is configured in early-init.el to handle archives primarily.
;; These settings serve as a fallback or configuration for package.el if needed.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/"))) ; Renamed elpa to gnu for clarity

;; Note: Priorities are less relevant when using Elpaca, which manages sources per-package.
;; Set priorities if primarily using package.el
(setq package-archive-priorities '(("org" . 10) ; Give Org ELPA high priority
                                   ("melpa-stable" . 8)
                                   ("melpa" . 5)
                                   ("gnu" . 1)))

;; Initialize the package system (required before first package-install or use-package :ensure)
;; Elpaca's bootstrap in early-init.el might make this redundant, but harmless.
(package-initialize)

;; Bootstrap use-package itself if not already installed
(unless (package-installed-p 'use-package)
  ;; Refresh only if use-package needs installing
  ;; Avoid automatic refresh otherwise, as it can slow startup significantly.
  (message "use-package not found. Refreshing package contents...")
  (package-refresh-contents)
  (package-install 'use-package))

;; Ensure use-package is loaded for the settings below
(require 'use-package)

;; Configure use-package defaults
(setq use-package-always-ensure t   ; Default to installing missing packages
      use-package-always-defer t    ; Default to deferring package loading
      use-package-always-demand nil ; Don't demand loading by default
      use-package-expand-minimally t ; Use faster macroexpansion
      ;; load-prefer-newer t ; Set this in early-init.el for best effect
      )

;; --- require-package Definition (for compatibility) ---
;; Note: It is generally recommended to replace calls to require-package
;; with standard `(use-package PACKAGE :ensure t ...)` blocks elsewhere
;; in your configuration. This definition is provided for compatibility
;; with existing code calling require-package.

(defvar package-refreshed-this-session nil
  "Flag to avoid refreshing package contents multiple times.")

(defun maybe-refresh-package-contents (package)
  "Refresh package contents if PACKAGE is not installed and if contents have not been refreshed this session."
  (unless (or package-refreshed-this-session (package-installed-p package))
    (message "Package '%s' not installed. Refreshing package contents..." package)
    (package-refresh-contents)
    (setq package-refreshed-this-session t)))

(defun require-package (package)
  "Ensure PACKAGE is installed, using use-package. For compatibility."
  (unless (package-installed-p package)
    (maybe-refresh-package-contents package)
    (condition-case err
        ;; Use `use-package` to handle installation via :ensure t
        ;; Evaluating use-package like this is non-standard but done here
        ;; to fulfill the wrapper's original intent.
        (eval `(use-package ,package :ensure t))
      (error (message "Failed to install %s via require-package: %S" package err)))))

;; --- End require-package Definition ---


;; Optional: Configure auto-compilation (byte-compiling .el files)
;; Native compilation often takes precedence if enabled.
(use-package auto-compile
  :ensure t ; Ensure auto-compile itself is installed
  :config (auto-compile-on-load-mode))


(provide 'core-packages)
;;; core-packages.el ends here
