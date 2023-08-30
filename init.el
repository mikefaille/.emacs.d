;; Fundamental configuration
(defvar current-user (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(message "Mike's .emac.d is powering up... Be patient, Master %s!" current-user)
(when (version< emacs-version "24.4")
  (error "Mike's dot emacs requires at least GNU Emacs 24.4, but you're running %s" emacs-version))
(setq-default
 make-backup-files nil
 auto-save-default nil
 inhibit-startup-message t
 inhibit-splash-screen t
 gc-cons-threshold most-positive-fixnum
 load-prefer-newer t
 ring-bell-function 'ignore
 large-file-warning-threshold 100000000
 native-comp-async-report-warnings-errors nil)

;; BUG
(setq outline-minor-mode-prefix "\C-c \C-o")

;; Optimizations for faster startup
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value))
		  file-name-handler-alist file-name-handler-alist-original)))

;; Define directories for core and package configurations, and for savefiles
(defconst core-dir (expand-file-name "core" user-emacs-directory))
(defconst packages-dir (expand-file-name "packages" user-emacs-directory))
(defconst theme-dir (expand-file-name "theme" user-emacs-directory))
(defconst main-savefile-dir (expand-file-name "savefile" user-emacs-directory))


(add-to-list 'load-path core-dir)
(add-to-list 'load-path packages-dir)

;; Ensure savefile directory exists
(unless (file-exists-p main-savefile-dir)
  (make-directory main-savefile-dir))

;; Load core and package configurations
(require 'core-packages)
(require 'core-look)
(require 'core-feel)
(require 'core-util)
(require 'pkg-org)
(require 'pkg-discover)
(require 'pkg-git)
(require 'pkg-company)
(require 'pkg-flycheck)
(require 'pkg-ssh)
(require 'pkg-docker)
(require 'pkg-feel)
(require 'pkg-lsp-python)
(require 'pkg-latex)
(require 'pkg-yaml)
(require 'pkg-php)
(require 'pkg-projectile)
(require 'pkg-bash)
(require 'pkg-search)
(require 'pkg-eshell)
(require 'pkg-rust)
(require 'pkg-emms)
(require 'pkg-web)
(require 'pkg-systemd)
(require 'pkg-multipleCursor)
(require 'pkg-mouvement)
;; (require 'pkg-dhall)
;; (require 'pkg-terraform-lsp)
(require 'pkg-go)
(require 'pkg-irc)
(require 'pkg-ido)
(add-to-list 'custom-theme-load-path theme-dir)
(load-theme 'my-solarized-dark t)
;; (require 'pkg-theme)
;; Execute the following only if emacs is compiled with treesit
;; (if (featurep 'treesit)
;;    (require 'pkg-combobulate))



;; Remove .elc files on save
(defun remove-elc-on-save ()
  "If you're saving an Elisp file, the .elc is likely no longer valid."
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))))

;; Other configurations
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; Enable winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Cleanup whitespace on save
(add-hook 'before-save-hook 'whitespace-cleanup)
