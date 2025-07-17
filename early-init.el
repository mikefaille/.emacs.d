(setq package-enable-at-startup nil)

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
 read-process-output-max (* 1024 1024) ; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
 native-comp-async-report-warnings-errors nil)

; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")



(global-display-line-numbers-mode 1)

;; Optimizations for faster startup
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 100000000 ;; 100mb
		  file-name-handler-alist file-name-handler-alist-original)))

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


(defconst main-savefile-dir (expand-file-name "savefile" user-emacs-directory))
;; Ensure savefile directory exists
(unless (file-exists-p main-savefile-dir)
  (make-directory main-savefile-dir))


(defconst core-dir (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path core-dir)

(require 'core-packages)
(require 'core-feel)
(require 'core-util)
(require 'core-look)

(package-initialize)
