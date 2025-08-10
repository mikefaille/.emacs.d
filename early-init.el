;; -*- lexical-binding: t; -*-

;; --- Early Performance Tweaks ---

;; Silence compiler warnings during startup (optional)
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

;; Set GC threshold high during startup, restore later
(setq gc-cons-threshold (* 128 1024 1024)) ; 128 MiB - Adjust as needed
(setq gc-cons-percentage 0.5) ; Trigger GC later

;; Defer file handler processing during startup
(defvar file-name-handler-alist-original nil)
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable package system initialization at startup (Elpaca will handle it)
(setq package-enable-at-startup nil)

;; --- Basic UI / Frame Settings ---

;; Inhibit startup screens
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Frame parameters (before initial frame is created)
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(setq scroll-bar-mode nil)

;; --- Basic Editor Settings ---

(setq-default
 make-backup-files nil          ; Disable backup files
 auto-save-default nil          ; Disable auto-save
 load-prefer-newer t            ; Prefer newer compiled files
 ring-bell-function 'ignore     ; Silence audible bell
 large-file-warning-threshold (* 100 1024 1024) ; 100 MiB threshold for large files

 ;; Performance tweaks from online discussions / Doom Emacs
 fast-but-imprecise-scrolling t
 redisplay-skip-fontification-on-input t
 inhibit-compacting-font-caches t
 idle-update-delay 1.0 ; Reduce background churn when idle

 ;; Bidirectional text processing (disable if only using LTR languages)
 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right
 ;; bidi-inhibit-bpa t ; Uncomment if needed, usually set by modes

 ;; Process communication tweaks (especially for LSP, terminals)
 read-process-output-max (* 2 1024 1024) ; 2 MiB buffer
 process-adaptive-read-buffering nil ; Disable adaptive buffering
 )

;; Default indentation settings (Consider moving to init.el/mode hooks if preferred)
(setq-default
 tab-width 2
 emacs-lisp-indent-offset 2)

;; Set environment variable for LSP performance (if using LSP Mode)
(setenv "LSP_USE_PLISTS" "true")

;; --- User Info & Version Check ---

(defvar current-user (getenv (if (eq system-type 'windows-nt) "USERNAME" "USER")) "Current system user.")
(message "Mike's .emac.d is powering up... Be patient, Master %s!" current-user)
(when (version< emacs-version "27.1") ; Elpaca requires 27.1+
  (error "Mike's dot emacs requires at least GNU Emacs 27.1 for Elpaca, but you're running %s" emacs-version))

;; --- Elpaca Package Manager Bootstrap ---
;; (Standard Elpaca bootstrap code - appears correct)
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; --- Optional Settings (Consider moving to init.el) ---

;; Outline Minor Mode Prefix (Clarify "BUG" comment or remove)
;; Outline Minor Mode Prefix (Clarify "BUG" comment or remove)
;; (setq outline-minor-mode-prefix "c o")


;; --- Restore Settings After Startup ---

;; Restore GC threshold and file handlers after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Restoring GC threshold and file handlers...")
            ;; Restore GC to a higher value suitable for LSP/heavy workloads
            (setq gc-cons-threshold (* 100 1024 1024)) ; 100 MiB - Starting point
            (setq gc-cons-percentage 0.2) ; Slightly higher percentage
            ;; Restore file handlers
            (setq file-name-handler-alist file-name-handler-alist-original)
            (message "GC threshold and file handlers restored.")
            ))

;; --- Load Core Configuration from init.el ---
;; (Commented out as these belong in init.el)

(message "Early init finished.")

;; early-init.el ends here
