;; -*- lexical-binding: t; -*-
;;; core-native-comp.el --- Configuration for Emacs Native Compilation

;;; Commentary:
;; Provides settings and utilities for Emacs native compilation.
;; Leverages built-in deferred compilation and adds an after-save hook
;; for faster recompilation during development.

;;; Code:

(require 'use-package)
(eval-when-compile (require 'comp)) ; Needed for native-comp-* vars at compile time

;; --- Configuration Group ---
(defgroup core-native-comp nil
  "Native compilation utilities."
  :group 'convenience
  :prefix "core-native-comp-")

;; --- Customizable Variables ---
(defcustom core-native-comp-exclude-regexps
  '("\\.dir-locals\\.el$" "/\\.#" "\\.elc$")
  "List of regexps matching files to exclude from native compilation hooks."
  :type '(repeat regexp)
  :group 'core-native-comp)

(defcustom core-native-comp-dirs
  (list user-emacs-directory) ; Compile user config by default
  "Directories to include in manual native compilation updates."
  :type '(repeat directory)
  :group 'core-native-comp)

;; --- Helper Function ---
(defun core-native-comp--excluded-p (file)
  "Check if FILE should be excluded based on `core-native-comp-exclude-regexps`."
  (or (null file)
      (seq-some (lambda (regexp) (string-match-p regexp file))
                core-native-comp-exclude-regexps)))

;; --- Hook Function for After Save ---
(defun core-native-comp--compile-after-save ()
  "Asynchronously native compile current buffer after save (with delay)."
  (when (and buffer-file-name
             (string-match-p "\\.el\\'" buffer-file-name)
             (not (core-native-comp--excluded-p buffer-file-name)))
    ;; Add a short delay before compiling to avoid issues with rapid saves
    (run-with-idle-timer 3 nil #'native-compile-async buffer-file-name nil nil)))

;; --- Manual Update Command ---
(defun core-native-comp-update ()
  "Manually update native compilation for files in `core-native-comp-dirs`."
  (interactive)
  (let ((native-comp-async-report-warnings-errors nil)) ; Suppress warnings for bulk compile
    (dolist (dir core-native-comp-dirs)
      (message "Native compiling in %s..." dir)
      ;; Recursively compile the directory
      (native-compile-async dir t nil)))
  (message "Manual native compilation update initiated."))

;; --- Configuration using use-package ---
(use-package native-comp
  :ensure nil ; Built-in feature
  :custom
  ;; Core native compilation settings
  (native-comp-speed 3) ; Optimization level (0-3)
  (native-comp-deferred-compilation t) ; Enable built-in async compilation
  (native-comp-jit-compilation t) ; Enable Just-In-Time compilation (Emacs 30+)
  ;; Check availability for trampolines (Emacs 29+)
  (native-comp-enable-subr-trampolines (native-comp-available-p))
  (comp-libgccjit-reproducer nil) ; Disable reproducer files unless debugging libgccjit
  ;; Adjust async jobs based on CPU cores (ensure at least 1)
  (native-comp-async-jobs-number (max 1 (/ (num-processors) 2)))
  ;; How to report async compilation warnings/errors ('silent, 'notify, 'message)
  (native-comp-async-report-warnings-errors 'silent)
  ;; Tell package managers (like Elpaca) to native compile packages
  (package-native-compile t)

  :init
  ;; Add user's eln-cache to the load path (ensure directory exists)
  (let ((eln-cache-dir (expand-file-name "eln-cache/" user-emacs-directory)))
    (unless (file-directory-p eln-cache-dir)
      (make-directory eln-cache-dir t))
    (add-to-list 'native-comp-eln-load-path eln-cache-dir))

  :config
  ;; Add the hook for faster recompilation after saving files
  (add-hook 'after-save-hook #'core-native-comp--compile-after-save)

  ;; Define keybinding for manual update
  (global-set-key (kbd "C-c C-n") #'core-native-comp-update))

;; Mark this file as provided
(provide 'core-native-comp)
;;; core-native-comp.el ends here
