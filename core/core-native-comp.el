;;; pkg-native-comp.el --- Optimized native compilation for Emacs 30 -*- lexical-binding: t -*-

;;; Commentary:
;; Provides optimized automatic and manual native compilation utilities for Emacs 30

;;; Code:

(eval-when-compile
  (require 'comp))

(defgroup pkg-native-comp nil
  "Native compilation utilities for Emacs 30."
  :group 'convenience
  :prefix "pkg-native-comp-")

(defcustom pkg-native-comp-exclude-regexps
  '("\\.dir-locals\\.el$" "/\\.#" "\\.elc$")
  "List of regexps matching files to exclude from native compilation."
  :type '(repeat regexp)
  :group 'pkg-native-comp)

(defcustom pkg-native-comp-dirs
  (list user-emacs-directory)
  "Directories to include in manual native compilation updates."
  :type '(repeat directory)
  :group 'pkg-native-comp)

(defun pkg-native-comp-excluded-p (file)
  "Check if FILE should be excluded from native compilation."
  (or (null file)
      (seq-some (lambda (regexp) (string-match-p regexp file))
                pkg-native-comp-exclude-regexps)))

(defun pkg-native-comp-after-load (file)
  "Ensure FILE is natively compiled after being loaded."
  (when (and (stringp file)
             (string-match-p "\\.el\\'" file)
             (not (pkg-native-comp-excluded-p file)))
    (native-compile-async file nil nil)))

(defun pkg-native-comp-after-save ()
  (when (and buffer-file-name
             (string-match-p "\\.el\\'" buffer-file-name)
             (not (pkg-native-comp-excluded-p buffer-file-name)))
    ;; Introduce a 5-second delay before compilation
    (run-with-idle-timer 5 nil #'native-compile-async buffer-file-name nil nil)))

(defun pkg-native-comp-update ()
  "Manually update native compilation for Emacs Lisp files in specified directories."
  (interactive)
  (let ((native-comp-async-report-warnings-errors nil))
    (dolist (dir pkg-native-comp-dirs)
      (message "Native compiling in %s..." dir)
      (native-compile-async dir t nil))
    (message "Manual native compilation update initiated.")))

(defun pkg-native-comp-init ()
  "Initialize native compilation settings for Emacs 30."
  (setq native-comp-speed 3
        native-comp-deferred-compilation t
        native-comp-jit-compilation t
        native-comp-enable-subr-trampolines (native-comp-available-p)
        comp-libgccjit-reproducer nil
        native-comp-async-jobs-number (max 1 (/ (num-processors) 2))
        native-comp-async-report-warnings-errors 'silent
        package-native-compile t)

  (add-hook 'after-load-functions #'pkg-native-comp-after-load)
  (add-hook 'after-save-hook #'pkg-native-comp-after-save)

  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory))

  ;; Compile all .el files in the load-path
  (mapc (lambda (dir)
          (when (file-directory-p dir)
            (native-compile-async dir t nil)))
        load-path)

  (global-set-key (kbd "C-c C-1") #'pkg-native-comp-update))

(pkg-native-comp-init)

(provide 'core-native-comp)

;;; pkg-native-comp.el ends here
