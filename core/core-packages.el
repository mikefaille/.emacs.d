;;; Core-packages.el ---

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-always-demand nil
      use-package-expand-minimally t)

(defvar core-modules '(smartparens idle-highlight-mode find-file-in-project smex scpaste helm flycheck undo-tree dired-hacks-utils flycheck eshell-prompt-extras fuzzy deferred auto-async-byte-compile markdown-mode async))

(defvar package-refreshed-this-session nil)

(defun maybe-refresh-package-contents (package)
  "Refresh package contents if PACKAGE is not installed and if contents have not been refreshed this session."
  (unless (or package-refreshed-this-session (package-installed-p package))
    (package-refresh-contents)
    (setq package-refreshed-this-session t)))

(defun require-package (package)
  "Ensure PACKAGE is installed."
  (unless (package-installed-p package)
    (maybe-refresh-package-contents package)
    (eval `(use-package ,package
             :ensure t))))

(defun module-list-install (modules-list)
  "Install missing modules from MODULES-LIST."
  (dolist (module modules-list)
    (require-package module)))

(module-list-install core-modules)

(defun add-subfolders-to-load-path (parent-dir)
  "Add subfolders of PARENT-DIR to load path."
  (let ((default-directory parent-dir))
    (add-to-list 'load-path parent-dir)
    (normal-top-level-add-subdirs-to-load-path)))

;; Enable native compilation if possible
(when (boundp 'comp-deferred-compilation)
  (setq comp-deferred-compilation t))
(when (boundp 'comp-speed)
  (setq comp-speed 3))

(provide 'core-packages)
;;; core-packages.el ends
