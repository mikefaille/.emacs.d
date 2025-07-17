;;; Core-packages.el ---

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities '(("org"  . 5)
				   ("melpa-stable" . 3)
                                   ("melpa"    . 2)
                                   ("elpa" . 1)))

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

(defun require-package (package)
  "Ensure PACKAGE is installed, with error handling."
  (unless (package-installed-p package)
    (condition-case err
        (progn
          (package-refresh-contents)
          (package-install package))
      (error (message "Failed to install %s: %S" package err)))))

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


(use-package auto-compile
  :config (auto-compile-on-load-mode))
;; we want to make sure that all packages added for configuration are installed, for this we use use-package-ensure.
;; Always compile packages, and use the newest version available.

(setq load-prefer-newer t)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(provide 'core-packages)
;;; core-packages.el ends
