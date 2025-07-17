;; Define directories for core and package configurations, and for savefiles
(defconst packages-dir (expand-file-name "packages" user-emacs-directory))
(defconst theme-dir (expand-file-name "theme" user-emacs-directory))

(add-to-list 'load-path packages-dir)

;; Load core and package configurations
(require 'pkg-loader)

(add-to-list 'custom-theme-load-path theme-dir)
(load-theme 'my-solarized-dark t)

(use-package emacs
  :config
  (winner-mode 1)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (when (file-exists-p (concat buffer-file-name "c"))
                            (delete-file (concat buffer-file-name "c"))))
                        nil t)))
  (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode)))
