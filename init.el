;; Define directories for core and package configurations, and for savefiles
(defconst packages-dir (expand-file-name "packages" user-emacs-directory))
(defconst theme-dir (expand-file-name "theme" user-emacs-directory))

(add-to-list 'load-path packages-dir)

;; Load core and package configurations
(require 'pkg-ido)
(require 'pkg-corfu2)
(require 'pkg-org)
(require 'pkg-discover)
(require 'pkg-git)
;; (require 'pkg-company)
(require 'pkg-flycheck)
(require 'pkg-ssh)
(require 'pkg-docker)
(require 'pkg-feel)
(require 'pkg-lsp)
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
;; (require 'pkg-go)
(require 'pkg-irc)
(require 'pkg-straight)

(require 'pkg-flutter)

;; (require 'pkg-corfu)

;; (require 'pkg-combobulate)
(add-to-list 'custom-theme-load-path theme-dir)
;; (require 'pkg-theme)
(load-theme 'my-solarized-dark t)

(straight-use-package 'gptel)

;; Llama.cpp offers an OpenAI compatible API
;; (gptel-make-openai "llama-cpp"          ;Any name
;;   :stream t                             ;Stream responses
;;   :protocol "http"
;;   :host "localhost:8000"                ;Llama.cpp server location
;;   :models '("test"))                    ;Any names, doesn't matter for Llama

;; ;; Execute the following only if emacs is compiled with treesit
;; (when (featurep 'treesit)
;;   (use-package combobulate
;;     :ensure t))


;; Execute the following only if emacs is compiled with treesit
;; (if (featurep 'treesit)
;;    (require 'pkg-combobulate))

;; (native-compile-async "/home/michael/.emacs.d" 'recursively)

;; Remove .elc files on save

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

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
