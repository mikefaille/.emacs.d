;; Define directories for core and package configurations, and for savefiles
(use-package solarized
 
:demand t

  :config
  (setq custom-safe-themes t)
   (require 'solarized)

	      (when (display-graphic-p)
  (load-theme 'solarized-dark t)
  )
				)

(require 'core-font)
(defconst packages-dir (expand-file-name "packages" user-emacs-directory))
(defconst theme-dir (expand-file-name "theme" user-emacs-directory))
  ;; (load-theme 'solarized-dark t)
(add-to-list 'load-path packages-dir)

;; ;;(add-to-list 'load-path (concat user-emacs-directory "astro-ts-mode"))
;; (;;require 'astro-ts-mode)
;; (setq treesit-language-source-alist
;;       '((astro "https://github.com/virchau13/tree-sitter-astro")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))
;; (mapc #'treesit-install-language-grammar '(astro css tsx))



;; Load  package configurations
;; (require 'pkg-elpaca)

(require 'pkg-ido)
(require 'pkg-corfu3)
(require 'pkg-org)
(require 'pkg-discover)
(require 'pkg-git)
;; (require 'pkg-company)
(require 'pkg-flycheck)
(require 'pkg-ssh)
(require 'pkg-docker)
(require 'pkg-feel)
(require 'pkg-lsp2)
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
;; (require 'pkg-straight)

(require 'pkg-flutter)
(require 'pkg-terraform)
;; (require 'pkg-corfu)



;; ;; (require 'pkg-combobulate)
;; (add-to-list 'custom-theme-load-path theme-dir)
;; (require 'pkg-theme)
;; (load-theme 'my-solarized-dark t)

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f)
                 (load-theme 'solarized-dark t)))))
;; ;; Theme management
;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (if (daemonp)
;;       (add-hook 'after-make-frame-functions
;;                 '(lambda (f)
;;                    (with-selected-frame f
;;                      (load-theme 'solarized-dark t))))
;;     (load-theme 'solarized-dark t)))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (if (daemonp)
;;       (add-hook 'after-make-frame-functions
;;                 '(lambda (f)
;;                    (with-selected-frame f
;;                      (when (window-system f) (load-theme 'solarized-dark t)))))
;;     (load-theme 'solarized-dark t)))





;; (use-package 'gptel)

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
  ;; (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (when (file-exists-p (concat buffer-file-name "c"))
                            (delete-file (concat buffer-file-name "c"))))
                        nil t)))
  (add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode)))



(use-package tree-sitter

:demand t

:config

	(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-ts-mode . typescript)

							 )
  
  ;; Correct way to add the highlighting hook
  (add-hook 'prog-mode-hook 'tree-sitter-hl-mode)

	)



(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; Enable highlighting in programming modes that support tree-sitter
(use-package tree-sitter-hl
	:ensure nil



	)


;; Enable highlighting in programming modes that support tree-sitter
(use-package tree-sitter-indent
	:ensure nil
	:after tree-sitter
	;; :hook prog-mode
	)


  

(use-package treesit-auto
	;; :after tree-sitter
  :custom
  (treesit-auto-install 'prompt)
	:hook tree-sitter-hl-mode


:init
    (setq treesit-auto-exclude-modes '(fundamental-mode))
  (let ((ts-dir (expand-file-name "tree-sitter" user-emacs-directory)))
    (unless (file-directory-p ts-dir)
      (make-directory ts-dir t))
    (setq treesit-extra-load-path (list ts-dir)))


  :config

  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
	)



(use-package indent-bars
	:ensure t
	:hook (prog-mode)
	)

