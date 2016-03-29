;;; Package ---
;;; Commentary:
;; no welcome screen
;;; code:

(add-to-list 'load-path "~/.emacs.d/git-packages/benchmark-init-el/")
(require 'benchmark-init)
(benchmark-init/activate)

(package-initialize)


(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
;; (if (not server-mode)
;;     (server-start nil t))
(ansi-color-for-comint-mode-on)
(message "Loading core...")
(add-to-list 'default-frame-alist '(background-mode . dark))

;; recompile all from prelude
(defun recompile-init ()
"Byte-compile all your dotfiles again."
(interactive)

(byte-recompile-directory user-emacs-directory 0))

(global-set-key (kbd "C-c C-1") 'recompile-init)


(setq debug-on-error t)

(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Mike's .emac.d is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.1")
  (error "Mike's dot emacs requires at least GNU Emacs 24.4, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar core-dir
  (concat user-emacs-directory
          (convert-standard-filename "core")) )


(defvar main-savefile-dir
  (concat user-emacs-directory
          (convert-standard-filename "savefile")) )


;; add core's directories to Emacs's `load-path'
(add-to-list 'load-path (expand-file-name core-dir))


(unless (file-exists-p main-savefile-dir)
(make-directory main-savefile-dir))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;; replace current selection by yank or type
(delete-selection-mode 1)

;(defvar main-savefile-dir
;(concat user-emacs-directory
;        (convert-standard-filename "savefile")) )


;; Always load newest byte code
(setq load-prefer-newer t)

(require 'eieio)
(require 'core-packages)
(require 'core-look)
(require 'core-feel)




(defvar packages-dir
  (concat user-emacs-directory
          (convert-standard-filename "packages")) )
(add-to-list 'load-path (expand-file-name packages-dir))

(require 'main-custom)


(require 'pkg-org)

;; I recommend to load yas before ac-complete
(require 'pkg-git)
(require 'pkg-yas)
(require 'pkg-ac-complete)
(require 'pkg-flycheck)


(require 'pkg-ssh)

(require 'pkg-docker)
(require 'pkg-feel)

(require 'pkg-python)
(require 'pkg-latex)
;;(require 'main-editor)
;;(require 'pkg-gnome)

(require 'pkg-yaml)
(require 'pkg-php)

;;(require 'pkg-projectile)
(require 'pkg-bash)
(require 'pkg-search)
(require 'pkg-eshell)
(require 'pkg-rust)
(require 'pkg-emms)

(require 'pkg-web)
(require 'pkg-systemd)
;; (require 'pkg-mu4e)


;; depend on pkg-ac-complete
(require 'pkg-js)
(require 'pkg-go)
(require 'go-autocomplete)

(require 'pkg-irc)
(require 'pkg-helm)

;; http://emacsredux.com/blog/2013/05/16/whitespace-cleanup/
(add-hook 'before-save-hook 'whitespace-cleanup)


;; top like
;; (global-set-key (kbd "C-x p") 'proced)


(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))



(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)


(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))


;; auto byte compile
(require 'auto-async-byte-compile)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; ;(require 'color-theme-solarized)l
;; ;; Color theme
;; (add-to-list 'load-path "~/.emacs.d/themes/solarized/")
;; (require 'color-theme)
;; ;(require 'color-theme-solarized-dark)
;; (eval-after-load "color-theme"
;;   '(progn
;; (color-theme-initialize)
;; (color-theme-solarized-dark)))
;; (setq color-theme-is-global t)

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")




                                        ;(require 'yasnippet)
(require 'undo-tree)

(when (fboundp 'winner-mode)
  (winner-mode 1))





;; java
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)





                                        ;(autoloadp 'sublimity)
                                        ; (autoloadp 'sublimity-scroll)
                                        ; (autoloadp 'sublimity-map)
                                        ; (autoloadp 'sublimity-attractive)
                                        ; (autoloadp 'minimap-autoloads)
                                        ;(setq sublimity-attractive-centering-width nil)
                                        ;(sublimity-mode 1)




                                        ;(autoloadp 'malabar-mode)
                                        ;(setq malabar-groovy-lib-dir "/path/to/malabar/lib")
                                        ;(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;;compile on save
                                        ;(add-hook 'malabar-mode-hook
                                        ;     (lambda ()
                                        ;      (add-hook 'after-save-hook 'malabar-compile-file-silently
                                        ;                  nil t)))
;; Auto-populate an empty java file
                                        ;    (add-hook 'malabar-mode-hook
                                        ;          '(lambda ()
                                        ;             (when (= 0 (buffer-size))
                                        ;               (malabar-codegen-insert-class-template))))



;; (add-hook 'after-init-hook (lambda ()
;;                              (message "activate-malabar-mode")
;;                              (activate-malabar-mode)))

;; (add-hook 'malabar-java-mode-hook 'flycheck-mode)
;; (add-hook 'malabar-groovy-mode-hook 'flycheck-mode)

;; completion framework
                                        ;(add-hook 'after-init-hook 'global-company-mode)
                                        ;(autoloadp 'company)                                   ; load company mode
                                        ;(autoloadp 'company-go)                                ; load company mode go backend
                                        ;(setq company-tooltip-limit 20)                      ; bigger popup window
;;(setq company-idle-delay .3)                         ; decrease delay before autocomp;letion popup shows
                                        ;(setq company-echo-delay 0)                          ; remove annoying blinking
                                        ;(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;; flycheck. for fuzzy check, install fuzzy...
                                        ;(autoloadp 'flycheck)
                                        ;(flycheck-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)






                                        ;(autoloadp 'linum-relative)

                                        ;(autoloadp 'eclim)
                                        ;(global-eclim-mode)
                                        ;(autoloadp 'eclimd)
                                        ;(custom-set-variables
                                        ;  '(eclim-eclipse-dirs '("~/app/eclipse"))
                                        ;(autoloadp 'company)
                                        ;(autoloadp 'company-emacs-eclim)
                                        ;(company-emacs-eclim-setup)
                                        ;(global-company-mode t)





;; (autoloadp 'smex) ; Not needed if you use package.el
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                   ; when Smex is auto-initialized on its first
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(require 'pkg-theme)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ensime sbt-mode web-mode volatile-highlights undo-tree tern-auto-complete systemd symon ssh-config-mode solarized-theme smex smartrep smartparens scpaste salt-mode rust-mode pt php-mode operate-on-number multiple-cursors mu4e-maildirs-extension minimap markdown-mode malabar-mode magit latex-extra json-mode js3-mode jedi ido-ubiquitous idle-highlight-mode helm golint go-scratch go-errcheck go-eldoc go-dlv go-autocomplete gitconfig-mode git-gutter fuzzy frame-fns flycheck-rust flx-isearch flx-ido find-file-in-project expand-region exec-path-from-shell eshell-prompt-extras erc-track-score erc-social-graph erc-image erc-hl-nicks emmet-mode emacs-eclim easy-kill dockerfile-mode docker-tramp docker dired-hacks-utils diminish css-eldoc cdlatex browse-kill-ring better-defaults bash-completion auto-complete-auctex auto-async-byte-compile auctex-latexmk anzu ac-capf))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
