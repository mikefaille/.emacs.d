;;; Package ---
;;; Commentary:
;; no welcome screen
;;; code:

;; (add-to-list 'load-path "~/.emacs.d/git-packages/benchmark-init-el/")
;; (require 'benchmark-init)
;; (benchmark-init/activate)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

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

(when (version< emacs-version "24.4")
  (error "Mike's dot emacs requires at least GNU Emacs 24.4, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar core-dir
  (convert-standard-filename(concat user-emacs-directory
          "core")) )


(defvar main-savefile-dir
  (convert-standard-filename (concat user-emacs-directory
           "savefile") ))


;; add core's directories to Emacs's `load-path'
(add-to-list 'load-path (expand-file-name core-dir))


(unless (file-exists-p main-savefile-dir)
(make-directory main-savefile-dir))

;; INCREASE GC
;; Increasing GC is a common way to speed up Emacs. gc-cons-threshold sets at what point Emacs should invoke its garbage collector Some people set it to a really larger number permanently. This works well until the garbage is actually collected (then you have to wait a long time). I’ve decided to just set it temporarily to a large number so we only garbage collect once on startup. After that we reset it to the standard value. Read @bling’s post for more info on this.
;; source https://matthewbauer.us/bauer/
(defvar file-name-handler-alist-backup
  file-name-handler-alist)
(setq gc-cons-threshold
      most-positive-fixnum
      file-name-handler-alist nil)
(add-hook 'after-init-hook
	  (lambda ()
	    (garbage-collect)
	    (setq gc-cons-threshold
		  (car (get 'gc-cons-threshold 'standard-value))
		  file-name-handler-alist
		  (append
		   file-name-handler-alist-backup
		   file-name-handler-alist))))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


(setenv "PATH"
        ( concat
          (getenv "HOME")
          "/bin" ":"
          (getenv "PATH")))

;; replace current selection by yank or type
(delete-selection-mode 1)

;(defvar main-savefile-dir
;(concat user-emacs-directory
;        (convert-standard-filename "savefile")) )


(require 'eieio)
(require 'core-packages)
(require 'core-look)
(require 'core-feel)
(require 'core-util)



(defvar packages-dir
  (concat user-emacs-directory
          (convert-standard-filename "packages")) )
(add-to-list 'load-path (expand-file-name packages-dir))

(require 'main-custom)



(require 'pkg-org)
(require 'pkg-discover)
;;  recommend to load yas before ac-complete
(require 'pkg-git)
;; (require 'pkg-yas)
;; (require 'pkg-ac-complete)
(require 'pkg-company)
(require 'pkg-flycheck)

(require 'pkg-ssh)

(require 'pkg-docker)
(require 'pkg-feel)

;; (require 'pkg-elpa-python)
;; (require 'pkg-lsp-python)
(require 'pkg-eglot-python)

(require 'pkg-latex)
;;(require 'main-editor)
;;(require 'pkg-gnome)

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
;; (require 'pkg-mu4e)

;; depend on pkg-ac-complete
(require 'pkg-js)
(require 'pkg-go)

(require 'pkg-irc)
;; (require 'pkg-helm)
(require 'pkg-ido)
;; (require 'pkg-chrome)

;; (require 'nix-mode)
(require 'pkg-multipleCursor)
(require 'pkg-mouvement)

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
(require 'undo-tree)

(when (fboundp 'winner-mode)
  (winner-mode 1))


;; java
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


(require 'pkg-theme)
