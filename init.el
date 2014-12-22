;;; package --- 
;;; Commentary:
;; no welcome screen
;;; code:
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(message "Loading core...")
(defun prelude-recompile-init ()
"Byte-compile all your dotfiles again."
(interactive)

(byte-recompile-directory user-emacs-directory 0))

(global-set-key (kbd "C-c C-1") 'prelude-recompile-init)



(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Mike's .emac.d is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar core-dir
(concat user-emacs-directory
        (convert-standard-filename "core")) )


;; add core's directories to Emacs's `load-path'
(add-to-list 'load-path (expand-file-name core-dir))


;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;; replace current selection by yank or type
(delete-selection-mode 1)

;; Always load newest byte code
(setq load-prefer-newer )

    
(require 'core-look)
(require 'core-feel)
(require 'core-packages)



(defvar packages-dir
(concat user-emacs-directory
        (convert-standard-filename "packages")) )

(add-to-list 'load-path (expand-file-name packages-dir))


(require 'pkg-theme)
(require 'pkg-ac-complete)
(require 'pkg-go)

(when (not package-archive-contents)
  (package-refresh-contents))





(require 'git-gutter)



;; auto byte compile
(require 'auto-async-byte-compile)
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; ;(require 'color-theme-solarized)
;; ;; Color theme
;; (add-to-list 'load-path "~/.emacs.d/themes/solarized/")
;; (require 'color-theme)
;; ;(require 'color-theme-solarized-dark)
;; (eval-after-load "color-theme"
;;   '(progn
;; (color-theme-initialize)
;; (color-theme-solarized-dark)))
;; (setq color-theme-is-global t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")




;(require 'yasnippet)
(require 'undo-tree)




;; ;; auto-complete
;; (require 'auto-complete)
;; (auto-complete-mode t)
;; (ac-set-trigger-key "TAB")
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;; (autoloadp 'auto-complete-config)
;; (ac-config-default)
;; (setq-default
;;  ac-sources
;;  '(
;;    ac-source-yasnippet
;;    ac-source-imenu
;;    ac-source-abbrev
;;    ac-source-words-in-same-mode-buffers
;;    ac-source-files-in-current-dir
;;    ac-source-filename
;;    )
;; )


;; ;; python jedi for completion
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (setq jedi:complete-on-dot t)
;; (autoloadp 'ein)
;; (setq ein:use-auto-complete-superpack t)
;; (setq ein:use-smartrep t)
;; (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)





;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install)))

;; load plugins
;(let ((default-directory load-path))
;  (normal-top-level-add-subdirs-to-load-path))

(autoloadp 'smartparens-config)
(smartparens-global-mode)

(autoloadp 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; performance autoloadpment for helm
(when (autoloadp 'dired-aux)
  (autoloadp 'dired-async))




;; init helm - a completion tool
;;(autoloadp 'helm)
;(autoloadp 'helm-config)
;(helm-mode)


;;  Interactively do things is a very convenient way to find files and switch buffers.
;ido-mode)
;ido-everywhere 1)


;; relative numbering
;;(linum-on)

;; Windows configs undo/redo
(when (fboundp 'winner-mode)
  (winner-mode 1))


;; yasnippet - template
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-global-mode 1)))

;; diminish keeps the modeline tidy
(autoloadp 'diminish)


;; java
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)


(autoloadp 'minimap)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . nil)
   (plantuml . nil)
   ))

(global-git-gutter-mode +1)
(autoloadp 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

;; If you enable git-gutter-mode for some modes
(add-hook 'ruby-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)



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




;; ;; emacs gnomeshell integration
;; (when (daemonp)
;;   (defadvice desktop-restore-file-buffer
;;     (around my-desktop-restore-file-buffer-advice)
;;     "Be non-interactive while starting a daemon."
;;     (let ((noninteractive t))
;;       ad-do-it))
;;   (ad-activate 'desktop-restore-file-buffer)
;;   (setq desktop-dirname             "~/.emacs.d/desktop/"
;;         desktop-base-file-name      (concat (daemonp) ".desktop")
;;         desktop-base-lock-name      (concat (daemonp) ".lock")
;;         desktop-path                (list desktop-dirname)
;;         desktop-save                t
;;         desktop-files-not-to-save   "^$" ;reload tramp paths
;;         desktop-load-locked-desktop t)
;;   (desktop-save-mode 1))


;
; Cycle through windows backwards with C-x p
(defun prev-window ()
  (interactive)
  (other-window -1))

(define-key global-map (kbd "C-x p") 'prev-window)



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


;(eval-after-load 'esh-opt
;  (progn
;    (autoloadp 'eshell-prompt-extras)))
;    (setq eshell-highlight-prompt nil
;          eshell-prompt-function 'epe-theme-lambda)))

;(if (not server-mode)
;    (server-start nil t))



;; (autoloadp 'smex) ; Not needed if you use package.el
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                   ; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.6)

 '(ac-use-comphist t)
 '(ac-use-fuzzy t)
 '(ansi-color-names-vector
   ["#262626" "#d70000" "#5f8700" "#af8700" "#0087ff" "#af005f" "#00afaf" "#626262"])
 '(background-color nil)
 '(background-mode dark)
 '(cursor-color nil)
 '(custom-safe-themes
   (quote
    ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(foreground-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


