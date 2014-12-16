;; no welcome screen
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; replace current selection by yank or type
(delete-selection-mode 1)


(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))




(defvar my-packages '(better-defaults smartparens idle-highlight-mode ido-ubiquitous find-file-in-project magit smex scpaste color-theme-solarized helm flycheck undo-tree dired-hacks-utils  yasnippet flycheck malabar-mode company company-go multiple-cursors go-mode go-autocomplete auto-complete emacs-eclim eshell-prompt-extras projectile fuzzy cl-lib deferred jedi ein ) "A list of packages to ensure are installed at launch.")




(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'undo-tree)


;; auto-complete
(require 'auto-complete)
(auto-complete-mode t)
(ac-set-trigger-key "TAB")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)
(setq-default
 ac-sources
 '(
   ac-source-yasnippet
   ac-source-imenu
   ac-source-abbrev
   ac-source-words-in-same-mode-buffers
   ac-source-files-in-current-dir
   ac-source-filename
   )
)


;; python jedi for completion
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(require 'ein)
(setq ein:use-auto-complete-superpack t)
(setq ein:use-smartrep t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;;;; go config
;; install godef 
; go get code.google.com/p/rog-go/exp/cmd/godef
; go get github.com/golang/lint/golint
; require package : auto-complete
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(require 'go-autocomplete)
  (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
  (require 'golint)
(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)



(setenv "PATH"
  ( concat 
    "/usr/local/go/bin" ":"
    (getenv "HOME") "/go/bin" ":"

    (getenv "PATH")
  )
)

(setenv "GOPATH"
  ( concat 
    (getenv "HOME") "/go" ":"

    (getenv "GOPATH")
  )
)

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/nsf/gocode"))


;; load git package
(let ((base "~/.emacs.d/git-packages"))
 ; (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name f))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (normal-top-level-add-subdirs-to-load-path)))))



;; load plugins
;(let ((default-directory load-path))
;  (normal-top-level-add-subdirs-to-load-path))

(require 'smartparens-config)
(smartparens-global-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; performance requirement for helm
(when (require 'dired-aux)
  (require 'dired-async))




;; init helm - a completion tool
;;(require 'helm)
;(require 'helm-config)
;(helm-mode)


;(require 'color-theme-solarized)
;; Color theme
(add-to-list 'load-path "~/.emacs.d/themes/solarized/")
(require 'color-theme)
;(require 'color-theme-solarized-dark)
(eval-after-load "color-theme"
  '(progn
(color-theme-initialize)
(color-theme-solarized-dark)))
(setq color-theme-is-global t)

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
             (yas-minor-mode)))




;; java
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)


(require 'minimap)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (python . nil)
   (plantuml . nil)
   ))

(global-git-gutter-mode +1)
(require 'git-gutter)

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



;(require 'sublimity)
; (require 'sublimity-scroll)
; (require 'sublimity-map)
; (require 'sublimity-attractive)
; (require 'minimap-autoloads)
;(setq sublimity-attractive-centering-width nil)
;(sublimity-mode 1)


;(require 'malabar-mode)
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



(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;windmove-default-keybindings 'meta)

;; completion framework
;(add-hook 'after-init-hook 'global-company-mode)
;(require 'company)                                   ; load company mode
;(require 'company-go)                                ; load company mode go backend
;(setq company-tooltip-limit 20)                      ; bigger popup window
;;(setq company-idle-delay .3)                         ; decrease delay before autocomp;letion popup shows
;(setq company-echo-delay 0)                          ; remove annoying blinking
;(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;; flycheck. for fuzzy check, install fuzzy...
;(require 'flycheck)
;(flycheck-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; mouse integration
(require 'mouse) ;; needed for iterm2 compatibility
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda ()
                           (interactive)
                           (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                           (interactive)
                           (scroll-up 1)))
(global-set-key [mouse-6] '(lambda ()
                           (interactive)
                           (scroll-right 1)))
(global-set-key [double-mouse-6] '(lambda ()
                           (interactive)
                           (scroll-right 1)))
(global-set-key [triple-mouse-6] '(lambda ()
                           (interactive)
                           (scroll-right 1)))
(global-set-key [mouse-7] '(lambda ()
                           (interactive)
                           (scroll-left 1)))
(global-set-key [double-mouse-7] '(lambda ()
                           (interactive)
                           (scroll-left 1)))
(global-set-key [triple-mouse-7] '(lambda ()
                           (interactive)
                           (scroll-left 1)))
(setq mouse-sel-mode t)
(defun track-mouse (e))
;; disable bell function
(setq ring-bell-function 'ignore)



;; emacs gnomeshell integration
(when (daemonp)
  (defadvice desktop-restore-file-buffer
    (around my-desktop-restore-file-buffer-advice)
    "Be non-interactive while starting a daemon."
    (let ((noninteractive t))
      ad-do-it))
  (ad-activate 'desktop-restore-file-buffer)
  (setq desktop-dirname             "~/.emacs.d/desktop/"
        desktop-base-file-name      (concat (daemonp) ".desktop")
        desktop-base-lock-name      (concat (daemonp) ".lock")
        desktop-path                (list desktop-dirname)
        desktop-save                t
        desktop-files-not-to-save   "^$" ;reload tramp paths
        desktop-load-locked-desktop t)
  (desktop-save-mode 1))


;; Tweaks for tmux's support for Ctrl+arrows
;; http://marc-abramowitz.com/archives/2006/10/05/ctrl-left-and-ctrl-right-in-bash-and-emacs/
(global-set-key "\M-[1;5C"    'forward-word)      ; Ctrl+right   => forward word
(global-set-key "\M-[1;5D"    'backward-word)     ; Ctrl+left    => backward word

;
; Cycle through windows backwards with C-x p
(defun prev-window ()
  (interactive)
  (other-window -1))

(define-key global-map (kbd "C-x p") 'prev-window)



;(require 'linum-relative)

;(require 'eclim)
;(global-eclim-mode)
;(require 'eclimd)
;(custom-set-variables
;  '(eclim-eclipse-dirs '("~/app/eclipse"))
;(require 'company)
;(require 'company-emacs-eclim)
;(company-emacs-eclim-setup)
;(global-company-mode t)


;(eval-after-load 'esh-opt
;  (progn
;    (require 'eshell-prompt-extras)))
;    (setq eshell-highlight-prompt nil
;          eshell-prompt-function 'epe-theme-lambda)))

;(if (not server-mode)
;    (server-start nil t))



(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

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
 '(ac-trigger-key "TAB")
 '(ac-use-comphist t)
 '(ac-use-fuzzy t)
 '(custom-safe-themes (quote ("64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
