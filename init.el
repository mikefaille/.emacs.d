;;; package --- 
;;; Commentary:
;; no welcome screen
;;; code:
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(message "Loading core...")


;; recompile all from prelude
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
  (error "Mike's dot emacs requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

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
(setq load-prefer-newer )

(require 'core-packages)
(require 'core-look)
(require 'core-feel)
(require 'core-org)


(defvar packages-dir
(concat user-emacs-directory
        (convert-standard-filename "packages")) )

(add-to-list 'load-path (expand-file-name packages-dir))




(require 'pkg-ac-complete)
(require 'pkg-go)
(require 'pkg-ssh)
(require 'pkg-gutter)
(require 'pkg-feel)
(require 'pkg-python)
(require 'pkg-latex)
;(require 'main-editor)
;; (require 'pkg-gnome)
(require 'main-custom)
(require 'pkg-docker)
(require 'yaml-mode)
(require 'pkg-php)
(require 'go-autocomplete)

(define-key ac-mode-map (kbd "TAB") 'auto-complete)

(when (not package-archive-contents)
  (package-refresh-contents))






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


;(eval-after-load 'esh-opt
;  (progn
;    (autoloadp 'eshell-prompt-extras)))
;    (setq eshell-highlight-prompt nil
;          eshell-prompt-function 'epe-theme-lambda)))

;(if (not server-mode)
;    (server-start nil t))



;; (autoloadp 'smex) ; Not needed if you use package.el
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                   ; when Smex is auto-initialized on its first 
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
 '(ansi-term-color-vector
   [unspecified "#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(cursor-color nil)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "ec603b4909fb21546c678f360d80f23cd98fb9d9f0b3baf7f50aa58dab8b2fd4" "d196b3cae974856c11b8fa632f6c4dae05cc844be6afc93584c0e07c84dce0ce" "9fbf79005cbc8c22e47f9639d20ddcb6ec130510eaa60008e718d376a586fa9f" "898d95abf1132b31112e02b4596645aca7ff7e10a545492f54c3633de48e071e" "e32da1e92c1e311dcf217f86e2671fbecacb6ca36c0a444d36884b9ecf2cb966" "26a16477cb646d32e0260f2e2013b56c15b08482395afc6b3e1a26ddd69d41d2" "fbdd92f4f41bd5d64fac0484d5d3ad1ad867a2165f64e1df4201e16a518e2c1d" "646604935d0e696c74cd2afc69b5776d2374858fc2e2d79dae3e756a719603ca" "d76783d3453bf2df95b60752feaeb7e86b7c593616997bcdc1c4e4a87571f50c" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(foreground-color nil)
 '(ido-everywhere t)
 '(pdf-latex-command "xelatex"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'pkg-theme)
