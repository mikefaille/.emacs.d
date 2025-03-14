(setq package-enable-at-startup nil)

(setq-default
 tab-width 2
 js-indent-level 2
 typescript-indent-level 2
 emacs-lisp-indent-offset 2)

(defvar current-user (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(message "Mike's .emac.d is powering up... Be patient, Master %s!" current-user)
(when (version< emacs-version "24.4")
  (error "Mike's dot emacs requires at least GNU Emacs 24.4, but you're running %s" emacs-version))
(setq-default
 make-backup-files nil
 auto-save-default nil
 inhibit-startup-message t
 inhibit-splash-screen t

 load-prefer-newer t
 ring-bell-function 'ignore
 large-file-warning-threshold 100000000
 read-process-output-max (* 1024 1024) ; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
 native-comp-async-report-warnings-errors nil)

; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setenv "LSP_USE_PLISTS" "true")

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(use-package emacs
  :custom
  (display-line-numbers-type 'relative)  ; Optional: for relative numbers
  ;; (display-line-numbers-width 3)         ; Optional: adjust width
  :hook
  (prog-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)) ; Optional: for text modes too

;; BUG
(setq outline-minor-mode-prefix "\C-c \C-o")

;; Optimizations for faster startup
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 2000000000 ;; 200mb
		  file-name-handler-alist file-name-handler-alist-original)))

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


(setq package-enable-at-startup nil)


(defconst main-savefile-dir (expand-file-name "savefile" user-emacs-directory))
;; Ensure savefile directory exists
(unless (file-exists-p main-savefile-dir)
  (make-directory main-savefile-dir))


(defconst core-dir (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path core-dir)


(require 'core-feel)
(require 'core-util)
(require 'core-native-comp)
(require 'core-look)
(require 'core-packages)


;; Load a nice theme if in GUI
