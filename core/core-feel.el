;; mouse integration
(require 'mouse) ;; needed for iterm2 compatibility


;; (ido-mode 1)
;; (ido-everywhere 1)

;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces t)
;; https://github.com/lewang/flx


;; ;(ido-mode 1)
;; (setq ido-use-filename-at-point 'guess)
;; (setq ido-create-new-buffer 'always)
;; (require 'ido)
;; ;(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
;; (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
;;                            "*Messages*" "Async Shell Command"))

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



; change window using meta arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings 'meta)

(require 'electric)
(add-hook 'prog-mode 'electric-pair-local-mode)


;; Tweaks for tmux's support for Ctrl+arrows
;; http://marc-abramowitz.com/archives/2006/10/05/ctrl-left-and-ctrl-right-in-bash-and-emacs/
(global-set-key "\M-[1;5C"    'forward-word)      ; Ctrl+right   => forward word
(global-set-key "\M-[1;5D"    'backward-word)     ; Ctrl+left    => backward word



;; Revert buffers when the underlying file has changed
;; One thing that can be annoying about Emacs when you first start using it is that it doesn't automatically refresh file buffers when the file on disk has been changed outside of Emacs. This can often happen when you're using tools that generate some kind of text file output that you need to read in an Emacs buffer.
;; The global-auto-revert-mode will make Emacs watch the files for all open buffers for changes on disk and it will automatically refresh those buffers if they don't have unsaved changes!
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)


;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" main-savefile-dir))

;; activate it for all buffers
(setq-default save-place t)

;; required for lsp performance
(setq read-process-output-max (* 1024 1024))

(provide 'core-feel)
