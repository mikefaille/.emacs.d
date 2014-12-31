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



; change window using meta arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings 'meta)


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




(provide 'core-feel)
