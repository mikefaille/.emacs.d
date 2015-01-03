
;; emacs gnomeshell integration
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
(provide 'pkg-gnome)
