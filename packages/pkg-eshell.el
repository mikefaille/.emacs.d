(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (add-hook 'eshell-preoutput-filter-functions
;;           'ansi-color-filter-apply)

;; (add-hook 'eshell-preoutput-filter-functions
;;           'ansi-color-apply)

;;  (ansi-color-for-comint-mode-on)
;;   (defun eshell-handle-ansi-color ()
;;     (ansi-color-apply-on-region eshell-last-output-start                                 eshell-last-output-end))
;;   (add-hook 'eshell-mode-hook
;;                   '(lambda ()
;;                                              (add-to-list
;;                                                                  'eshell-output-filter-functions
;;  'eshell-handle-ansi-color)))


;;                                         (eval-after-load 'esh-opt
;;                                          (progn
;;                                            (autoloadp 'eshell-prompt-extras)))
;;                                            (setq eshell-highlight-prompt nil
;;                                                  eshell-prompt-function 'epe-theme-lambda)


(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))






(defun eshell-toggle ()
  (if (bound-and-true-p 'shell-mode)
       (eshell-here)
    (eshell/x)
    )

  )


(global-set-key (kbd "C-!") 'eshell-toggle)



(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq password-cache t) ; enable password caching
(setq password-cache-expiry 3600) ; for one hour (time in secs)
(setq eshell-prefer-lisp-functions t)

(provide 'pkg-eshell)
