;(require-package 'solarized-theme)
(require-package 'color-theme-solarized)
;(require 'color-theme-solarized)

;'(custom-enabled-themes (quote (solarized-dark)))




(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
;(set-background-color "black")

;; (lambda (frame) (set-frame-parameter frame 'background-mode dark) t)
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (set-frame-parameter frame
;;                                  'background-mode
;;                                    'dark)
;;             (enable-theme 'solarized)))


;(require-package 'base16-theme)
;(solarized)
;;(require 'solarized-theme)

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (select-frame frame)
;;                 (setq solarized-termcolors 256)
;;                 (set-background-color "black")
;;                 (setq solarized-degrade t)
;;                 (load-theme 'solarized-dark t)))
;;   (setq solarized-termcolors 256)
;;   (set-background-color "black")
;;   (setq solarized-degrade t)
;;   (load-theme 'solarized t))

;(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa/")

;; ;; deamon load without X
;; (if (daemonp)
;; (add-hook 'after-make-frame-functions
;;           '(lambda (f)
;;              (with-selected-frame f
;;                (when (window-system f) (load-theme 'solarized-dark t))
;;              )
;;           (load-theme 'solarized t)
;; ;; can add else...
;; ))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame
                                 'background-mode
                                  'dark)
            (enable-theme 'solarized))
 )
(set-background-color "black")
(load-theme 'solarized)
(provide 'pkg-theme)
