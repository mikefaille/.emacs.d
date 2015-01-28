;(require-package 'solarized-theme)
(require-package 'color-theme)
(require 'color-theme-solarized)
;(require 'color-theme-solarized)

;'(custom-enabled-themes (quote (solarized-dark)))




;(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
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

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (set-frame-parameter frame
;;                                  'background-mode
;;                                  'dark)
;;             (enable-theme 'solarized))
;;           )

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (set-frame-parameter frame
;;                                  'background-mode
;;                                  'dark))
;;             (enable-theme 'solarized))

;; (setq frame-background-mode "dark")
;; ;(setq solarized-termcolors '256)
;; ;(setq solarized-degrade nil)


;; (load-theme 'solarized)
;; (provide 'pkg-theme)




;(set-background-color "black")

(set-background-color "black")
(add-hook 'after-make-frame-functions
          (setq solarized-termcolors (if window-system '256 '16))
          (lambda (frame)
            (set-frame-parameter frame
                                 'background-mode
                                 'dark)
            (enable-theme 'solarized))
            
          )
(load-theme 'solarized)

;; (unless window-system
;;   (setq solarized-degrade (if window-system nil '16)
;; )


;;             (set-frame-parameter frame
;;                                  'background-mode
;;                                  'dark)

;;             (enable-theme 'solarized))
;;           )


;(setq frame-background-mode "black")

;(setq solarized-termcolors '256)
;(setq solarized-degrade t)

(provide 'pkg-theme)
