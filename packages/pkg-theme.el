(require-package 'color-theme-solarized)
(require 'color-theme)
(color-theme-initialize)

;(require-package 'base16-theme)
;;(solarized-dark)
;;(require 'solarized-theme)
;(load-theme 'solarized-dark t)


;; deamon load without X
(if (daemonp)
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f) (load-theme 'solarized-dark t))))
         (color-theme-tty-dark) )
;; can add else...
)

(provide 'pkg-theme)
