;; projectile is a project management mode

(require-package 'projectile)
(require-package 'go-projectile)

(require 'go-projectile)
;(require 'go-projectile-autoloads)


(projectile-global-mode)




(require-package 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" main-savefile-dir))
(projectile-global-mode t)

(provide 'pkg-projectile)
