;; projectile is a project management mode

(require-package 'projectile)
(require-package 'go-projectile)


(require 'go-projectile)
(require-package 'exec-path-from-shell)

(setq go-projectile-tools-path (concat (exec-path-from-shell-copy-env "HOME") "/go"))
;(require 'go-projectile-autoloads)


(projectile-global-mode)




(require-package 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" main-savefile-dir))
(projectile-global-mode t)

(provide 'pkg-projectile)
