;; projectile is a project management mode

(require-package 'projectile)
(require-package 'go-projectile)
(projectile-mode t)

(require-package 'exec-path-from-shell)

(setq go-projectile-tools-path (concat (exec-path-from-shell-copy-env "HOME") "/go"))
;(require 'go-projectile-autoloads)


(setq projectile-completion-system 'ido)

;; (setq projectile-keymap-prefix (kbd "C-c C-p"))
(global-set-key (kbd "C-é") 'projectile-commander)
(setq projectile-switch-project-action 'projectile-dired)

(require-package 'perspective)
(require-package 'persp-projectile)
(persp-mode)

;; (define-key projectile-mode-map (kbd "s-x") 'projectile-persp-switch-project)

;; (define-key projectile-mode-map (kbd "C-<tab>") 'persp-next)
;; (define-key projectile-mode-map (kbd "<C-iso-lefttab>") 'persp-prev)



(setq projectile-cache-file (expand-file-name "projectile.cache" main-savefile-dir))


(setq projectile-project-search-path '("~/src" "/etc/nixos"))
(define-key projectile-mode-map (kbd "C-<tab>") 'projectile-persp-switch-project)

(provide 'pkg-projectile)
