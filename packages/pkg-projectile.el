;;; pkg-projectile.el ---

;; Ensure necessary packages are loaded
(require 'projectile)
(require 'go-projectile)
(require 'exec-path-from-shell)
(require 'perspective)

;; Projectile Configuration
(setq projectile-completion-system 'ido
      projectile-switch-project-action 'projectile-dired
      projectile-cache-file (expand-file-name "projectile.cache" main-savefile-dir)
      projectile-project-search-path '("~/src" "/etc/nixos")
      go-projectile-tools-path (concat (exec-path-from-shell-copy-env "HOME") "/go"))

(projectile-mode t)

;; Key bindings for Projectile
(global-set-key (kbd "C-é") 'projectile-commander)
(define-key projectile-mode-map (kbd "C-<tab>") 'projectile-persp-switch-project)

;; Perspective Configuration
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
(persp-mode)

(provide 'pkg-projectile)

;;; pkg-projectile.el ends here
