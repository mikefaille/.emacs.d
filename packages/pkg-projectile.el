;;; pkg-projectile.el ---

;; Ensure necessary packages are installed
(require-package 'projectile)
(require-package 'go-projectile)
(require-package 'exec-path-from-shell)
(require-package 'perspective)
(require-package 'persp-projectile)

;; Projectile Configuration
(projectile-mode t)
(setq go-projectile-tools-path (concat (exec-path-from-shell-copy-env "HOME") "/go"))
(setq projectile-completion-system 'ido)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-cache-file (expand-file-name "projectile.cache" main-savefile-dir))
(setq projectile-project-search-path '("~/src" "/etc/nixos"))

;; Key bindings for Projectile
(global-set-key (kbd "C-é") 'projectile-commander)
(define-key projectile-mode-map (kbd "C-<tab>") 'projectile-persp-switch-project)

;; Perspective Configuration
(setq persp-mode-prefix-key "C-c p")  ; Set persp-mode-prefix-key
(persp-mode)

(provide 'pkg-projectile)

;;; pkg-projectile.el ends here
