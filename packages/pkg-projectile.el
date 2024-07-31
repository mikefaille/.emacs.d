;;; pkg-projectile.el ---

;; Ensure necessary packages are loaded
(require-package 'projectile)
(require-package 'go-projectile)
(require-package 'exec-path-from-shell)
(require-package 'perspective)

;; Projectile Configuration
(setq projectile-completion-system 'ido
      projectile-switch-project-action 'projectile-dired
      projectile-cache-file (expand-file-name "projectile.cache" main-savefile-dir)
      projectile-project-search-path '("~/src")
      go-projectile-tools-path (concat (exec-path-from-shell-copy-env "HOME") "/go"))

(projectile-mode t)

(add-hook 'project-find-functions #'project-projectile)


;; Key bindings for Projectile
(global-set-key (kbd "C-é") 'projectile-commander)
(define-key projectile-mode-map (kbd "C-<tab>") 'projectile-persp-switch-project)

;; Perspective Configuration
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
(persp-mode)

(provide 'pkg-projectile)

;;; pkg-projectile.el ends here
