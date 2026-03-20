;;; pkg-workspaces.el --- Workspace Management (Perspective) -*- lexical-binding: t; -*-

(require 'use-package)

;; --- Perspective (Workspaces) ---
(use-package perspective
  :ensure t
  :bind (("C-x C-b" . persp-list-buffers)         ; Switch to perspective buffer list
         ("C-x b" . persp-switch-to-buffer*)      ; Switch buffer within perspective
         ("C-x k" . persp-kill-buffer*))          ; Kill buffer within perspective
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))           ; Prefix for perspective commands
  :init
  (persp-mode))

;; --- Perspective + Projectile Integration ---
(use-package persp-projectile
  :ensure t
  :after (perspective projectile)
  :bind (:map projectile-mode-map
              ("C-c p p" . projectile-persp-switch-project)))

(provide 'pkg-workspaces)
;;; pkg-workspaces.el ends here
