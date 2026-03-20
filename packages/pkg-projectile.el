;; -*- lexical-binding: t; -*-
;;; pkg-projectile.el --- Project management

(require 'use-package)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'vertico)
  (projectile-cache-file (expand-file-name "projectile.cache" michael-savefile-dir))
  (projectile-project-search-path '("~/src"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (add-hook 'project-find-functions #'project-projectile))

(provide 'pkg-projectile)
;;; pkg-projectile.el ends here
