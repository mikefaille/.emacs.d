;; -*- lexical-binding: t; -*-
;;; pkg-projectile.el --- Configuration for Projectile and related packages

(require 'use-package)

;; Ensure shell environment is loaded for things like GOPATH
;; Needs to be loaded early if other packages depend on env vars
(use-package exec-path-from-shell
  :ensure t
  :config
  ;; Define vars to import from shell (customize as needed)
  (setq exec-path-from-shell-variables
        '("PATH" "GOPATH" "GOCACHE" "MANPATH")) ; Added GOPATH/GOCACHE
  (exec-path-from-shell-initialize))

;; Configure Projectile (Project Interaction Library)
(use-package projectile
  :ensure t
  :custom
  ;; Use ido for projectile completion (ensure pkg-ido is loaded)
  (projectile-completion-system 'ido)
  ;; Action when switching projects (e.g., open dired, run magit-status)
  (projectile-switch-project-action #'projectile-dired)
  ;; Cache file location (using variable defined in init.el)
  ;; *** Ensure 'michael-savefile-dir' is defined before this file is loaded ***
  (projectile-cache-file (expand-file-name "projectile.cache" michael-savefile-dir))
  ;; Where projectile should look for projects
  (projectile-project-search-path '("~/src")) ; Add other project parent dirs if needed
  :config
  ;; Enable projectile globally
  (projectile-mode 1)
  ;; Integrate projectile with built-in project.el (Emacs 28+)
  (add-hook 'project-find-functions #'project-projectile)
  :bind (("C-é" . projectile-commander))) ; Custom keybinding

;; Configure Go + Projectile integration
(use-package go-projectile
  :ensure t
  :after (projectile exec-path-from-shell) ; Load after projectile & exec-path
  :config
  ;; Set go tools path relative to HOME (fetched from shell env)
  ;; Ensure GOPATH or similar is set correctly in your shell environment
  (setq go-projectile-tools-path (expand-file-name "go" (getenv "HOME"))))

;; Configure Perspective (Workspace Management)
(use-package perspective
  :ensure t
  :custom
  ;; Define a custom prefix key for perspective commands
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  ;; Enable perspective mode globally
  (persp-mode 1)
  ;; Integrate projectile and perspective (optional binding)
  (define-key projectile-mode-map (kbd "C-<tab>") #'projectile-persp-switch-project)
  :bind (("C-x C-b" . persp-list-buffers))) ; Override C-x C-b for perspective buffer list


;; Mark this file as provided
(provide 'pkg-projectile)
;;; pkg-projectile.el ends here
