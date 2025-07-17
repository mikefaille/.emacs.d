;;; pkg-docker.el --- Docker integration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures Docker integration with docker.el and
;; dockerfile-mode.

;;; Code:

(require-package 'docker)
(use-package docker
  :defer t)

(require-package 'dockerfile-mode)
(use-package dockerfile-mode
  :defer t
  :config
  ;; Associate Dockerfiles with dockerfile-mode.
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  ;; Set the indentation for dockerfile-mode.
  (add-hook 'dockerfile-mode-hook '(lambda()
				   (setq-local sh-basic-offset 2)
				   (setq-local indent-line-function #'sh-indent-line))))

(require-package 'tramp-container)
(use-package tramp-container
  :defer t)

(provide 'pkg-docker)
