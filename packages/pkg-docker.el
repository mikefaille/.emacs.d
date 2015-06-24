(require-package 'dockerfile-mode)
(require-package 'docker)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


(provide 'pkg-docker)
