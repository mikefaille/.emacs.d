(require-package 'dockerfile-mode)

(require-package 'docker)

(require-package 'docker-tramp)


(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))


(provide 'pkg-docker)
