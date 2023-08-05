(require 'tramp-container)

(require-package 'dockerfile-mode)

(require-package 'docker)


(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(add-hook 'dockerfile-mode-hook '(lambda()
				   (setq-local sh-basic-offset 2)
				   (setq-local indent-line-function #'sh-indent-line)))

(provide 'pkg-docker)
