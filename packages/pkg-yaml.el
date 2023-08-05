(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(add-hook 'yaml-mode-hook
	  (lambda ()
	    (require-package 'indent-tools)
	    (require-package 'flycheck-yamllint)
	    (local-set-key (kbd "C-c >") 'indent-tools-hydra/body)
	    (eval-after-load 'flycheck
	      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))
	    ))

(provide 'pkg-yaml)
