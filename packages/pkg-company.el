(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.3)
(provide 'pkg-company)
