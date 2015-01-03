(require-package 'jedi)

;; python jedi for completion
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(autoloadp 'ein)
(setq ein:use-auto-complete-superpack t)
(setq ein:use-smartrep t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(provide 'pkg-python)
