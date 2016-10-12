(require-package 'jedi)



;; (require-package 'elpy)
;; (package-initialize)
;; (elpy-enable)

;;python jedi for completion
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(autoloadp 'ein)
(setq ein:use-auto-complete-superpack t)
(setq ein:use-smartrep t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)


;; pip install jedi
;; # flake8 for code checks
;; pip install flake8
;; # and importmagic for automatic imports
;; pip install importmagic
;; https://github.com/jorgenschaefer/elpy

(require-package 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=120"))


(provide 'pkg-python)
