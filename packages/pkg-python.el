;; (require-package 'jedi)
(require-package 'elpy)
(require-package 'py-autopep8)
(require-package 'ein)

;; Load python-mode
(add-hook 'python-mode-hook 'elpy)
(when (require 'elpy nil t)
  (elpy-enable))

(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; For elpy
(setq elpy-rpc-python-command "python3")
;; For interactive shell
(setq python-shell-interpreter "python3")
(setq elpy-rpc-backend "jedi")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt")


(setq ein:use-auto-complete-superpack t)
(setq ein:use-smartrep t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(setq python-guess-indent t)

;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)

(setq py-autopep8-options '("--max-line-length=120"))

;; pip install jedi
;; # autoflake8 for code checks
;; pip install autoflake8
;; # and importmagic for automatic imports
;; pip install importmagic
;; https://github.com/jorgenschaefer/elpy

(provide 'pkg-python)
