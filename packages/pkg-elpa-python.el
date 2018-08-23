(require 'company)                                   ; load company mode


(require-package 'jedi)
(require-package 'elpy)
(require-package 'py-autopep8)
(require-package 'ein)

;; Load python-mode
(add-hook 'python-mode-hook 'elpy-enable)
(when (require 'elpy nil t)
  (elpy-enable))

(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; For elpy
;; (setq elpy-rpc-python-command "python3")
;; For interactive shell
;; (setq python-shell-interpreter "python3")




(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(setq elpy-rpc-backend "jedi")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt"
      elpy-rpc-python-command "python3"
      )

(setq ein:use-auto-complete-superpack t)
(setq ein:use-smartrep t)
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(setq python-guess-indent t)

;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)

(setq py-autopep8-options '("--max-line-length=120"))


(require-package 'lsp-python)
(require 'lsp-python)

(defun emikes-lsp-python ()
  "Multiply NUMBER by seven."
  (lambda

    (lsp-python-enable)
    (lsp-python)
    (lsp-ui-mode)
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (push 'company-lsp company-backends)
    (company-mode)
    (lsp-define-stdio-client lsp-python "python"
                             #'projectile-project-root
                             '("pyls"))

    ))

(add-hook 'python-mode-hook #'emikes-lsp-python)








;; pip install jedi
;; # autoflake8 for code checks
;; pip install autoflake8
;; # and importmagic for automatic imports
;; pip install importmagic
;; https://github.com/jorgenschaefer/elpy

(provide 'pkg-elpa-python)
