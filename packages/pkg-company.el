(require-package 'company)

(with-eval-after-load 'company
  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common))

(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0.3)
(setq company-minimum-prefix-length 2)
(setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))
(setq company-backends '((company-foo-backend company-bar-backend company-capf company-files)))

(provide 'pkg-company)
