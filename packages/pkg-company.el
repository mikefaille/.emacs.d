(require-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)

(setq company-idle-delay 0.3)
(provide 'pkg-company)
