;;; pkg-yaml.el --- YAML configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures YAML support with yaml-mode, indent-tools,
;; and flycheck-yamllint.

;;; Code:

(require-package 'yaml-mode)
(use-package yaml-mode
  :defer t
  :mode ("\\.yml$" . yaml-mode)
  :mode ("\\.yaml$" . yaml-mode)
  :config
  (require-package 'indent-tools)
  (use-package indent-tools
    :defer t)
  (require-package 'flycheck-yamllint)
  (use-package flycheck-yamllint
    :defer t)
  (add-hook 'yaml-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c >") 'indent-tools-hydra/body)
              (eval-after-load 'flycheck
                '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup)))))

(provide 'pkg-yaml)
