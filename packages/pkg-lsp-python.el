(require 'lsp-mode)
(require 'lsp-ui)
(require-package 'lsp-python)
(require 'lsp-python)
(add-hook 'python-mode-hook #'lsp-python-enable)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

(provide 'pkg-lsp-python)
