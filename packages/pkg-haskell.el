(require-package 'lsp-ui)
(require-package 'lsp-haskell)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
(add-hook 'haskell-mode-hook 'flycheck-mode)


(provide 'pkg-haskell)
