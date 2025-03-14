(use-package lsp-mode
  :ensure t
  :hook ((terraform-mode . lsp-deferred)))


(provide 'pkg-terraform)
