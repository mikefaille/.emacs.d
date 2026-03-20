;;; pkg-haskell.el --- Haskell Configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package haskell-mode
  :ensure t
  :hook (haskell-mode . lsp-deferred)
  :config
  ;; Ensure lsp-haskell is installed/configured if needed by lsp-mode
  ;; lsp-mode usually handles this automatically or via lsp-haskell package
  )

;; (use-package lsp-haskell
;;   :ensure t
;;   :after (haskell-mode lsp-mode))

(provide 'pkg-haskell)
;;; pkg-haskell.el ends here
