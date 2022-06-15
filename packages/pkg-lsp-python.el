(require 'lsp-mode)
(require 'lsp-ui)
(require-package 'lsp-pyright)
(require 'lsp-pyright)
;; (add-hook 'python-mode-hook #'lsp-python-enable)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook (lambda ()
                              (require 'lsp-pyright)
                              (lsp)))  ; or lsp-deferre) ; or lsp-deferred


(setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
(setq lsp-pyright-stub-path (concat (getenv "HOME") "/src/python-type-stubs")) ;; example

(provide 'pkg-lsp-python)
