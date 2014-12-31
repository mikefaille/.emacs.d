(require-package 'yasnippet-bundle)


;; yasnippet - template
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-global-mode 1)))


(provide 'yas)
