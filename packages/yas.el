
;; yasnippet - template
(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-global-mode 1)))

;; diminish keeps the modeline tidy
(autoloadp 'diminish)

(provide 'yas)
