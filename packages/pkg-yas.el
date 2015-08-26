(require-package 'yasnippet)


;; yasnippet - template
;(add-hook 'prog-mode-hook
;          '(lambda ()
(yas-global-mode 1)
;))


(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)




(provide 'pkg-yas)
