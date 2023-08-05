(require-package 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")
       ))

(add-hook 'web-mode-hook
          (lambda ()
            (whitespace-mode -1)
            (with-eval-after-load 'sgml-mode
              (require-package 'emmet-mode)
              (emmet-mode 1))
            (with-eval-after-load 'css-mode
              (require-package 'emmet-mode)
              (emmet-mode 1))
            (with-eval-after-load 'web-mode
              (require-package 'css-eldoc))
            ))

(provide 'pkg-web)
