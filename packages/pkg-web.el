;;; pkg-web.el --- Web mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures web-mode for editing web templates.

;;; Code:

(use-package web-mode
  :defer t
  :mode ("\\.html?\\'" . web-mode)
  :mode ("\\.phtml\\'" . web-mode)
  :mode ("\\.tpl\\.php\\'" . web-mode)
  :mode ("\\.[agj]sp\\'" . web-mode)
  :mode ("\\.as[cp]x\\'" . web-mode)
  :mode ("\\.erb\\'" . web-mode)
  :mode ("\\.mustache\\'" . web-mode)
  :mode ("\\.djhtml\\'" . web-mode)
  :mode ("\\.css\\'" . css-mode)
  :config
  ;; Define the web mode engines.
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  ;; Add a hook to web-mode.
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Disable whitespace-mode in web-mode.
              (whitespace-mode -1)
              ;; Enable emmet-mode for sgml-mode.
              (with-eval-after-load 'sgml-mode
                (use-package emmet-mode
                  :defer t
                  :config
                  (emmet-mode 1)))
              ;; Enable emmet-mode for css-mode.
              (with-eval-after-load 'css-mode
                (use-package emmet-mode
                  :defer t
                  :config
                  (emmet-mode 1)))
              ;; Enable css-eldoc for web-mode.
              (with-eval-after-load 'web-mode
                (use-package css-eldoc
                  :defer t)))))

(provide 'pkg-web)
