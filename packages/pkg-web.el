(require-package 'web-mode)
(add-hook 'web-mode-hook (lambda ()
                           (whitespace-mode -1)
                           (require-package 'css-eldoc)
                           (require-package 'emmet-mode)

                           ;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

                           (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
                           (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

                           (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))



                           (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
                           (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
                           (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
                           (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
                           (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
                           (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
                           (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
                           (setq web-mode-engines-alist
                                 '(("php"    . "\\.phtml\\'")
                                   ("blade"  . "\\.blade\\."))
                                 )



                           (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes


                           (require 'smart-tab)
                           (defun add-emmet-expand-to-smart-tab-completions ()
                             ;; Add an entry for current major mode in
                             ;; `smart-tab-completion-functions-alist' to use
                             ;; `emmet-expand-line'.
                             (add-to-list 'smart-tab-completion-functions-alist
                                          (cons major-mode #'emmet-expand-line)))
                           (add-hook 'sgml-mode-hook 'add-emmet-expand-to-smart-tab-completions)

                           (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


                           ))

(provide 'pkg-web)
