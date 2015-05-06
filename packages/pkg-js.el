;; sudo npm install -g tern

(require-package 'js3-mode)
(require-package 'tern)
(require-package 'tern-auto-complete)
(require-package 'json-mode)
(require-package 'json-reformat)
(require-package 'json-snatcher)

;; json :

;; C-c C-f: format the region/buffer with json-reformat (https://github.com/gongo/json-reformat)
;; C-c C-p: display a path to the object at point with json-snatcher (https://github.com/Sterlingg/json-snatcher)

(add-hook 'js3-mode-hook
          (lambda ()
            (setq js3-auto-indent-p t
                  js3-curly-indent-offset 0
                  js3-enter-indents-newline t
                  js3-expr-indent-offset 2
                  js3-indent-on-enter-key t
                  js3-lazy-commas t
                  js3-lazy-dots t
                  js3-lazy-operators t
                  js3-paren-indent-offset 2
                  js3-square-indent-offset 4)
            (linum-mode 1)))

;; https://github.com/Fuco1/smartparens/issues/239
;; (defadvice js3-enter-key (after fix-sp-state activate)
;;   (setq sp-last-operation 'sp-self-insert))

;; (sp-local-pair 'js3-mode
;;                "{"
;;                nil
;;                :post-handlers
;;                '((ome-create-newline-and-enter-sexp js3-enter-key))))
(add-to-list 'ac-modes 'js3-mode)


(when (package-installed-p 'js2-mode)
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))
(when (package-installed-p 'js3-mode)
  (add-hook 'js3-mode-hook (lambda () (tern-mode t))))
(setq tern-command (cons (executable-find "tern") '()))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))




(provide 'pkg-js)
