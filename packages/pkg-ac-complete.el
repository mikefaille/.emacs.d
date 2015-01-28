;;; package --- Suma auto-complete

;;; Commentary:

(require-package 'auto-complete)
(require-package 'ac-capf)
(ac-capf-setup)
(require 'go-autocomplete)
(ac-config-default)

;;; Code:




(require-package 'jedi)

;; auto-complete latex
(require-package 'auto-complete-auctex)



(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

;; (setq-default
;;  ac-sources
;;  '(
;;    ac-source-yasnippet
;;    ac-source-imenu
;;    ac-source-abbrev
;;    ac-source-words-in-same-mode-buffers
;;    ac-source-files-in-current-dir
;;    ac-source-filename
;;    ac-source-semantic

;;    )
;; )

(setq ac-auto-start 1)
(setq ac-auto-show-menu 7)
(setq ac-use-comphist t)
(setq ac-use-fuzzy t)


;; (defadvice ac-fallback-command (around no-yasnippet-fallback activate)
;;   (let ((yas-fallback-behavior nil))
;;     ad-do-it))



(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(setq-default ac-sources (push 'ac-source-yasnippet ac-sources))

;(setq ac-source-yasnippet nil)


(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(provide 'pkg-ac-complete)
;;; ac-complete ends here
