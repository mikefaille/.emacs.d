;;; package --- Suma auto-complete

;;; Commentary:
(require-package 'auto-complete)

;;; Code:

(require-package 'ac-capf)
(ac-capf-setup)

(require-package 'jedi)

;; auto-complete latex
(require-package 'auto-complete-auctex)


(ac-set-trigger-key "TAB")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(autoloadp 'auto-complete-config)
(ac-config-default)
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
(setq ac-auto-show-menu 0.6)
(setq ac-use-comphist t)
(setq ac-use-fuzzy t)


;; (defadvice ac-fallback-command (around no-yasnippet-fallback activate)
;;   (let ((yas-fallback-behavior nil))
;;     ad-do-it))

(add-to-list 'ac-sources 'ac-source-yasnippet)
;(setq ac-sources (append '(ac-source-yasnippet) ac-sources))

;(setq ac-source-yasnippet nil)

(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))


(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(provide 'pkg-ac-complete)
;;; ac-complete ends here
