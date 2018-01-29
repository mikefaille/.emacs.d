(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


(add-hook 'yaml-mode-hook
	  (lambda ()
	    (require-package 'indent-tools)
	    (local-set-key (kbd "C-c >") 'indent-tools-hydra/body)


	    (require-package 'flycheck-yamllint)
	    (eval-after-load 'flycheck
	      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))

	    ))

;; (flycheck-define-checker yaml-mode
;;   "Erlang syntax checker based on dialyzer."
;;   :command ("yamllint" "-f" "parsable"
;;             "-d" "{extends: relaxed, rules: {line-length: {max: 120}}}" source-original)
;;   :error-patterns
;;   ((error line-start
;;           (file-name)
;;           ":"
;;           line
;;           ":"
;;           column
;;           ":"
;;           (message)

;;           line-end))
;;   :modes yaml-mode)
;; (add-to-list 'flycheck-checkers 'erlang-dialyzer t)

(provide 'pkg-yaml)
