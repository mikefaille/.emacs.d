(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

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
