(require-package 'auto-complete)

(require-package 'bash-completion)

(defun ac-bash-candidates ()
"This function is a modifed version of
bash-completion-dynamic-complete from bash-completion.el"
  (when bash-completion-enabled
    (let* ( (start (comint-line-beginning-position))
                    (pos (point))
                    (tokens (bash-completion-tokenize start pos))
                    (open-quote (bash-completion-tokenize-open-quote tokens))
                    (parsed (bash-completion-process-tokens tokens pos))
                    (line (cdr (assq 'line parsed)))
                    (point (cdr (assq 'point parsed)))
                    (cword (cdr (assq 'cword parsed)))
                    (words (cdr (assq 'words parsed)))
                    (stub (nth cword words))
                    (completions (bash-completion-comm line point words cword open-quote))
                    ;; Override configuration for comint-dynamic-simple-complete.
                    ;; Bash adds a space suffix automatically.
                    (comint-completion-addsuffix nil) )
      (if completions
                  completions))))

(setq ac-source-bash
      '((candidates . ac-bash-candidates)))

(add-hook 'shell-mode-hook
          (lambda()
            (add-to-list ac-sources '(ac-source-bash))
            (auto-complete-mode)))

(provide 'pkg-bash)
