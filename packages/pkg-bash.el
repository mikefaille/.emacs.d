;; Bash settings

(require-package 'bash-completion)

;; Function to turn off indent-tabs-mode
(defun turn-off-indent-tabs-mode ()
  "Turn off the use of tabs for indentation."
  (setq indent-tabs-mode nil)
  (setq tab-width 8))

;; Add hook for shell script mode
(add-hook 'sh-mode-hook 'turn-off-indent-tabs-mode)

;; Function to generate auto-complete candidates for bash
(defun ac-bash-candidates ()
  "Generate a list of potential completions for the current bash command.
This function is a modified version of `bash-completion-dynamic-complete'
from bash-completion.el."
  (when bash-completion-enabled
    (let* ((start (comint-line-beginning-position))
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
           (comint-completion-addsuffix nil))
      completions)))

(provide 'pkg-bash)
