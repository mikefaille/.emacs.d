;;; -*- lexical-binding: t -*-

;; This is the file for python eglot configuration.
;; It should be required from your init.el file with (require 'pkg-eglot-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Configure Eglot for Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :hook (python-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylyzer"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Custom Functions for Interactive Python Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-python-shell-run ()
  "Run python in shell, killing the old process and sending the buffer."
  (interactive)
  ;; Kill existing Python process if it exists to ensure a fresh start
  (when-let* ((proc (get-buffer-process "*Python*"))) ; <-- This is the corrected line
    (set-process-query-on-exit-flag proc nil)
    (kill-process proc)
    (sleep-for 0.2)) ; Short delay to ensure the process is killed

  ;; Start a new Python process
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(defun my-python-shell-run-region (start end)
  "Send selected region to the Python shell."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Configure Python Mode and Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :ensure t
  :config
  (setq python-shell-interpreter "python3")

  ;; Setup keybindings after python-mode is loaded
  (define-key python-mode-map (kbd "C-c C-c") #'my-python-shell-run)
  (define-key python-mode-map (kbd "C-c C-r") #'my-python-shell-run-region)
  ;; NOTE: C-h f is a standard Emacs help key. Eglot provides documentation
  ;; automatically, so this binding may not be necessary.
  ;; (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)
  )

(provide 'pkg-eglot-python)
