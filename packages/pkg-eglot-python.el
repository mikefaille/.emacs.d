;; Require the eglot package
(require-package 'eglot)

;; Ensure eglot is enabled when python-mode starts
(add-hook 'python-mode-hook 'eglot-ensure)

;; Set python shell interpreter
(setq python-shell-interpreter "python3")

;; Run python and pop-up its shell
(defun my-python-shell-run ()
  "Run python in shell and send buffer content."
  (interactive)
  ;; Kill existing Python process if exists
  (let ((proc (get-buffer-process "*Python*")))
    (when proc
      (set-process-query-on-exit-flag proc nil)
      (kill-process proc)
      ;; Delay to make sure the process is killed
      (sleep-for 0.5)))

  ;; Start Python process
  (run-python (python-shell-parse-command) nil nil)

  ;; Send buffer content to Python process
  (python-shell-send-buffer)

  ;; Switch to Python process shell if not visible
  (unless (get-buffer-window "*Python*" t)
    (python-shell-switch-to-shell)))

;; Function to run a region in Python shell
(defun my-python-shell-run-region ()
  "Send selected region to Python shell."
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell))

;; Python key bindings
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-c") 'my-python-shell-run)
  (define-key python-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
  (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point))

(provide 'pkg-eglot-python)
