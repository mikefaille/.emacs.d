;;;; go config
(require-package 'go-mode)
(require-package 'eglot)

(setq go-path (concat (getenv "HOME") "/go")
      go-bin "/usr/local/go/bin"
      go-tools '("goimports" "godef" "oracle" "golint"))

(defun add-to-PATH (path)
  "Add PATH to the PATH environment variable and `exec-path'."
  (setenv "PATH" (concat path path-separator (getenv "PATH")))
  (add-to-list 'exec-path path))

(defun set-go-environment ()
  "Set environment variables for Go development."
  (setenv "GOPATH" (concat go-path ":/usr/local/go"))
  (add-to-PATH go-bin)
  (add-to-PATH (concat go-path "/bin")))

(defun load-go-tool (tool)
  "Load the Go tool TOOL if it exists."
  (let ((toolpath (concat go-path "/src/" tool "/" tool ".el")))
    (when (file-readable-p toolpath)
      (load-file toolpath))))

(defun setup-go-mode ()
  "Setup for Go development."
  ;; Set environment variables
  (set-go-environment)

  ;; Set compile command
  (set (make-local-variable 'compile-command)
       "go build -v && go test -v && go vet")

  ;; Set jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)

  ;; Set run key binding
  (local-set-key (kbd "C-c C-c") (lambda ()
                                   (interactive)
                                   (compile (concat "go run " (buffer-file-name)))))

  ;; Stop highlighting whitespace
  (whitespace-toggle-options '(tabs))

  ;; Run gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Use goimports if available
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))

  ;; Load go tools
  (dolist (tool go-tools)
    (load-go-tool tool)))

(add-hook 'go-mode-hook 'setup-go-mode)

(provide 'pkg-go)
