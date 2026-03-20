;;; pkg-go.el --- Go Configuration -*- lexical-binding: t; -*-

(require 'use-package)

(use-package go-mode
  :ensure t
  :config
  ;; Go config
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

  (defun setup-go-mode ()
    "Setup for Go development."
    ;; Set environment variables
    (set-go-environment)

    ;; Set compile command
    (set (make-local-variable 'compile-command)
         "go build -v && go test -v && go vet")

    ;; Set run key binding
    (local-set-key (kbd "C-c C-c") (lambda ()
                                       (interactive)
                                       (compile (concat "go run " (buffer-file-name)))))

    ;; Stop highlighting whitespace
    (whitespace-toggle-options '(tabs))

    ;; Run gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save))
  
  :hook (go-mode . setup-go-mode))

;; LSP Integration (Replaces Eglot)
;; lsp-mode is already configured for go-mode in pkg-lsp.el, 
;; so we just ensure it's enabled if not already caught by the hook there.
;; (add-hook 'go-mode-hook #'lsp-deferred) ; Handled in pkg-lsp.el

(provide 'pkg-go)
;;; pkg-go.el ends here
