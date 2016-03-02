;;;; go config
;; install godef

;;go get -u github.com/nsf/gocode

;; go get -u code.google.com/p/rog-go/exp/cmd/godef
;;go install -v code.google.com/p/rog-go/exp/cmd/godef
                                        ;
;; go get github.com/golang/lint/golint
; autoloadp package : auto-complete


;; go lint
(require-package 'golint)
(require-package 'go-autocomplete)
(require-package 'go-mode)

(setenv "PATH"
  ( concat
    "/usr/local/go/bin" ":"
    (getenv "HOME") "/go/bin" ":"

    (getenv "PATH")
  )
)

(setenv "GOPATH"
  ( concat
    (getenv "HOME") "/go" ":"

    (getenv "GOPATH")
  ))



;; (require-package go-errcheck)
;;(executable-interpret "go get github.com/kisielk/errcheck")


; go-goto-imports
;; If you decide you want to look at your imports or edit them manually, go-goto-imports will take you to them automatically, placing your cursor after the last import. It isn’t bound to a key, either, mainly because I couldn’t come up with a good default that didn’t violate Emacs guidelines. But you can bind it manually, just like before:
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))



(with-eval-after-load 'go-mode

  (require-package 'go-eldoc)
  (go-eldoc-setup)
  ;; Add to default go-mode key bindings
  (let ((map go-mode-map))
    (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c m") 'go-test-current-file)
    (define-key map (kbd "C-c .") 'go-test-current-test)
    (define-key map (kbd "C-c b") 'go-run)
    (define-key map (kbd "C-h f") 'godoc-at-point))
  ;; Prefer goimports to gofmt if installed
  (let ((goimports (executable-find "goimports")))
    (when goimports
      (setq gofmt-command goimports)))


  ;; stop whitespace being highlighted
  (whitespace-toggle-options '(tabs))


  ;; TODO Fix this path
  (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
  ;; Customize compile command to run go build

  (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
                                        ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)


  (let ((file-oracle "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))
    (when (file-exists-p file-oracle)
      (load-file file-oracle)

      )
    )

  (let ((oracle (executable-find "oracle")))
    (when oracle
      (setq go-oracle-command oracle)
      ))


  (let ((file-go-autocomplete "$GOPATH/src/github.com/nsf/gocode/emacs/go-autocomplete.el"))
    (when (file-exists-p file-go-autocomplete)
      (load-file file-go-autocomplete)
      )
    )


  ;; helper function
  (defun go-run ()
    "run current buffer"
    (interactive)
    (compile (concat "go run " (buffer-file-name))))

  (local-set-key (kbd "C-c C-c") 'go-run)

  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  ;; Enable go-oracle-mode if available


  )



(provide 'pkg-go)
