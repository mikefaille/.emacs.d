;;;; go config
;; install godef

;;go get -u github.com/nsf/gocode

;; go get -u code.google.com/p/rog-go/exp/cmd/godef
;;go install -v code.google.com/p/rog-go/exp/cmd/godef
                                        ;
;; go get github.com/golang/lint/golint
                                        ; autoloadp package : auto-complete
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
          (getenv "HOME") "/go" ;; ":"
          ":/usr/local/go"
          ;; (getenv "GOPATH")
          ))

(setq exec-path (split-string (getenv "PATH") path-separator))

(defun load-from-pathenv-ifexist(pathenv path-suffix)


  (let ((path "") path-list )
    (setq pathenv "GOPATH")
    ;; (setq path-suffix "/src/golang.org/x/tools/cmd/oracle/oracle.el" )
    ;; (setq path-list (parse-colon-path (getenv pathenv)))
    (while (and path-list (not (file-readable-p path)))
      (setq path-prefix (car path-list))

      (setq path (concat path-prefix path-suffix))

      (when (file-readable-p path)

        (load-file path)
        )
      (setq path-list (cdr path-list )))
    (file-readable-p path))
  )



(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))



(add-hook 'go-mode-hook ;; guessing
          '(lambda ()
	     (require-package 'company-go)
	     (require-package 'golint)

	     (add-to-list 'company-backends 'company-go)
	     (company-go-show-annotation t)
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


             ;; Customize compile command to run go build

             (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
             (if (not (string-match "go" compile-command))
                 (set (make-local-variable 'compile-command)
                      "go build -v && go test -v && go vet"))
                                        ; Godef jump key binding
             (local-set-key (kbd "M-.") 'godef-jump)

             (load-from-pathenv-ifexist "GOPATH"  "/src/golang.org/x/tools/cmd/oracle/oracle.el")
             (let ((oracle (executable-find "oracle")))
               (when oracle
                 (setq go-oracle-command oracle)
                 ))

             (load-from-pathenv-ifexist "GOPATH" "/src/github.com/golang/lint/misc/emacs/golint.el" )

             ;; helper function
             (defun go-run ()
               "run current buffer"
               (interactive)
               (compile (concat "go run " (buffer-file-name))))

             (local-set-key (kbd "C-c C-c") 'go-run)

             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)

             ))

(provide 'pkg-go)
