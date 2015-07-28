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
(require-package 'exec-path-from-shell)

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
  )
)





;; helper function
(defun go-run ()
  "run current buffer"
  (interactive)
  (compile (concat "go run " (buffer-file-name))))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-c") 'go-run)))


(load   (concat (exec-path-from-shell-copy-env "GOPATH")  "/src/github.com/nsf/gocode/emacs/go-autocomplete.el"))


;; wget https://github.com/golang/tools/blob/master/cmd/oracle/oracle.el
;; using fedora ? install golang-googlecode-tools-oracle package
;; tutorial http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/


(load (concat
       (exec-path-from-shell-copy-env "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
;; (require 'oracle)




;;(exe)
;;(exec-path-from-shell-initialize)
(add-to-list 'exec-path (concat
                         (exec-path-from-shell-copy-env "GOPATH") "/bin"))
;;(exec-path-from-shell-copy-env "GOPAT")
;(executable-interpret "go get github.com/golang/lint/golint")


(add-hook 'before-save-hook 'gofmt-before-save)


;(require-package go-errcheck)
;;(executable-interpret "go get github.com/kisielk/errcheck")



;go-remove-unused-imports
;; Instead of offering a function for removing a single import, go-mode will detect all unused imports and delete them (or comment them) once you run go-remove-unused-imports. It is not bound to a key by default, but you can bind it yourself if you want to. Personally I have bound it to C-c C-r:
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

; go-goto-imports
;; If you decide you want to look at your imports or edit them manually, go-goto-imports will take you to them automatically, placing your cursor after the last import. It isn’t bound to a key, either, mainly because I couldn’t come up with a good default that didn’t violate Emacs guidelines. But you can bind it manually, just like before:
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c i") 'go-goto-imports)))

;; eldoc for go
(require-package 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)



(eval-after-load 'go-mode
  '(progn

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




       (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
       ;; Customize compile command to run go build
       (defun my-go-mode-hook ()

                                        ; Use goimports instead of go-fmt
         (setq gofmt-command "goimports")                                      ; Call Gofmt before saving
         (add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
         (if (not (string-match "go" compile-command))
             (set (make-local-variable 'compile-command)
                  "go build -v && go test -v && go vet"))
                                        ; Godef jump key binding
         (local-set-key (kbd "M-.") 'godef-jump))
       (add-hook 'go-mode-hook 'my-go-mode-hook)

       ;; Enable go-oracle-mode if available
       (let ((oracle (executable-find "oracle")))
         (when oracle
         (setq go-oracle-command oracle)
         )))
  )


(provide 'pkg-go)
