;;;; go config
;; install godef 
; go get code.google.com/p/rog-go/exp/cmd/godef
; go get github.com/golang/lint/golint
; autoloadp package : auto-complete

;; go lint
(require-package 'golint)


(require-package 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(require-package 'go-autocomplete)
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)



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

(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/nsf/gocode"))

;(executable-interpret "go get github.com/golang/lint/golint")

;;(executable-interpret "go get github.com/kisielk/errcheck")


(provide 'pkg-go)
