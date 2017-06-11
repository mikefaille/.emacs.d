(require-package 'rust-mode)
(require-package 'flycheck-rust)


(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)


(add-hook 'rust-mode-hook #'(lambda()
                              ;; (setq racer-cmd "<path-to-racer-srcdir>/target/release/racer")
                              ;; (setq racer-rust-src-path "<path-to-rust-srcdir>/src/")
                              ;; (racer-mode)
                              (eldoc-mode)
                              (defun rust-save-compile-and-run ()
                                (interactive)
                                (save-buffer)

                                (if (locate-dominating-file (buffer-file-name) "Cargo.toml")

                                    (compile "cargo run")


                                  (compile

                                   (format "rustc %s &&  .//%s"
                                           (file-name-nondirectory (buffer-file-name))

                                           (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))




                              ;; (define-key rust-mode-map (kbd "<f5>") 'rust-save-compile-and-run)

                              (define-key rust-mode-map (kbd "C-c C-c") 'rust-save-compile-and-run)
                              (setq rust-format-on-save t)

                              ;; (eval-after-load 'flycheck
                              ;;   '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))





                              ))


;; (flycheck-define-checker servo-rust
;;   "A Rust syntax checker using the Rust compiler in Servo."
;;   :command ("/path/to/servo/rustc"
;;             "--parse-only"
;;             source)
;;   :error-patterns
;;   ((error line-start (file-name) ":" line ":" column ": "
;;           (one-or-more digit) ":" (one-or-more digit) " error: "
;;           (message) line-end))
;;   :modes rust-mode)

                                        ;(add-hook 'rust-mode-hook (lambda () (flycheck-select-checker 'servo-rust)))




(provide 'pkg-rust)
