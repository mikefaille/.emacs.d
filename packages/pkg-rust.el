(require-package 'rust-mode)
(require-package 'flycheck-rust)


(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(defun rust-save-compile-and-run ()
  (interactive)
  (save-buffer)

  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")

      (compile "cargo run")

    (compile
     (format "rustc %s & ./%s"
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name))))))

(add-hook 'rust-mode-hook
          (lambda ()
            (define-key rust-mode-map (kbd "<f5>") 'rust-save-compile-and-run)))
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


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

(local-set-key (kbd "C-c C-c") 'rust-save-compile-and-run)

(provide 'pkg-rust)
