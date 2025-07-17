;;; pkg-rust.el --- Rust configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures Rust support with rust-mode and flycheck-rust.

;;; Code:

(use-package rust-mode
  :defer t
  :config
  (use-package flycheck-rust
    :defer t)

  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

  (add-hook 'rust-mode-hook #'(lambda()
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

                                (define-key rust-mode-map (kbd "C-c C-c") 'rust-save-compile-and-run)

                                (eval-after-load 'flycheck
                                  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))))

(provide 'pkg-rust)
