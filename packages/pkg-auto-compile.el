;; -*- lexical-binding: t; -*-
;;; pkg-auto-compile.el --- Auto-compilation of Elisp files

(require 'use-package)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'pkg-auto-compile)
;;; pkg-auto-compile.el ends here
