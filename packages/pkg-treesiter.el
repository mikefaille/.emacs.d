;;; pkg-treesiter.el --- Advanced Tree-sitter configuration -*- lexical-binding: t; -*-

(require 'use-package)

;; --- 1. The Foundation: treesit-auto ---
;; Handles grammar installation and remapping classic modes to -ts-modes.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'always)
  :config
  ;; Ensure all supported languages are handled
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; --- 2. The Power: Combobulate ---
;; Adds structured editing (navigation, splicing, cloning) for non-Lisp languages.
(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c o")
  ;; Note: Combobulate works on specific -ts-modes. Add hooks as needed.
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (go-ts-mode . combobulate-mode)))

;; --- 3. The Polish: Modern Folding ---
(use-package treesit-fold
  :ensure (treesit-fold :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (prog-mode . treesit-fold-mode))

;; --- 4. Astro Support ---
(use-package treesit
  :ensure nil
  :config
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(astro "https://github.com/virchau13/tree-sitter-astro"))
    
    (define-derived-mode astro-ts-mode prog-mode "Astro[TS]"
      "Major mode for .astro files using Tree-sitter."
      (when (treesit-ready-p 'astro)
        (treesit-parser-create 'astro)
        (treesit-major-mode-setup)))

    (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))))

(provide 'pkg-treesiter)
;;; pkg-treesiter.el ends here
