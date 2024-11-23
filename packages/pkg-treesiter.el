;; First, we make sure that both the `treesit` and `combobulate` packages are installed
(require-package 'treesit)


;; Once `treesit` is loaded, we can configure it
(eval-after-load 'treesit
  '(progn
     ;; Function to install Tree-sitter grammars
     ;; This function goes through a list of grammar specifications, each containing
     ;; a language name and the corresponding URL of the Tree-sitter grammar.
     ;; For each grammar, if it is not already available, it is installed.
     (defun mp-setup-install-grammars ()
       "Install Tree-sitter grammars if they are absent."
       (interactive)
       (dolist (grammar
                '((css "https://github.com/tree-sitter/tree-sitter-css")
                  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                  (python "https://github.com/tree-sitter/tree-sitter-python")
                  (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
         (add-to-list 'treesit-language-source-alist grammar)
         (unless (treesit-language-available-p (car grammar))
           (treesit-install-language-grammar (car grammar)))))

     ;; Remapping of major modes to their Tree-sitter counterparts
     ;; This allows Tree-sitter to be used instead of the standard major modes for
     ;; the languages listed
     (dolist (mapping '((python-mode . python-ts-mode)
                        (css-mode . css-ts-mode)
                        (typescript-mode . tsx-ts-mode)
                        (js-mode . js-ts-mode)
                        (css-mode . css-ts-mode)
                        (yaml-mode . yaml-ts-mode)))
       (add-to-list 'major-mode-remap-alist mapping))

     ;; Define the key prefix for `combobulate` commands
     (setq combobulate-key-prefix "C-c o")

     ;; Hooks to enable `combobulate-mode` in the Tree-sitter modes for the languages listed
     ;; When a file in one of these languages is opened, `combobulate-mode` will be activated
     (add-hook 'python-ts-mode 'combobulate-mode)
     (add-hook 'js-ts-mode 'combobulate-mode)
     (add-hook 'css-ts-mode 'combobulate-mode)
     (add-hook 'yaml-ts-mode 'combobulate-mode)
     (add-hook 'typescript-ts-mode 'combobulate-mode)
     (add-hook 'tsx-ts-mode 'combobulate-mode)))

(provide 'pkg-combobulate)
