;; Ensure quelpa-use-package is installed and available
(require-package 'quelpa-use-package)
(require 'quelpa-use-package)

;; Make the checkout process more efficient
(setq quelpa-checkout-type 'light)

;; Function to install Tree-sitter grammars if they are not already installed
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
    ;; Only install `grammar' if it's not already installed
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

;; Load treesit package
(require 'treesit)

;; Remap major modes - Tree-sitter enabled major modes are distinct from their ordinary counterparts
(dolist (mapping '((python-mode . python-ts-mode)
                   (css-mode . css-ts-mode)
                   (typescript-mode . tsx-ts-mode)
                   (js-mode . js-ts-mode)
                   (css-mode . css-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
  (add-to-list 'major-mode-remap-alist mapping))

;; Install Tree-sitter grammars
(mp-setup-install-grammars)

;; Set up Combobulate using use-package
(use-package combobulate
  :if (not (package-installed-p 'combobulate))
  :quelpa (combobulate :fetcher github :repo "mickeynp/combobulate"))

;; Set up Combobulate, regardless of how it was installed
(use-package combobulate
  :preface
  ;; Set the Combobulate key prefix
  (setq combobulate-key-prefix "C-c o")

  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode))
  )

(provide 'pkg-combobulate)
