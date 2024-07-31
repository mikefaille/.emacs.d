;; `M-x combobulate' (default: `C-c o o') to start using Combobulate

(straight-use-package
 '(combotulate :type git :host github :repo "mickeynp/combobulate"))
(use-package treesit-auto

  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)


(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
              '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                (toml "https://github.com/tree-sitter/tree-sitter-toml")
                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
		(dart . ("https://github.com/UserNobody14/tree-sitter-dart" "master"))
		(go . ("https://github.com/smacker/go-tree-sitter" "master" "src") )
		))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
         '((python-mode . python-ts-mode)
           (css-mode . css-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (js2-mode . js-ts-mode)
           (bash-mode . bash-ts-mode)
           (css-mode . css-ts-mode)
           (json-mode . json-ts-mode)
           (js-json-mode . json-ts-mode)
	   (dart-mode . dart-ts-mode)
	   (go-mode . go-ts-mode)
	   ))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
)


(provide 'pkg-combobulate)
