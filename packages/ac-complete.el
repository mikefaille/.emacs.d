;;; package --- Suma auto-complete

;;; Commentary:
(require 'auto-complete)
;;; Code:


(defvar-local local-packages '(better-defaults smartparens idle-highlight-mode ido-ubiquitous find-file-in-project magit smex scpaste color-theme-solarized helm flycheck undo-tree dired-hacks-utils  yasnippet flycheck malabar-mode company company-go multiple-cursors go-mode go-autocomplete auto-complete emacs-eclim eshell-prompt-extras projectile fuzzy cl-lib deferred jedi ein ) "A list of packages to ensure are installed at launch.")



(auto-complete-mode t)
(ac-set-trigger-key "TAB")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(autoloadp 'auto-complete-config)
(ac-config-default)
(setq-default
 ac-sources
 '(
   ac-source-yasnippet
   ac-source-imenu
   ac-source-abbrev
   ac-source-words-in-same-mode-buffers
   ac-source-files-in-current-dir
   ac-source-filename
   )
)
(provide 'ac-complete)
;;; ac-complete ends here
