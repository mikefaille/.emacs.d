;;; package --- Suma auto-complete

;;; Commentary:
(require-package 'auto-complete)
;;; Code:

;; auto-complete latex
(require-package 'auto-complete-auctex)

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
(provide 'pkg-ac-complete)
;;; ac-complete ends here
