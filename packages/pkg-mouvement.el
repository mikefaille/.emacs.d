(require-package 'ace-window)
(require-package 'avy)
(require-package 'zop-to-char)
(require-package 'ace-jump-buffer)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-w") 'ace-window)
(global-set-key (kbd "C-c k") 'ace-jump-buffer)
(global-set-key (kbd "s->") 'ace-jump-buffer)


;; replace zap-to-char functionaity with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

(provide 'pkg-mouvement)
