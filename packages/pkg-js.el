;; JavaScript and JSON settings

(require-package 'js2-mode)
(require-package 'ac-js2)
(require-package 'tern)
(require-package 'tern-auto-complete)
(require-package 'json-mode)
(require-package 'json-reformat)
(require-package 'json-snatcher)

;; Set up Tern.js
(defun setup-tern-mode ()
  "Configure tern-mode."
  (tern-mode t)
  (when (package-installed-p 'tern-auto-complete)
    (tern-ac-setup)))

;; Enable Tern.js when entering JS2 mode
(when (package-installed-p 'js2-mode)
  (add-hook 'js2-mode-hook 'setup-tern-mode))

;; Set tern-command
(setq tern-command (cons (executable-find "tern") '()))

;; JSON mode settings
(add-hook 'json-mode-hook
	  (lambda ()
	    ;; Set formatting keybindings
	    (local-set-key (kbd "C-c C-f") 'json-reformat-region)
	    ;; Set path display keybindings
	    (local-set-key (kbd "C-c C-p") 'jsons-print-path)))

(provide 'pkg-js)
