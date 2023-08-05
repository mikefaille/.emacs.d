;; Require necessary packages
(require-package 'browse-kill-ring)
(require-package 'flx-ido)

;; Enable ido mode and flx-ido mode
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

;; Enable flexible matching and use faces in ido
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)

;; Enable icomplete mode for better ido selection behavior
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html for more info
(icomplete-mode t)

;; Override M-x to use ido for command selection
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

(provide 'pkg-ido)
