(require-package 'browse-kill-ring)
(require-package 'flx-ido)

(ido-mode 1)
(ido-everywhere 1)


(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html
;; for better ido selection behavior
(icomplete-mode t)

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
