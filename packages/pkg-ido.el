;;; pkg-ido.el --- Ido configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures Ido, the Interactive Do mode, for completion.

;;; Code:

(require-package 'ido)
(use-package ido
  :init
  ;; Enable Ido mode for all buffer and file operations.
  (ido-mode 1)
  (ido-everywhere 1)
  ;; Enable flexible matching and use faces in Ido.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces t)
  :config
  ;; Use marginalia to show annotations in the minibuffer.
  (require-package 'marginalia)
  (use-package marginalia
    :defer t
    :bind (:map minibuffer-local-map
           ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  ;; Override M-x to use Ido for command selection.
  (global-set-key
   "\M-x"
   (lambda ()
     (interactive)
     (call-interactively
      (intern
       (ido-completing-read
        "M-x "
        (all-completions "" obarray 'commandp)))))))

(provide 'pkg-ido)
