;; -*- lexical-binding: t; -*-
;; pkg-discover.el --- Configuration for discover and related packages

(require 'use-package)

;; Configure discover.el and discover-my-major
;; discover-my-major depends on discover
(use-package discover
  :ensure t) ; Ensure the base package is installed

(use-package discover-my-major
  :ensure t
  :after discover ; Load after discover.el
  :bind (("C-h C-m" . discover-my-major) ; Bind key within use-package
         ;; Also bind discover-my-mode if needed
         ("C-h M-m" . discover-my-mode)))

;; Note: The global-set-key calls are replaced by the :bind keyword above

;; Mark this file as provided
(provide 'pkg-discover)
;;; pkg-discover.el ends here
