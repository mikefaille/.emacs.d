;; -*- lexical-binding: t; -*-
;;; pkg-bash.el --- Bash and Shell Script settings

(require 'use-package)

;; Modern Bash completion using bash-completion package
(use-package bash-completion
  :ensure t
  :init
  (bash-completion-setup))

;; Function to turn off indent-tabs-mode and set standard width
(defun michael-bash-mode-setup ()
  "Setup standard indentation for shell scripts."
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

;; Add hook for shell script mode
(add-hook 'sh-mode-hook #'michael-bash-mode-setup)
(add-hook 'bash-ts-mode-hook #'michael-bash-mode-setup)

(provide 'pkg-bash)
;;; pkg-bash.el ends here
