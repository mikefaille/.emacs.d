;; -*- lexical-binding: t; -*-

(require 'use-package)

;; for eat terminal backend:
(use-package eat
  :ensure t
  :defer t)

;; for slash commands popup
(use-package popup
  :ensure t
  :defer t)

;; install gemini-cli.el
(use-package gemini-cli
  :ensure t
  :vc (:url "https://github.com/linchen2chris/gemini-cli.el" :rev :newest)
  :config (gemini-cli-mode)
  :bind-keymap ("C-c c" . gemini-cli-command-map))

(provide 'pkg-gemini)
