;; -*- lexical-binding: t; -*-
(use-package mc-extras
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (with-eval-after-load 'mc-extras
    (define-key mc/keymap (kbd "C-. =") #'mc/compare-chars)
    (define-key mc/keymap (kbd "C-. C-a") #'mc/edit-beginnings-of-lines)
    (define-key mc/keymap (kbd "C-. C-e") #'mc/edit-ends-of-lines)
    (define-key mc/keymap (kbd "C-. |") #'mc/vertical-align)
    (define-key mc/keymap (kbd "C-. n") #'mc/insert-numbers)
    (define-key mc/keymap (kbd "C-. s") #'mc/sort-regions)
    (define-key mc/keymap (kbd "C-. r") #'mc/reverse-regions)))

(provide 'pkg-multipleCursor)