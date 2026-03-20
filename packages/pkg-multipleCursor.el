;; -*- lexical-binding: t; -*-
;;; pkg-multipleCursor.el --- Multiple cursors configuration

(require 'use-package)

(use-package multiple-cursors
  :ensure t
  :init
  (global-set-key (kbd "C-c C-c") #'mc/edit-lines)
  (global-set-key (kbd "C->") #'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-v") #'mc/mark-all-like-this))

(use-package mc-extras
  :ensure t
  :after multiple-cursors
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
;;; pkg-multipleCursor.el ends here
