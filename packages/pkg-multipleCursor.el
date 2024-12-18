(use-package mc-extras
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :bind (:map mc/keymap
              ("C-. =" . mc/compare-chars)
              ("C-. C-a" . mc/edit-beginnings-of-lines)
              ("C-. C-e" . mc/edit-ends-of-lines)
              ("C-. |" . mc/vertical-align)
              ("C-. n" . mc/insert-numbers)
              ("C-. s" . mc/sort-regions)
              ("C-. r" . mc/reverse-regions)))

(provide 'pkg-multipleCursor)
