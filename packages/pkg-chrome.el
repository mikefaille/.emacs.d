(when (demonp)
  (require-package 'edit-server-htmlize)
  (setq edit-server-new-frame t)
  (autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
  (autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
  (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
  (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)


  (defun html2text-clean-span (p1 p2 p3 p4)
    (html2text-delete-tags p1 p2 p3 p4))

  (add-to-list 'html2text-format-tag-list '("span" . html2text-clean-span))

  )

(provide 'pkg-chrome)
