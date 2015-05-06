(defun mike-emms ()
  "Mike's emms launcher'"
  (interactive)
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)

  (setq emms-info-asynchronously t)
)

(provide 'pkg-emms)
