(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(require 'org-protocol)
(setq org-capture-templates
      (quote
       (("w"
         "Default template"
         entry
         (file+headline "~/org/capture.org" "Notes")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)
        ;; ... more templates here ...
        )))

(provide 'pkg-org)










