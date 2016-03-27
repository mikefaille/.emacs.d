;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
;; active Babel languages


;; dont use org from melpa : https://github.com/purcell/emacs.d/issues/297
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (ruby . t)
   (sh . t)
   ))

(require 'org-protocol)
(require 'org-capture)
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

;; (global-set-key (kbd "C-c o")
;;                 (lambda () (interactive) (find-file "~/organizer.org")))

(set-register ?o (cons 'file "~/organizer.org"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(ido-mode)
(setq org-completion-use-ido t)


(global-set-key (kbd "C-c c") 'org-capture)

(setq org-default-notes-file "~/organizer.org")

(setq org-default-notes-file (concat org-directory "/notes.org"))

(provide 'pkg-org)
;;
