;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
;; active Babel languages


;; dont use org from melpa : https://github.com/purcell/emacs.d/issues/297



(require 'org-protocol)
(require 'org-capture)
(package-install 'deft)


(require-package 'org-bullets)

(setq org-directory "~/ownCloud/org")

(setq org-default-notes-file (concat org-directory "/notes.org"))

(set-register ?o (cons 'file (concat org-directory "/notes.org")))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))



(setq org-agenda-files (concat org-directory "/task.org"))


(with-eval-after-load 'org-capture-mode

  (setq org-capture-templates
        (quote
         (("w"
           "Default template"
           entry
           (file+headline org-capture-default-file "Notes")
           "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
           :empty-lines 1)
          ;; ... more templates here ...
          )))

  )




;; Deft config
(setq deft-directory org-directory)
(setq deft-default-extension "org")
(setq deft-extensions (quote ("org" "txt" "text" "md" "markdown")))



(defvar org-capture-default-file (concat org-directory "/capture.org"))


;; (global-set-key (kbd "C-c o")
;;                 (lambda () (interactive) (find-file "~/organizer.org")))


(global-set-key (kbd "C-c c") 'org-capture)




(with-eval-after-load 'org


  (org-bullets-mode)
  ;; Org-Babel

  ;; Fontifying source blocks

  ;; Enable syntax highlighting in src blocks.

  (setq-default org-src-fontify-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (ruby . t)
     (sh . t)
     ))


  ;; Notes / Tasks / TODOs

  ;; Make custom markers for todo items:

  ;; TODO
  ;; something that needs to be done at some point. If it has a date, it should be done on that day but it may be moved.
  ;; PENDING
  ;; something that’s awaiting feedback from someone else. If it has a date, it needs followup if there hasn’t been any feedback at that time.
  ;; MEETING
  ;; a scheduled meeting and cannot easily be rescheduled.
  ;; DOING
  ;; In progress, needs to be finished
  ;; DONE
  ;; done.
  ;; CANCELED
  ;; can be ignored. May include a note on why it’s been cancelled.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(i)" "PENDING(p)" "MEETING(m)" "|" "DONE(d)" "CANCELED(c)")))
  )

(provide 'pkg-org)
;;
