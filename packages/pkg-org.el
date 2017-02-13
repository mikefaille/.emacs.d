;; http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
;; active Babel languages


;; dont use org from melpa : https://github.com/purcell/emacs.d/issues/297

;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html

(require-package 'org-plus-contrib)

(require 'org-protocol)
(require 'org-capture)
(require 'org-mobile)

(require-package 'ox-reveal)
;; TODO : test it
(require-package 'deft)
;; (require 'deft)

(require-package 'org-bullets)

(setq org-directory "~/org")

(setq org-default-notes-file (concat org-directory "/refile.org"))

(defun set-org-file (pkg-org/file)

  (concat org-directory pkg-org/file)

  )





;; as suggested here : http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
(setq org-completion-use-ido t)

;; Set to the location of your Org files on your local system
(setq org-mobile-files org-directory )
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull (concat org-mobile-files "/flagged.org"))
;; TODO Set to <your syncthing directory root directory>/MobileOrg.
(setq org-mobile-directory "~/ownCloud/org-mobile")

(require 'org-clock)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


;; (func (org-reveal2scp)
;; (let (export-name)
;;   (defvar export-name   (org-reveal-export-to-html))
;;   (start-process "org-reveal2scp" "*org-reveal2scp*" "scp" export-name ":/var/" "-p" "222" )
;; ⇒ #<process my-process>

;;   )
;; )


(set-register ?o (cons 'file (concat org-directory "/notes.org")))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-latex-pdf-process '(
                              "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"
                              "LATEX=lualatex texi2dvi -p -b -V %f"
                              "latexmk -pdflatex='xetex -shell-escape -interaction nonstopmode' -pdf -f  %f"
                              "LATEX=xelatex texi2dvi -p -b -V %f"
                              "latexmk -pdf -f  %f"
                              "texi2dvi -p -b -V %f"
                              )
      )



;; (setq org-agenda-files (concat org-directory "/task.org"))


(with-eval-after-load 'org-capture-mode
(setq org-capture-templates
      (quote

       (("t" "todo" entry (file (set-org-file  "/refile.org"))
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file (set-org-file "/refile.org"))
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file (set-org-file "/refile.org"))
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree (set-org-file "~/git/org/diary.org"))
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file (set-org-file "~/git/org/refile.org"))
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("m" "Meeting" entry (file (set-org-file "~/git/org/refile.org"))
         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("p" "Phone call" entry (file "~/git/org/refile.org")
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file (set-org-file "~/git/org/refile.org"))
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))





  )




;; deft config
(setq deft-directory org-directory)
(setq deft-default-extension "org")
(setq deft-extensions (quote ("org" "txt" "text" "md" "markdown")))
(global-set-key [f8] 'deft)
(setq deft-auto-save-interval 5)

(defvar org-capture-default-file (concat org-directory "/capture.org"))


;; (global-set-key (kbd "C-c o")
;;                 (lambda () (interactive) (find-file "~/organizer.org")))


(global-set-key (kbd "C-c c") 'org-capture)




;; (with-eval-after-load 'org

;;   )


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

(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode)

            (visual-line-mode t)
            ;; Org-Babel

            ;; Fontifying source blocks
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "turquoise" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))


(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

            ;; Make custom markers for todo items:





(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode)

            (visual-line-mode t)
            ;; Org-Babel

            ;; Fontifying source blocks

            ;; Enable syntax highlighting in src blocks.

            (setq-default org-src-fontify-natively t)
            ))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . nil)
   (ruby . t)
   (sh . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   ))









(setq org-agenda-files
      (list
       (concat org-directory "/home/")
       (concat org-directory "/clients/desjardins")
       ;; (concat org-directory "/clients/ETS")


       ))

(setq org-log-done t)


(setq org-publish-project-alist
      '(
        ("org-DevOps-VilleMTL"
         ;; Location of org files
         :base-directory "~/src/DevOps-VilleMTL"
         :base-extension "org"
         :headline-levels 4

         ;; Location of Jekyll files
         :publishing-directory "~/Projects/randomgeekery.org/jekyll/"
         :recursive t
         :publishing-function org-publish-org-to-html

         :html-extension "html"

         ;; Only export section between <body></body>
         :body-only t)

        ("org-static-randomgeekery"
         :base-directory "~/Projects/randomgeekery.org/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory "~/Projects/randomgeekery.org/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("rg" :components ("org-randomgeekery" "org-static-randomgeekery"))
        ))


(provide 'pkg-org)
;;
