(require-package 'org)

;; Load necessary packages and libraries
(require 'org-protocol)
(require 'org-capture)
(require 'org-mobile)

;; Install necessary packages if not already installed
(require-package 'ox-reveal)
(require-package 'org-bullets)
(require-package 'org-brain)
(require-package 'deft)

;; Set the directory where org files are stored
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/refile.org"))

;; Function to get the path to an org file
(defun set-org-file (pkg-org/file)
  (concat org-directory pkg-org/file))

;; Set the path for org-brain
(setq org-brain-path "~/org/brain")

;; Set initial state for org-brain-visualize-mode
(with-eval-after-load 'evil
  (evil-set-initial-state 'org-brain-visualize-mode 'emacs))

;; Set configuration for org ids
(setq org-id-track-globally t)
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")

;; Add a template for org-brain to org-capture-templates
(push '("b" "Brain" plain (function org-brain-goto-end)
        "* %i%?" :empty-lines 1)
      org-capture-templates)

;; Set org-brain configuration
(setq org-brain-visualize-default-choices 'all)
(setq org-brain-title-max-length 12)

;; Set up org-mobile
(setq org-mobile-files org-directory)
(setq org-mobile-inbox-for-pull (concat org-mobile-files "/flagged.org"))
(setq org-mobile-directory "~/ownCloud/org-mobile")

;; Set up org-clock
(require 'org-clock)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Set org-capture templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file (set-org-file  "/refile.org"))
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file (set-org-file "/refile.org"))
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree (set-org-file "~/git/org/diary.org"))
               "* %?\n%U\n" :clock-in t :clock-resume t))))

;; Configure deft
(setq deft-directory org-directory)
(setq deft-default-extension "org")
(setq deft-extensions (quote ("org" "txt" "text" "md" "markdown")))
(global-set-key [f8] 'deft)
(setq deft-auto-save-interval 5)

;; Bind org-capture to "C-c c"
(global-set-key (kbd "C-c c") 'org-capture)

;; Add hook to org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode)
            (visual-line-mode t)
            (setq-default org-src-fontify-natively t)))

;; Load languages for org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (ruby . t)
   (shell . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (python . t)))

;; Set org-agenda files
(setq org-agenda-files (list (concat org-directory "/home/")
                             (concat org-directory "/clients/desjardins")))

;; Enable logging of TODOs
(setq org-log-done t)

;; Set org-publish-project-alist
(setq org-publish-project-alist
      '(("org-DevOps-VilleMTL"
         :base-directory "~/src/DevOps-VilleMTL"
         :base-extension "org"
         :headline-levels 4
         :publishing-directory "~/Projects/randomgeekery.org/jekyll/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :html-extension "html"
         :body-only t)
        ("org-static-randomgeekery"
         :base-directory "~/Projects/randomgeekery.org/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
         :publishing-directory "~/Projects/randomgeekery.org/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("rg" :components ("org-randomgeekery" "org-static-randomgeekery"))))

(provide 'pkg-org)
