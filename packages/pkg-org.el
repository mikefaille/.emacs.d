;; -*- lexical-binding: t; -*-
;; pkg-org.el --- Configuration for Org mode and related packages

(require 'use-package)

;; --- Core Org Mode Configuration ---
(use-package org
  ;; :ensure t ; Uncomment only if using Org ELPA version, otherwise built-in is used
  :hook (org-mode . my-org-mode-hook) ; Add custom org-mode hook
  :bind (("C-c c" . org-capture)) ; Standard capture binding
  :custom
  ;; Directories and Files
  (org-directory (expand-file-name "~/org"))
  (org-default-notes-file (expand-file-name "refile.org" org-directory))
  (org-agenda-files (list (expand-file-name "home" org-directory)
                          (expand-file-name "clients/desjardins" org-directory))) ; Adjust paths as needed
  ;; ID Management
  (org-id-track-globally t)
  (org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory)) ; Store IDs within .emacs.d
  ;; Logging
  (org-log-done 'time) ; Log completion time
  ;; Source Blocks
  (org-src-fontify-natively t) ; Fontify source blocks using native modes
  (org-src-tab-acts-natively t) ; Make TAB behave as in native mode
  (org-confirm-babel-evaluate nil) ; Don't ask for confirmation to evaluate code blocks (use with caution)
  ;; Other settings
  (org-hide-emphasis-markers t) ; Hide emphasis markers like *bold* -> bold

  :config
  ;; Load Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) ; Enable elisp evaluation (nil = don't require confirmation)
     (ruby . t)
     (shell . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (python . t)))

  ;; Org Clock Persistence (requires org-clock module)
    ;; Org Clock Persistence (requires org-clock module)
  (with-eval-after-load 'org-clock
    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)))

  ;; Org Capture Templates (define within :config or :init)
  (setq org-capture-templates
        '(("t" "todo" entry (file (expand-file-name "refile.org" org-directory))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "note" entry (file (expand-file-name "refile.org" org-directory))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree (expand-file-name "diary.org" org-directory)) ; Corrected path assumption
           "* %?\n%U\n" :clock-in t :clock-resume t)))

  ;; Org Publishing configuration
  (setq org-publish-project-alist
        '(("org-DevOps-VilleMTL"
           :base-directory "~/src/DevOps-VilleMTL" ; Ensure path exists
           :base-extension "org"
           :headline-levels 4
           :publishing-directory "~/Projects/randomgeekery.org/jekyll/" ; Ensure path exists
           :recursive t
           :publishing-function org-html-publish-to-html ; Use modern function name
           :html-extension "html"
           :body-only t)
          ("org-static-randomgeekery"
           :base-directory "~/Projects/randomgeekery.org/org/" ; Ensure path exists
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg"
           :publishing-directory "~/Projects/randomgeekery.org/" ; Ensure path exists
           :recursive t
           :publishing-function org-publish-attachment)
          ("rg" :components ("org-randomgeekery" "org-static-randomgeekery")))) ; Check component names match project names
  )

;; --- Org Mode Hook Function ---
;; Defines settings/modes to enable specifically within Org buffers
(defun my-org-mode-hook ()
  "Custom hook for Org mode."
  ;; Enable nicer bullets (requires org-bullets package)
  (org-bullets-mode 1)
  ;; Use visual lines for better wrapping
  (visual-line-mode 1)
  ;; Other mode-specific settings can go here
  )

;; --- Org Related Packages ---

;; Org Bullets for prettier lists
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)) ; Enable via hook is cleaner

;; Org Brain for Zettelkasten-like notes
(use-package org-brain
  :ensure t
  :after org
  :custom
  (org-brain-path (expand-file-name "brain" org-directory)) ; Use org-directory variable
  (org-brain-visualize-default-choices 'all)
  (org-brain-title-max-length 12)
  :config
  ;; Add org-brain capture template (ensure org-capture-templates is defined first)
  (add-to-list 'org-capture-templates
               '("b" "Brain" plain #'org-brain-capture-goto-end ; Use function symbol
                 "* %i%?" :empty-lines 1))

  ;; Set initial state for visualize mode after evil loads
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs)))

;; Deft for quick note searching/filtering
(use-package deft
  :ensure t
  :bind (("<f8>" . deft)) ; Use :bind for keys
  :custom
  (deft-directory org-directory) ; Use org-directory variable
  (deft-default-extension "org")
  (deft-extensions '("org" "txt" "text" "md" "markdown"))
  (deft-auto-save-interval 5))

;; Org Reveal.js presentations (ox-reveal)
(use-package ox-reveal
  :ensure t
  :after org) ; Load after org

;; Org Mobile synchronization
(use-package org-mobile
  :ensure nil ; Part of Org contrib, ensure Org is recent enough or install separately if needed
  :after org
  :custom
  (org-mobile-directory (expand-file-name "~/ownCloud/org-mobile")) ; Ensure path exists
  (org-mobile-files org-directory) ; Sync entire org directory (adjust if needed)
  (org-mobile-inbox-for-pull (expand-file-name "flagged.org" org-directory)) ; Adjust path
  )

;; Org Protocol for external links (e.g., from browser)
(use-package org-protocol
  :ensure nil ; Built-in
  ;; Configuration often involves server setup and browser extensions
  ;; No specific Emacs config needed here unless customizing protocol handling
  ;; (require 'org-protocol) ; Only if needed before functions are autoloaded
  )

;; Mark this file as provided
(provide 'pkg-org)

(use-package outline
  :ensure nil ; Built-in
  :config
  (setq outline-minor-mode-prefix "c o"))
;;; pkg-org.el ends here
