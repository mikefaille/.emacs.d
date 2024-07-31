;; ;; Enable vertico
;; (use-package vertico
;;   :init
;;   (vertico-mode)

;;   ;; Different scroll margin
;;   ;; (setq vertico-scroll-margin 0)

;;   ;; Show more candidates
;;   ;; (setq vertico-count 20)

;;   ;; Grow and shrink the Vertico minibuffer
;;   ;; (setq vertico-resize t)

;;   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   ;; (setq vertico-cycle t)
;;   )

;; ;; Persist history over Emacs restarts. Vertico sorts by history position.
;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;   (defun crm-indicator (args)
;;     (cons (format "[CRM%s] %s"
;;                   (replace-regexp-in-string
;;                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                    crm-separator)
;;                   (car args))
;;           (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Support opening new minibuffers from inside existing minibuffers.
;;   (setq enable-recursive-minibuffers t)

;;   ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
;;   ;; mode.  Vertico commands are hidden in normal buffers. This setting is
;;   ;; useful beyond Vertico.
;;   (setq read-extended-command-predicate #'command-completion-default-include-p))
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
;; Require necessary packages
(require-package 'browse-kill-ring)
;; (use-package flx-ido)
;; (use-package flx)
;; (require 'flx-ido)
;; (flx-ido-mode)
;; Enable ido mode and flx-ido mode
(ido-mode 1)
(ido-everywhere 1)

;; (flx-ido-mode t)

;; enable flexible matching and use faces in ido
(setq ido-enable-flex-matching t)
(setq ido-use-faces t)



;; Enable icomplete mode for better ido selection behavior
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Icomplete.html for more info
(icomplete-mode t)

;; Override M-x to use ido for command selection
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

(provide 'pkg-ido)
