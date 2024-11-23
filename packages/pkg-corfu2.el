;;; pkg-corfu2.el --- Enhanced completion with Corfu and Orderless -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides an enhanced completion experience using Corfu,
;; Cape, Orderless, Vertico, Consult, and related packages.

;;; Code:

(require 'use-package)
(require 'completion-preview)  ;; Require completion-preview

(use-package corfu
  :ensure t
  :after cape
  :custom
  (corfu-cycle t)
  (corfu-auto t)          ;; Keep corfu-auto as it is
  (corfu-auto-prefix 2)   ;; Keep corfu-auto-prefix as it is
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'always)  ;; Experiment with this
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
          ("M-p" . corfu-previous)
          ("M-n" . corfu-next)
          ("M-d" . corfu-info-documentation))
  :init
  (global-corfu-mode)
  (global-completion-preview-mode)) ;; Enable completion-preview globally

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package corfu-echo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-echo-mode)
  :custom
  (corfu-doc-border "#FFB6C1")        ;; Customize border color (light pink)
  (corfu-doc-max-width 60)            ;; Adjust documentation width
  (corfu-doc-position 'window))       ;; Position documentation window ('window or 'inline)



(use-package cape
  :ensure t
  :defer nil  ; Don't defer loading
  :init  ; Move configuration to :config to ensure cape is loaded



  (setq completion-at-point-functions
              (list
    (cape-capf-super
     #'cape-file
     #'cape-dabbrev
     #'cape-abbrev  ; Changed from cape-keyword
     #'cape-line
     #'cape-dict)))

  ;; Configure Cape settings
  (setq cape-dabbrev-min-length 2
        cape-dabbrev-check-other-buffers 'some)

  ;; Ensure Cape works well with Corfu
  (advice-add 'cape-wrap-buster :around 'cape-capf-buster)

  :bind (("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion orderless)))
                                   (command (styles orderless-prefixes orderless))
                                   (variable (styles orderless-prefixes orderless))
                                   (symbol (styles orderless-prefixes orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex))
  :config
  (setq orderless-style-dispatchers '(orderless-affix-dispatch))
  (setq completion-category-defaults nil))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-count 13)
  (vertico-resize t)
  (vertico-multiform-mode t)
  :init
  (vertico-mode)
  :config
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)
          (consult-location buffer)))

  ;; Persist history over Emacs restarts
  (use-package savehist
    :init
    (savehist-mode))

  ;; Add extensions
  (use-package vertico-directory
    :after vertico
    :ensure nil
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :custom
  (marginalia-align 'right)
  (marginalia-max-relative-age 0))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("C-x C-r" . consult-recent-file)
         ("C-c i" . consult-imenu)
         ("C-c m" . consult-mark)
         ("M-s h" . consult-history)
         ("M-s m" . consult-mode-command)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro))
  :custom
  (consult-preview-key 'any)
  (consult-narrow-key "<")
  :config
  ;; Check if ripgrep (rg) is available
  (when (executable-find "rg")
    ;; Only bind and configure ripgrep if it's available
    (global-set-key (kbd "M-s r") #'consult-ripgrep)

    ;; Configure consult-ripgrep to use .gitignore
    (setq consult-ripgrep-args
          (concat "rg --null --line-buffered --color=ansi --max-columns=1000 "
                  "--no-heading --line-number . -H --no-ignore --hidden "
                  "-g !.git")))

  ;; Configure the asynchronous search behavior
  (setq consult-async-min-input 2)
  (setq consult-async-refresh-delay 0.15)
  (setq consult-async-input-throttle 0.2)
  (setq consult-async-input-debounce 0.1))

(use-package consult-xref
  :ensure nil  ; It's part of consult, so we don't need to ensure it separately
  :after (consult xref)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize
   consult-xref
   :preview-key
   (list :debounce 0.2 'any)))

(use-package consult-project-extra
  :ensure t
  :after consult
  :bind (("C-c p f" . consult-project-extra-find)
         ("C-c p o" . consult-project-extra-find-other-window)))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Enable recursive minibuffers and savehist
(setq enable-recursive-minibuffers t)
(use-package savehist
  :init
  (savehist-mode))

(provide 'pkg-corfu2)
