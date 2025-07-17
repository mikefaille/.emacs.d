;;; pkg-corfu2.el --- Enhanced completion with Corfu and Orderless -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides an enhanced completion experience using Corfu,
;; Cape, Orderless, Vertico, Consult, and related packages.

;;; Code:

(require-package 'corfu)
(use-package corfu
  :defer t
  :after cape
  :custom
  ;; Enable cycling through completion candidates.
  (corfu-cycle t)
  ;; Enable auto-completion.
  (corfu-auto t)
  ;; Set the prefix length for auto-completion.
  (corfu-auto-prefix 2)
  ;; Set the delay for the popup info.
  (corfu-popupinfo-delay '(0.5 . 0.2))
  ;; Insert the current candidate on preview.
  (corfu-preview-current 'insert)
  ;; Always preselect the first candidate.
  (corfu-preselect 'always)
  ;; Don't quit on exact match.
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
          ("M-p" . corfu-previous)
          ("M-n" . corfu-next)
          ("M-d" . corfu-info-documentation))
  :init
  ;; Enable Corfu globally.
  (global-corfu-mode)
  ;; Enable completion preview globally.
  (global-completion-preview-mode))

(require-package 'corfu-popupinfo)
(use-package corfu-popupinfo
  :defer t
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  ;; Set the delay for the popup info.
  (corfu-popupinfo-delay '(0.5 . 0.2)))

(require-package 'corfu-echo)
(use-package corfu-echo
  :defer t
  :after corfu
  :hook (corfu-mode . corfu-echo-mode)
  :custom
  ;; Customize the border color of the echo area.
  (corfu-doc-border "#FFB6C1")
  ;; Adjust the documentation width.
  (corfu-doc-max-width 60)
  ;; Position the documentation window.
  (corfu-doc-position 'window))

(require-package 'cape)
(use-package cape
  :defer t
  :init
  ;; Set the completion at point functions.
  (setq completion-at-point-functions
        (list
         (cape-capf-super
          #'cape-file
          #'cape-dabbrev
          #'cape-abbrev
          #'cape-line
          #'cape-dict)))
  ;; Configure Cape settings.
  (setq cape-dabbrev-min-length 2
        cape-dabbrev-check-other-buffers 'some)
  ;; Ensure Cape works well with Corfu.
  (advice-add 'cape-wrap-buster :around 'cape-capf-buster)
  :bind (("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)))

(require-package 'orderless)
(use-package orderless
  :defer t
  :custom
  ;; Set the completion styles.
  (completion-styles '(orderless basic))
  ;; Set the completion category overrides.
  (completion-category-overrides '((file (styles . (partial-completion orderless)))
                                   (command (styles orderless-prefixes orderless))
                                   (variable (styles orderless-prefixes orderless))
                                   (symbol (styles orderless-prefixes orderless))))
  ;; Set the orderless component separator.
  (orderless-component-separator #'orderless-escapable-split-on-space)
  ;; Set the orderless matching styles.
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex))
  :config
  ;; Set the orderless style dispatchers.
  (setq orderless-style-dispatchers '(orderless-affix-dispatch))
  (setq completion-category-defaults nil))

(require-package 'vertico)
(use-package vertico
  :defer t
  :custom
  ;; Enable cycling through completion candidates.
  (vertico-cycle t)
  ;; Set the number of candidates to show.
  (vertico-count 13)
  ;; Enable resizing of the Vertico minibuffer.
  (vertico-resize t)
  ;; Enable multiform mode.
  (vertico-multiform-mode t)
  :init
  ;; Enable Vertico mode.
  (vertico-mode)
  :config
  ;; Set the vertico multiform categories.
  (setq vertico-multiform-categories
        '((file grid)
          (consult-grep buffer)
          (consult-location buffer)))
  ;; Persist history over Emacs restarts.
  (require-package 'savehist)
  (use-package savehist
    :defer t
    :init
    (savehist-mode))
  ;; Add extensions.
  (require-package 'vertico-directory)
  (use-package vertico-directory
    :defer t
    :after vertico
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(require-package 'marginalia)
(use-package marginalia
  :defer t
  :init (marginalia-mode)
  :custom
  ;; Align the annotations to the right.
  (marginalia-align 'right)
  ;; Set the maximum relative age of annotations.
  (marginalia-max-relative-age 0))

(require-package 'consult)
(use-package consult
  :defer t
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
  ;; Set the preview key.
  (consult-preview-key 'any)
  ;; Set the narrow key.
  (consult-narrow-key "<")
  :config
  ;; Check if ripgrep (rg) is available.
  (when (executable-find "rg")
    ;; Only bind and configure ripgrep if it's available.
    (global-set-key (kbd "M-s r") #'consult-ripgrep)
    ;; Configure consult-ripgrep to use .gitignore.
    (setq consult-ripgrep-args
          (concat "rg --null --line-buffered --color=ansi --max-columns=1000 "
                  "--no-heading --line-number . -H --no-ignore --hidden "
                  "-g !.git")))
  ;; Configure the asynchronous search behavior.
  (setq consult-async-min-input 2)
  (setq consult-async-refresh-delay 0.15)
  (setq consult-async-input-throttle 0.2)
  (setq consult-async-input-debounce 0.1))

(require-package 'consult-xref)
(use-package consult-xref
  :defer t
  :after (consult xref)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize
   consult-xref
   :preview-key
   (list :debounce 0.2 'any)))

(require-package 'consult-project-extra)
(use-package consult-project-extra
  :defer t
  :after consult
  :bind (("C-c p f" . consult-project-extra-find)
         ("C-c p o" . consult-project-extra-find-other-window)))

(require-package 'embark)
(use-package embark
  :defer t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(require-package 'embark-consult)
(use-package embark-consult
  :defer t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(require-package 'kind-icon)
(use-package kind-icon
  :defer t
  :after corfu
  :custom
  ;; Use icons in the completion list.
  (kind-icon-use-icons t)
  ;; Set the default face for the icons.
  (kind-icon-default-face 'corfu-default)
  ;; Don't blend the background of the icons.
  (kind-icon-blend-background nil)
  ;; Set the blend fraction.
  (kind-icon-blend-frac 0.08)
  :config
  ;; Add the kind-icon margin formatter to Corfu.
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Enable recursive minibuffers.
(setq enable-recursive-minibuffers t)
(require-package 'savehist)
(use-package savehist
  :defer t
  :init
  (savehist-mode))

(provide 'pkg-corfu2)
