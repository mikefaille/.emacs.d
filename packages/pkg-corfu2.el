(require 'use-package)
(require 'orderless)
(require 'completion-preview);; Corfu for Completion

(use-package all-the-icons
  :if (display-graphic-p))
(use-package corfu
:ensure t
:after cape
:custom
  (corfu-cycle t)
  (corfu-auto t)              ; Auto completion ON
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay '(0.5. 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'none)     ; Don't preselect anything
  (corfu-on-exact-match nil)  ; Don't complete on exact match
:bind (:map corfu-map
              ("M-p". corfu-previous)
              ("M-n". corfu-next)
              ("M-d". corfu-info-documentation)
              ("<return>". newline-and-indent)
              ("RET". newline-and-indent)
              ("<tab>". corfu-insert-dedicated); Dedicated insert key
              ("TAB". corfu-insert-dedicated))
:init
  (global-corfu-mode)
  (global-completion-preview-mode));; Corfu Popup Info (Optional)
(use-package corfu-popupinfo
:ensure nil
:after corfu
:hook (corfu-mode. corfu-popupinfo-mode)
:custom
  (corfu-popupinfo-delay '(0.5. 0.2)));; Corfu Echo (Optional)

(use-package corfu-echo
:ensure nil; Not in MELPA by default
:after corfu
:hook (corfu-mode. corfu-echo-mode)
:custom
   (corfu-doc-border "#FFB6C1")        ;; Customize border color (light pink)
   (corfu-doc-max-width 60)            ;; Adjust documentation width
 (corfu-doc-position 'window)
  (corfu-echo-documentation t)
  (corfu-echo-delay 0.5)
  (corfu-echo-max-width 60))
(use-package cape
:ensure t
:init
 (setq completion-at-point-functions
        (list 
              (cape-capf-buster #'cape-lsp) ; Apply capf-buster to cape-lsp
              #'cape-file
              #'cape-keywords
              #'cape-dabbrev
              #'cape-abbrev
              #'cape-history
              #'cape-line
              #'cape-symbols
              #'cape-dict))
  (setq cape-dabbrev-min-length 2
        cape-dabbrev-check-other-buffers 'some)


:bind (("C-c p t". complete-tag)



       ("C-c p d". cape-dabbrev)
         ("C-c p h". cape-history)
         ("C-c p s". cape-symbol)
         ("C-c p a". cape-abbrev)
         ("C-c p i". cape-ispell)
         ("C-c p l". cape-line)
         ("C-c p w". cape-dict)));; Orderless - Flexible Completion
(use-package orderless
:ensure t
:custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles. (partial-completion orderless)))
                                  (command (styles orderless-prefixes orderless))
                                  (variable (styles orderless-prefixes orderless))
                                  (symbol (styles orderless-prefixes orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex))
:config
  (setq orderless-style-dispatchers '(orderless-affix-dispatch))
  (setq completion-category-defaults nil));; Vertico - Vertical Completion UI (Optional, but recommended)
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
          (consult-location buffer))));; Persist history




;; Vertico Directory (Optional)
(use-package vertico-directory
:after vertico
:ensure nil
:bind (:map vertico-map
              ("RET". vertico-directory-enter)
              ("DEL". vertico-directory-delete-char)
              ("M-DEL". vertico-directory-delete-word))
:hook (rfn-eshadow-update-overlay. vertico-directory-tidy));; Marginalia - Annotations (Optional)
(use-package marginalia
:ensure t
:init (marginalia-mode)
:custom
  (marginalia-align 'right)
  (marginalia-max-relative-age 0));; Consult - Enhanced Searching (Optional, but highly recommended)
(use-package consult
:ensure t
:bind (("C-s". consult-line)
         ("C-x b". consult-buffer)
         ("M-y". consult-yank-pop)
         ("M-g g". consult-goto-line)
         ("C-x C-r". consult-recent-file)
         ("C-c i". consult-imenu)
         ("C-c m". consult-mark)
         ("M-s h". consult-history)
         ("M-s m". consult-mode-command)
         ("C-x r b". consult-bookmark)
         ("C-c k". consult-kmacro))
:custom
  (consult-preview-key 'any)
  (consult-narrow-key "<")
:config
  (when (executable-find "rg")
    (global-set-key (kbd "M-s r") #'consult-ripgrep)
    (setq consult-ripgrep-args
          (concat "rg --null --line-buffered --color=ansi --max-columns=1000 "
                  "--no-heading --line-number. -H --no-ignore --hidden "
                  "-g!.git")))
  (setq consult-async-min-input 2)
  (setq consult-async-refresh-delay 0.15)
  (setq consult-async-input-throttle 0.2)
  (setq consult-async-input-debounce 0.1));; Consult Xref (Optional)
(use-package consult-xref
:ensure nil
:after (consult xref)
:config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize
   consult-xref
 :preview-key
   (list:debounce 0.2 'any)));; consult-project-extra (Optional)
(use-package consult-project-extra
:ensure t
:after consult
:bind (("C-c p f". consult-project-extra-find)
         ("C-c p o". consult-project-extra-find-other-window)));; Embark - Contextual Actions (Optional, but recommended)
(use-package embark
:ensure t
:bind (("C-.". embark-act)
         ("M-.". embark-dwim)
         ("C-h B". embark-bindings))
:init
  (setq prefix-help-command #'embark-prefix-help-command));; Embark Consult (Integration)
(use-package embark-consult
:ensure t
:after (embark consult)
:hook
  (embark-collect-mode. consult-preview-at-point-mode));; Kind Icon (Optional)
(use-package kind-icon
:ensure t
:after corfu
:custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
:config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter));; Recursive minibuffers and savehist
(setq enable-recursive-minibuffers t)
(use-package savehist
   :init
   (savehist-mode))

(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)


  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(provide 'pkg-corfu2);;; pkg-corfu2.el ends here
