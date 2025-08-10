;; -*- lexical-binding: t; -*-
;; https://kristofferbalintona.me/posts/202202270056/


;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;     :bind
;;   (:map corfu-map
;;         ("TAB" . corfu-next)
;;         ([tab] . corfu-next)
;;         ("S-TAB" . corfu-previous)
;;         ([backtab] . corfu-previous))


;;   ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
;;   ;; be used globally (M-/).  See also the customization variable
;;   ;; `global-corfu-modes' to exclude certain modes.
;;   :init
;;   (global-corfu-mode)

;;   )


;; ;; Cape: Completion at point enhancements
;; (use-package cape
;;   :init
;;   ;; Set the order of completion functions, prioritizing LSP
;;   (setq completion-at-point-functions '(lsp-completion-at-point cape-dabbrev cape-file cape-keyword))
;;   (global-set-key (kbd "<tab>") 'completion-at-point))


;; (defun corfu-enable-in-minibuffer ()
;;   "Enable Corfu in the minibuffer."
;;   (when (local-variable-p 'completion-at-point-functions)
;;     ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
;;     (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
;;                 corfu-popupinfo-delay nil)
;;     (corfu-mode 1)))
;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;; ;; Use Dabbrev with Corfu!
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   :config
;;   (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
;;   ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
;;   (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
;;   (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))



;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; TAB cycle if there are only few candidates
;;   ;; (setq completion-cycle-threshold 3)

;;   ;; Enable indentation+completion using the TAB key.
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   (setq tab-always-indent 'complete)


;;   ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
;;   ;; try `cape-dict'.
;;   (setq text-mode-ispell-word-completion nil)

;;   ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
;;   ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
;;   ;; setting is useful beyond Corfu.
;;   (setq read-extended-command-predicate #'command-completion-default-include-p))

;; (use-package nerd-icons-corfu
;;   :ensure t
;;   :config
;;   (setq nerd-icons-corfu-mapping
;;         '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
;;           (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
;;           ;; ... Add more mappings as needed ...
;;           (t :style "cod" :icon "code" :face font-lock-warning-face)))
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; ;; Orderless: Flexible completion styles
;; (use-package orderless
;;   :custom
;;   (completion-styles '(orderless partial-completion basic)))


;; (use-package prescient
;;   :ensure t
;;   :config
;;   (prescient-persist-mode 1)) ;; Persist history across sessions



;; ;; LSP Mode: Core LSP support with refined integration
;; (use-package lsp-mode
;;   :custom
;;   (lsp-completion-provider :none)
;;   :init
;;   (defun my/lsp-mode-setup-completion ()
;;     ;; Prioritize LSP and set up orderless for LSP completion
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))
;;     ;; Use local completion functions list, placing LSP first
;;     (setq-local completion-at-point-functions '(lsp-completion-at-point cape-dabbrev cape-file cape-keyword)))
;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion))

;; ;; (use-package lsp-mode
;; ;;   :custom
;; ;;   (lsp-completion-provider :none) ;; we use Corfu!

;; ;;   :init
;; ;;   (defun my/orderless-dispatch-flex-first (_pattern index _total)
;; ;;     (and (eq index 0) 'orderless-flex))

;; ;;   (defun my/lsp-mode-setup-completion ()
;; ;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;; ;;           '(orderless))
;; ;;     ;; Optionally configure the first word as flex filtered.
;; ;;     (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
;; ;;     ;; Optionally configure the cape-capf-buster.
;; ;;     (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

;; ;;   :hook
;; ;;   (lsp-completion-mode . my/lsp-mode-setup-completion))

;; (setq tab-always-indent 'complete)
;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-use-icons t)                       ; Enable icons
;;   (kind-icon-default-face 'corfu-default)       ; Match Corfu's background
;;   (kind-icon-blend-background nil)              ; Disable background blending
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; ;; (use-package kind-icon
;; ;;   :after corfu
;; ;;   :custom
;; ;;   (kind-icon-use-icons t)
;; ;;   (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
;; ;;   (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
;; ;;   (kind-icon-blend-frac 0.08)

;; ;;   ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
;; ;;   ;; directory that defaults to the `user-emacs-directory'. Here, I change that
;; ;;   ;; directory to a location appropriate to `no-littering' conventions, a
;; ;;   ;; package which moves directories of other packages to sane locations.
;; ;;   (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
;; ;;   :config
;; ;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

;; ;;   ;; Add hook to reset cache so the icon colors match my theme
;; ;;   ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
;; ;;   ;; the theme using my custom defined command for switching themes. If I don't
;; ;;   ;; do this, then the backgound color will remain the same, meaning it will not
;; ;;   ;; match the background color corresponding to the current theme. Important
;; ;;   ;; since I have a light theme and dark theme I switch between. This has no
;; ;;  ;; function unless you use something similar
;; ;;   (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache)))
;; ;;   )

;; (use-package nerd-icons-corfu ; Optional: Nerd icons integration
;;   :ensure t)
;; (setq-local completion-at-point-functions '(orderless-completion-at-point prescient-completion-at-point cape-keyword cape-dabbrev cape-file))
;; Embark Setup
(use-package embark
  :ensure t
  :init
  (setq embark-quit-after-action t))  ; Automatically close Embark after an action is performed

;; Custom Embark Functions for Corfu Integration
(defun my-embark-act-corfu ()
  (interactive)
  (embark-act))

(defun embark-act-after-completion ()
  (when corfu--selected                ; Check if a candidate is selected
    (my-embark-act-corfu)))            ; Trigger Embark actions if a candidate is selected


;; Corfu Setup with Embark Integration
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preselect 'prompt)
  :hook
  ((corfu-mode . (lambda ()
                   (setq-local corfu-use-mouse t)
                   (advice-add 'corfu-insert :after #'embark-act-after-completion)))
   )
  :init
  (global-corfu-mode))


;; Cape: Completion at point enhancements
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions 'cape-keyword) ; Add keywords to completion
  (add-to-list 'completion-at-point-functions 'cape-file)    ; Add file paths to completion
  )

;; Enable Corfu in the minibuffer
(defun corfu-enable-in-minibuffer ()
  (when (local-variable-p 'completion-at-point-functions)
    (setq-local corfu-echo-delay nil    ; Make Corfu more responsive in minibuffer
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)


;; Dabbrev: Abbreviation expansion for enhanced completion
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))


;; Orderless: Flexible completion styles
(use-package orderless
  :custom
  (completion-styles '(orderless)))    ; Only use orderless style for better sorting


;; Prescient: Persistent completion history
(use-package prescient
  :config
  (prescient-persist-mode 1))

;; LSP Mode: Core LSP support with refined integration
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ; Let Corfu handle completion


;; Kind Icon: Add icons to Corfu
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))




;; Nerd Icons Corfu (Optional): Use Nerd Font icons
(use-package nerd-icons-corfu
  :ensure t
  :config
  (setq nerd-icons-corfu-mapping
        '((variable :icon "nf-cod-variable")
          (function :icon "nf-cod-function")
          (keyword :icon "nf-cod-keyword")
          (class :icon "nf-cod-class")
          (module :icon "nf-cod-package")
          (t :icon "nf-mdi-code_braces"))) ;; Default for other types
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Completion-at-point setup:
(setq completion-at-point-functions
      '(lsp-completion-at-point
	cape-keyword              ; Complete keywords from programming modes
	cape-dabbrev              ; Use dabbrev for better suggestions
        cape-file                 ; Complete file names

        ))




(setq tab-always-indent 'complete)  ; Prioritize completion over indentation

(provide 'pkg-corfu)