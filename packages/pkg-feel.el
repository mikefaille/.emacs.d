;; diminish keeps the modeline tidy
(require-package 'diminish)
(require-package 'minimap)

(defvar main-savefile-dir
  (concat user-emacs-directory
          (convert-standard-filename "savefile")) )


(require-package 'smartparens)
(autoloadp 'smartparens-config)
(smartparens-global-mode)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(show-smartparens-global-mode +1)

;; super ido
(require-package 'flx-ido)
(flx-ido-mode 1)

(require-package 'flx-isearch)
(flx-isearch-mode)
(global-set-key (kbd "C-M-s") #'flx-isearch-forward)
(global-set-key (kbd "C-M-r") #'flx-isearch-backward)


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" main-savefile-dir))


;; activate it for all buffers
(setq-default save-place t)


;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables

      ;; search entries
      '(search-ring regexp-search-ring)

      ;; save every minute
      savehist-autosave-interval 60

      ;; keep the home clean
      savehist-file (expand-file-name "savehist" main-savefile-dir))
(savehist-mode +1)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" main-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15

      ;; disable recentf-cleanup on Emacs start, because it can cause

      ;; problems with remote files
      recentf-auto-cleanup 'never)
(defun main-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list main-savefile-dir package-user-dir)))))
(add-to-list 'recentf-exclude 'main-recentf-exclude-p)


;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(recentf-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

(defcustom prelude-auto-save t
"Non-nil values enable Prelude's auto save."
:type 'boolean
:group 'prelude)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun main-auto-save-command ()
  "Save the current buffer if `main-auto-save' is not nil."
  (when (and main-auto-save
             buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))
(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))


;; advise all window switching functions
(advise-commands "auto-save"
                 (switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right)
                 before
                 (main-auto-save-command))
(add-hook 'mouse-leave-buffer-hook 'main-auto-save-command)
(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'main-auto-save-command))
(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; highlight the current line
(global-hl-line-mode +1)
(require-package 'volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; tramp, for sudo access
(require 'tramp)

;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")
(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(defun main-enable-flyspell ()
  "Enable command `flyspell-mode' if `main-flyspell' is not nil."
  (when (and main-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))
(defun main-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `main-clean-whitespace-on-save' is not nil."
  (when main-clean-whitespace-on-save
    (whitespace-cleanup)))
(defun main-enable-whitespace ()
  "Enable `whitespace-mode' if `main-whitespace' is not nil."
  (when main-whitespace

    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'main-cleanup-maybe nil t)
    (whitespace-mode +1)))
(add-hook 'text-mode-hook 'main-enable-flyspell)
(add-hook 'text-mode-hook 'main-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)
(require-package 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" main-savefile-dir)
      bookmark-save-flag 1)

;; anzu-mode enhances isearch & query-replace by showing total matches and current match position
(require-package 'anzu)

(anzu-mode +1)

(diminish 'anzu-mode)
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
(require-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)
(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))
(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) main-yank-indent-threshold)
      (indent-region beg end nil)))
(advise-commands "indent" (yank yank-pop) after
                 "If current mode is one of `main-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
                 (if (and (not (ad-get-arg 0))
                          (not (member major-mode main-indent-sensitive-modes))
                          (or (derived-mode-p 'prog-mode)
                              (member major-mode main-yank-indent-modes)))
                     (let ((transient-mark-mode nil))
                       (yank-advised-indent-function (region-beginning) (region-end)))))

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(require 'whitespace)

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)
(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" main-savefile-dir))
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" main-savefile-dir))

;; Compilation from Emacs
(defun main-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)

  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))
(require 'compile)
(setq compilation-ask-about-save nil ; Just save before compiling
      compilation-always-kill t ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'main-colorize-compilation-buffer)

;; ;; enable Main's keybindings
;; (main-global-mode t)

;; sensible undo
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; enable winner-mode to manage window configurations
(winner-mode +1)

;; ;; diff-hl
;; (global-diff-hl-mode +1)
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)


;; easy-kill
(require-package 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; operate-on-number
(require-package 'operate-on-number)

(require-package 'smartrep)
(require 'smartrep)
(smartrep-define-key global-map "C-c ."
'(("+" . apply-operation-to-number-at-point)
("-" . apply-operation-to-number-at-point)
("*" . apply-operation-to-number-at-point)
("/" . apply-operation-to-number-at-point)
("\\" . apply-operation-to-number-at-point)
("^" . apply-operation-to-number-at-point)
("<" . apply-operation-to-number-at-point)
(">" . apply-operation-to-number-at-point)
("#" . apply-operation-to-number-at-point)
("%" . apply-operation-to-number-at-point)
("'" . operate-on-number-at-point)))
(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
"Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'. So you may wish to open filename in that format.
Just call:
emacsclient filename:linenumber
and file 'filename' will be opened and cursor set on line 'linenumber'"
(ad-set-arg 0
(mapcar (lambda (fn)
(let ((name (car fn)))
(if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
(cons
(match-string 1 name)
(cons (string-to-number (match-string 2 name))
(string-to-number (or (match-string 3 name) ""))))
fn))) files)))


(provide 'pkg-feel)
