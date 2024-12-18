;; package --- Suma auto-complete

;;; Commentary:

(require-package 'auto-complete)
(require-package 'ac-capf)
(ac-capf-setup)

(ac-config-default)

(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;;; Code:


(add-hook 'prog-mode-hook (lambda()

			    ;; auto-complete latex
			    (require-package 'auto-complete-auctex)

			    (ac-set-trigger-key "TAB")
			    (ac-set-trigger-key "<tab>")
			    (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

			    ;; (setq-default
			    ;;  ac-sources
			    ;;  '(
			    ;;    ac-source-yasnippet
			    ;;    ac-source-imenu
			    ;;    ac-source-abbrev
			    ;;    ac-source-words-in-same-mode-buffers
			    ;;    ac-source-files-in-current-dir
			    ;;    ac-source-filename
			    ;;    ac-source-semantic

			    ;;    )
			    ;; )

			    ;;(setq ac-auto-start 1)
			    ;;(setq ac-auto-show-menu 7)
			    (setq ac-use-comphist t)
			    (setq ac-use-fuzzy t)


			    ;; (defadvice ac-fallback-command (around no-yasnippet-fallback activate)
			    ;;   (let ((yas-fallback-behavior nil))
			    ;;     ad-do-it))




			    (defun add-yasnippet-ac-sources ()
			      (add-to-list 'ac-sources 'ac-source-yasnippet))

			    (if (featurep 'yasnippet)
				(setq-default ac-sources (push 'ac-source-yasnippet ac-sources)))

					;(setq ac-source-yasnippet nil)


			    (setq-default ac-expand-on-auto-complete nil)
			    (setq-default ac-auto-start nil)
			    (setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

			    ;;----------------------------------------------------------------------------
			    ;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
			    ;;----------------------------------------------------------------------------
			    (setq tab-always-indent 'complete) ;; use 't when auto-complete is disabled
			    (add-to-list 'completion-styles 'initials t)

			    ;; Stop completion-at-point from popping up completion buffers so eagerly
			    (setq completion-cycle-threshold 5)

			    ;; TODO: find solution for php, haskell and other modes where TAB always does something
			    ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/C-Indent.html
			    (setq c-tab-always-indent nil
				  c-insert-tab-function 'indent-for-tab-command)

			    ;; hook AC into completion-at-point
			    (defun sanityinc/auto-complete-at-point ()
			      (when (and (not (minibufferp))
					 (fboundp 'auto-complete-mode)
					 auto-complete-mode)
				(auto-complete)))

			    (defun sanityinc/never-indent ()
			      (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

			    (defun set-auto-complete-as-completion-at-point-function ()
			      (setq completion-at-point-functions
				    (cons 'sanityinc/auto-complete-at-point
					  (remove 'sanityinc/auto-complete-at-point completion-at-point-functions))))

			    (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
			    (set-default 'ac-sources
					 '(ac-source-imenu
					   ac-source-dictionary
					   ac-source-words-in-buffer
					   ac-source-words-in-same-mode-buffers
					   ac-source-words-in-all-buffer))
			    (dolist (mode '(magit-log-edit-mode eshell-mode
								log-edit-mode org-mode text-mode haml-mode
								git-commit-mode
								sass-mode yaml-mode csv-mode espresso-mode haskell-mode
								html-mode nxml-mode sh-mode smarty-mode clojure-mode
								lisp-mode textile-mode markdown-mode tuareg-mode
								js3-mode css-mode less-css-mode sql-mode
								sql-interactive-mode
								inferior-emacs-lisp-mode))
			      (add-to-list 'ac-modes mode))

			    ;; Exclude very large buffers from dabbrev
			    (defun sanityinc/dabbrev-friend-buffer (other-buffer)


			      (< (buffer-size other-buffer) (* 1 1024 1024)))

			    (setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)


			    ))


(provide 'pkg-ac-complete)
;;; ac-complete ends here
