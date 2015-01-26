;;; package --- Suma auto-complete

;;; Commentary:
(require-package 'auto-complete)

;;; Code:

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete) ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(setq completion-cycle-threshold 5)


;; hook AC into completion-at-point
(defun sanityinc/auto-complete-at-point ()
  (when (and (not (minibufferp))
             (fboundp 'auto-complete-mode)
             auto-complete-mode)
    (auto-complete)))



(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


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



;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(require-package 'jedi)

;; auto-complete latex
(require-package 'auto-complete-auctex)

(auto-complete-mode t)
(ac-set-trigger-key "TAB")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(autoloadp 'auto-complete-config)
(ac-config-default)
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



(setq ac-auto-show-menu 0.6)
(setq ac-use-comphist t)
(setq ac-use-fuzzy t)


;(setq ac-source-yasnippet nil)

(defun add-yasnippet-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))


(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(provide 'pkg-ac-complete)
;;; ac-complete ends here
