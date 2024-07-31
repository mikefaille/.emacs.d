(use-package magit)


(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(defun git-add-files(files)
  "Run git add with the input file"
  (interactive)
  (shell-command (format "git add %s" files)))

(defun dired-git-add-marked-files()
  "For each marked file in a dired buffer add it to the index"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((filenames (dired-get-marked-files))
	    (files ""))
	(dolist (fn filenames)
	  (setq fn (shell-quote-argument fn))
	  (setq files (concat files " " fn)))
	(git-add-files files))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))


;; ;; see here https://magit.vc/manual/magit/Wip-Modes.html
;; (add-hook 'prog-mode-hook 'magit-wip-mode)
;; (add-hook 'text-mode-hook 'magit-wip-mode)

(provide 'pkg-git)
