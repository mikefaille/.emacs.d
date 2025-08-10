;; -*- lexical-binding: t; -*-
(use-package magit
  :config
  ;; Remove obsolete hooks before enabling wip-mode
  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpulled-from-pushremote
          magit-insert-unpulled-from-upstream
          magit-insert-unpushed-to-pushremote
          magit-insert-unpushed-to-upstream))
  (magit-wip-mode 1))


(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (with-eval-after-load 'git-gutter
    (setq git-gutter:update-interval 0.02)))

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


(provide 'pkg-git)