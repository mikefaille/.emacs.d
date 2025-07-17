;;; pkg-git.el --- Git integration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures Git integration with Magit, git-gutter, and
;; git-gutter-fringe.

;;; Code:

(require-package 'magit)
(use-package magit
  :defer t
  :config
  ;; Remove obsolete hooks before enabling wip-mode.
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
  ;; Enable work-in-progress mode.
  (magit-wip-mode 1))

(require-package 'git-gutter)
(use-package git-gutter
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :config
  ;; Set the update interval for git-gutter.
  (setq git-gutter:update-interval 0.02))

(require-package 'git-gutter-fringe)
(use-package git-gutter-fringe
  :defer t
  :config
  ;; Define the fringe bitmaps for git-gutter.
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
