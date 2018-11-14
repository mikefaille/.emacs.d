(require-package 'gitconfig-mode)
(require-package 'magit)
(require 'magit-wip)
(require-package 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; ;; If you would like to use git-gutter.el and linum-mode
;; (git-gutter:linum-setup)

;; If you enable git-gutter-mode for some modes
(add-hook 'prog-mode-hook 'git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter-mode)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)



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
(add-hook 'prog-mode-hook 'magit-wip-after-save-local-mode)
(add-hook 'text-mode-hook 'magit-wip-after-save-local-mode)

(provide 'pkg-git)
