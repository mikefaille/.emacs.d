;;; core-packages.el --- 

;; Copyright © 2011-2014 Bozhidar Batsov <bozhidar@batsov.com>
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/projectile
;; Keywords: project, convenience
;; Version: 0.11.0
;; Package-Requires: ((s "1.6.0") (f "0.17.1") (dash "1.5.0") (pkg-info "0.4"))
;; This file is NOT part of GNU Emacs.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;; Commentary:
;;
;; This library provides easy project management and navigation. The
;; concept of a project is pretty basic - just a folder containing
;; special file. Currently git, mercurial and bazaar repos are
;; considered projects by default. If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it. See the README for more details.
;;
;;; Code:





(require 'package)
(package-initialize)

(require 'use-package)
(defvar core-modules '(better-defaults smartparens idle-highlight-mode ido-ubiquitous find-file-in-project magit smex scpaste color-theme-solarized helm flycheck undo-tree dired-hacks-utils  yasnippet flycheck malabar-mode company company-go multiple-cursors go-mode go-autocomplete  emacs-eclim eshell-prompt-extras projectile fuzzy cl-lib deferred jedi ein ) "A list of modules to ensure are installed at launch.")

(defvar-local enabled-modules '(nil) "A list of modules to ensure are installed at launch.")

(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))



; activate all the packages (in particular autoloads)





(unless package-archive-contents
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  
  )


(defun require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package core-modules)
    (add-to-list 'core-modules package))
  (unless (package-installed-p package)
    (package-install package)))

(defun module-list-install (modules-list)
  ; install the missing packages
  (dolist (module modules-list )
                                        ;
    (unless (package-installed-p module)
      (package-refresh-contents)      
      (package-install module))))
  ;; (if (not (package-installed-p 'use-package))
  ;;     (progn
  ;;       (package-refresh-contents)
  ;;       (package-install 'use-package))))
;



;; load git package
(let ((base "~/.emacs.d/git-packages"))
 ; (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name f))
      (when (and (file-directory-p name) 
                 (not (equal f ".."))
                 (not (equal f ".")))
        (normal-top-level-add-subdirs-to-load-path)))))







(provide 'core-packages)
;;; core-packages.el ends here
