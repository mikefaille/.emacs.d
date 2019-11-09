;;; core-packages.el ---

;; Copyright © 2015 Michael Faille <michael@faille.io>
;; Author:
;; URL: https://github.com/mikefaille/.emacs.d
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

;; TODO make it async
(when (not package-archive-contents)
  (package-refresh-contents t))

(defvar core-modules '(smartparens idle-highlight-mode  find-file-in-project  smex scpaste  helm flycheck undo-tree dired-hacks-utils   flycheck   eshell-prompt-extras fuzzy deferred auto-async-byte-compile markdown-mode async ) "A list of modules to ensure are installed at launch.")

(defvar-local enabled-modules '(nil) "A list of modules to ensure are installed at launch.")

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


; activate all the packages (in particular autoloads)
(package-initialize)


(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."

  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents t)
        (require-package package min-version t)))))


(unless package-archive-contents
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")

  )



(defun module-list-install (modules-list)
  ; install the missing packages
  (dolist (module modules-list )
      (require-package module)
;      (package-install module)


))
(module-list-install core-modules)


(defun add-subfolders-to-load-path (parent-dir)
  "Add subfolders to load path"
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(defun add-subfolders-to-load-path2 (parent-dir)
(let ((base parent-dir))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name))))))


(defvar git-packages-dir
(concat user-emacs-directory
        (convert-standard-filename "git-packages") ) )


(add-subfolders-to-load-path2 (expand-file-name git-packages-dir))







(provide 'core-packages)
;;; core-packages.el ends here
