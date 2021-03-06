;; License:
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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
;;; Code:
;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway

;; http://www.emacswiki.org/emacs/AnsiColor
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Escape-sequences-in-shell-output.html
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

(when (fboundp 'tool-bar-mode)
(tool-bar-mode -1))
(menu-bar-mode -1)
;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
;; disable startup screen
(setq inhibit-startup-screen t)
;; nice scrolling
(setq scroll-margin 0
scroll-conservatively 100000
scroll-preserve-screen-position 1)
;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
(fringe-mode 4))
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
'("" invocation-name " eMikes - " (:eval (if (buffer-file-name)
(abbreviate-file-name (buffer-file-name))
"%b"))))
;; use zenburn as the default theme
;;(load-theme prelude-theme t)

;; http://stackoverflow.com/questions/812135/emacs-modes-command-attempted-to-use-minibuffer-while-in-minibuffer
(setq enable-recursive-minibuffers t)

(setq uniquify-buffer-name-style (quote post-forward-angle-brackets))
(setq uniquify-ignore-buffers-re "^\\*")
(setq uniquify-min-dir-content 1)

;; String separator for buffer name components. Hide When
;; `uniquify-buffer-name-style' is `post-forward', separates base file name from
;; directory part in buffer names (default "|").  When
;; `uniquify-buffer-name-style' is `reverse', separates all file name components
;; (default "\").
;;
;; Defaults to nil
(setq uniquify-separator "/")


(provide 'core-look)
;;; prelude-ui.el ends here
