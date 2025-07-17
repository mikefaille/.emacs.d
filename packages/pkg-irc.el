;;; pkg-irc.el --- ERC configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This package configures ERC, the Emacs IRC client.

;;; Code:

(use-package erc
  :defer t
  :config
  ;; Import necessary packages for ERC.
  (require 'auth-source)
  (require 'erc-log)
  (require 'erc-notify)
  (require 'erc-spelling)
  (require 'erc-autoaway)

  ;; ERC Configuration

  ;; General settings.
  (setq erc-modules '(autoaway autojoin button completion fill irccontrols keep-place
                    list log match menu move-to-prompt netsplit networks noncommands
                    notify notifications readonly ring stamp track)
        erc-log-channels-directory "~/.erc/logs/"
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t)

  ;; Create logging directory if not exists.
  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t))

  ;; Autoaway settings.
  (setq erc-auto-discard-away t
        erc-autoaway-idle-seconds 600
        erc-autoaway-use-emacs-idle t)

  ;; Tracking settings.
  (require 'erc-track)
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")
        erc-track-showcount t
        erc-track-position-in-mode-line nil)

  ;; Match mode settings.
  (require 'erc-match)
  (setq erc-keywords '("RyanRix" "dongiverse" "\\brix\\b" "\\bPSA\\b" "\\b@here\\b" "\\b@all\\b")
        erc-pals '("aviau")
        erc-fools '("Froward"))
  (erc-match-mode 1)
  (remove-hook 'erc-text-matched-hook 'erc-hide-fools)

  ;; Function to reset tracking.
  (defun reset-erc-track-mode ()
    "Clear the ERC tracking list."
    (interactive)
    (setq erc-modified-channels-alist nil)
    (erc-modified-channels-update))
  (global-set-key (kbd "C-c r") 'reset-erc-track-mode)

  ;; Function to connect to Freenode.
  (defun chat/freenode ()
    "Connect to the Freenode network."
    (interactive)
    (erc-tls :server "faille.io"
             :port 6969
             :nick "darkmike"
             :password (get-auth-password "social.faille.io" "michael.faille@gmail.com" "6969")))

  ;; Function to connect to Pirate.
  (defun chat/pirate ()
    "Connect to the Pirate network."
    (interactive)
    (erc-tls :server "faille.io"
             :port 6969
             :nick "darkmike"
             :password (get-auth-password "social.faille.io" "michael.faille@gmail.com" "6969")))

  ;; Function to retrieve password from auth-source.
  (defun get-auth-password (machine login port)
    "Retrieve password from auth-source."
    (funcall (plist-get (car (auth-source-search :machine machine :login login :port port)) :secret))))

(provide 'pkg-irc)
