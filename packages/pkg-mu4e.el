(require-package 'mu4e-maildirs-extension)
(require 'mu4e)
(require 'mu4e-contrib)
(require 'smtpmail)

(setq mu4e-maildir       "~/.mail/gmail")   ;; top-level Maildir

(setq mu4e-html2text-command 'mu4e-shr2text
      mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder   "/[Gmail].Sent Mail"
      mu4e-trash-folder  "/[Gmail].Trash"
      mu4e-decryption-policy t
      mu4e-get-mail-command "mbsync gmail || [ $? -eq 1 ]"
      mu4e-headers-auto-update t
      mu4e-update-interval 300
      mu4e-headers-fields
      '((:human-date . 12)
        (:flags . 6)
        (:from . 22)
        (:subject))
      mu4e-use-fancy-chars t
      mu4e-headers-include-related t
      mu4e-user-mail-address-list
      '("michael.faille.1@ens.etsmtl.ca" "michael@faille.io" "michael.faille@gmail.com")
      )


(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))


(setq mu4e-maildir-shortcuts
      '( ("/Inbox"               . ?i)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].All Mail"    . ?a)))

;; something about ourselves
(setq
 user-mail-address "michael.faille.1@ens.etsmtl.ca"
 user-full-name  "Michaël Faille"
 mu4e-reply-to-address "michael.faille.io"
 mu4e-compose-signature
 (concat
  "---\n"
  "Michaël Faille\n"
  "Étudiant au baccalauréat en génie des technologies de l'information\n"
  "Université du Québec - École de technologie supérieure, Montréal (Québec)\n"
  ))

;;alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-auth-credentials
    '(("smtp.gmail.com" 587 "michael.faille@gmail.com" nil))
    smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)


;; show images
(setq mu4e-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (set-fill-column 72)
            (flyspell-mode)))

   ;;; message view action
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))


(setq mu4e-view-prefer-html t)
(setq mail-user-agent 'mu4e-user-agent)



(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(provide 'pkg-mu4e)
