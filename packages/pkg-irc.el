;; (require-package 'znc)
(require 'auth-source)
(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)


(setq erc-modules
  (quote
   (autoaway autojoin button completion fill irccontrols keep-place list log match menu move-to-prompt netsplit networks noncommands notify notifications readonly ring stamp track)))

'(erc-notify-list (quote ("aviau")))

;; ;; :password  (funcall
;; ;;                  (plist-get
;; ;;                   (nth 0 (auth-source-search :machine "social.faille.io" :login "michael.faille@gmail.com" :port "6969"))
;; ;;                   :secret))


(erc-scrolltobottom-mode 1)

;; autoaway setup
(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 600)
(setq erc-autoaway-use-emacs-idle t)


(require 'erc-track)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-showcount t)
(setq erc-track-position-in-mode-line nil)

;; Code bottom suppose to permit me to track only on erc buffer...
;; ^ TODO dont work
;;(erc-track-mode t)

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")
(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

;; (erc-spelling-mode 1)
;; (setq erc-spelling-dictionaries '(("#emacs" "american" "french")))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)



;; This should work, but if erc-track-position-in-mode-line is set to nil, erc-track simply no-ops the update callback which is … nonoptimal. For now, I jank it and define my own without the no-op, here

(defun erc-modified-channels-display ()
  "Set `erc-modified-channels-object'
according to `erc-modified-channels-alist'.
Use `erc-make-mode-line-buffer-name' to create buttons."
  (cond ((or (eq 'mostactive erc-track-switch-direction)
         (eq 'leastactive erc-track-switch-direction))
     (erc-track-sort-by-activest))
    ((eq 'importance erc-track-switch-direction)
     (erc-track-sort-by-importance)))
  (run-hooks 'erc-track-list-changed-hook)
  (let* ((oldobject erc-modified-channels-object)
     (strings
      (when erc-modified-channels-alist
        ;; erc-modified-channels-alist contains all the data we need.  To
        ;; better understand what is going on, we split things up into
        ;; four lists: BUFFERS, COUNTS, SHORT-NAMES, and FACES.  These
        ;; four lists we use to create a new
        ;; `erc-modified-channels-object' using
        ;; `erc-make-mode-line-buffer-name'.
        (let* ((buffers (mapcar 'car erc-modified-channels-alist))
           (counts (mapcar 'cadr erc-modified-channels-alist))
           (faces (mapcar 'cddr erc-modified-channels-alist))
           (long-names (mapcar #'(lambda (buf)
                       (or (buffer-name buf)
                           ""))
                       buffers))
           (short-names (if (functionp erc-track-shorten-function)
                    (funcall erc-track-shorten-function
                         long-names)
                  long-names))
           strings)
          (while buffers
        (when (car short-names)
            (setq strings (cons (erc-make-mode-line-buffer-name
                     (car short-names)
                     (car buffers)
                     (car faces)
                     (car counts))
                    strings)))
          (setq short-names (cdr short-names)
            buffers (cdr buffers)
            counts (cdr counts)
            faces (cdr faces)))
        strings)))
       (newobject (erc-modified-channels-object strings)))
      (unless (equal oldobject newobject)
    (setq erc-modified-channels-object newobject)
    (force-mode-line-update t))))

;; I don't have erc-track-mode set to render on all of my buffers, you'll notice, since I set erc-track-position-in-mode-line to nil. That is because I only want to render it on ERC buffers, of which I always have one open when I am working. This keeps my mode-line relatively uncluttered, which is nice. I need to figure out how to do something similar with my Org mode clocking state. I define an erc-mode-hook that renders the tracker only on that buffer's modeline:

(defun mikefaille/erc-track-here ()
  "Add the ERC tracker to the current buffer's modeline"
  (add-to-list 'mode-line-format '(t erc-modified-channels-object) t))
(add-hook 'erc-mode-hook 'mikefaille/erc-track-here)


;; image on irc buffer
(require-package 'erc-image)
(require 'erc-image)
(erc-image-mode)


;; erc-social-graph is a neat package that graphs the relationships between people.

(require-package 'erc-social-graph)
(require 'erc-social-graph)
(erc-social-graph-enable)









;; ERC has four classes of match-able highlights:

;; keywords: Things that you want to receive notifications about
;; pals: friends
;; fools: frenemies. I don't like /ignore myself so this is kind of nice to have, I guess.
;; dangerous-hosts: I'm not sure quite what the deal is with this; I guess you could use it to note trolls and their IPs, but most of those twits just use proxies or somesuch.
(require 'erc-match)
(erc-match-mode 1)
(setq erc-keywords '("RyanRix" "dongiverse" "\\brix\\b" "\\bPSA\\b" "\\b@here\\b" "\\b@all\\b")
      erc-pals     '("aviau")
      erc-fools    '("Froward"))
(remove-hook 'erc-text-matched-hook 'erc-hide-fools)




;; The upshot of having these, is that you can do some magic with them; erc-track-score combines erc-match and erc-track to give buffers a score that will decay over time; if people are talking about things you care about, if it's people you like, the buffer becomes "hotter", which is just that the decay function is being prevented by activity you care about. Conversely, erc-fools and dangerous-hosts will pull the buffer towards cold.
(require-package 'erc-track-score)
(require 'erc-track-score)
(setq erc-track-score-reset-every 120)
(erc-track-score-mode 1)
(setq erc-track-switch-direction 'mostactive)

;; C-c r to clear the tracking list.
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist ())
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)


;; Configure ERC filling; this sets a wrap on the buffers so that the text doesn't look super janky.
(setq erc-fill-function 'erc-fill-static
      erc-fill-column   110
      erc-fill-static-center 14
      erc-fill-prefix 8)


(require-package 'erc-hl-nicks)
;; (erc-hl-nicks-enable)


;; (setq erc-prompt (lambda () (concat "-" (buffer-name) "->")))


(defun chat/freenode ()
  "Connect to ZNC.
Prompt for password first."
  (interactive)

  (erc-tls :server "social.faille.io"
           :port 6969
           :nick "darkmike"
           :password (concat "michael/freenode:" (funcall
                                                  (plist-get
                                                   (nth 0 (auth-source-search :machine "social.faille.io" :login "michael.faille@gmail.com" :port "6969"))
                                                   :secret)) )
           )

  )


(defun chat/pirate ()
  "Connect to ZNC.
Prompt for password first."
  (interactive)


  (erc-tls :server "social.faille.io"
           :port 6969
           :nick "darkmike"
           :password (concat "michael/freenode:" (funcall
                                                  (plist-get
                                                   (nth 0 (auth-source-search :machine "social.faille.io" :login "michael.faille@gmail.com" :port "6969"))
                                                   :secret)) )
           )
  )

;; (erc-tls :server "social.faille.io"
;;          :port 6969
;;          :nick "darkmike"
;;          :password (concat "michael/pirate:" (funcall
;;                                                 (plist-get
;;                                                  (nth 0 (auth-source-search :machine "social.faille.io" :login "michael.faille@gmail.com" :port "6969"))
;;                                                  :secret)) )
;;          )


;; (erc-tls :server "social.faille.io"
;;          :port 6969
;;          :nick "darkmike"
;;          :password "michael/freenode:passsss")

;; (erc-tls :server "social.faille.io"
;;          :port 6969
;;          :nick "darkmike"
;;          :password "michael/pirate:passss")

;;          ;; :password (concat "michael/freenode:" (funcall
;;          ;;                               (plist-get
;;          ;;                                (nth 0 (auth-source-search :machine "social.faille.io" :login "michael.faille@gmail.com" :port "6969"))
;;          ;;                                :secret)) )



;; (erc-tls :server "montreal-ca.pirateirc.net"
;;          :port 6697
;;          :nick "darkmike"
;;          )



;; (setq znc-servers nil)
;; ;; ;; (defconst *znc-server-type* `(group (string  :tag "Host" :value ,*znc-server-default-host*)
;; ;; ;;                                     (integer :tag "Port" :value ,*znc-server-default-port*)
;; ;; ;;                                     (boolean :tag "SSL"  :value nil)
;; ;; ;;                                     (repeat :tag "Accounts on server" ,@*znc-server-accounts-type*))
;; ;; ;;   "A group describing a ZNC server endpoint and the accounts on it")


;; (setq znc-servers `(("social.faille.io" 6969 nil
;;                      ((freenode ,"darkmike"  (funcall
;;                                              (plist-get
;;                                               (nth 0 (auth-source-search :machine "social.faille.io" :login "michael.faille@gmail.com" :port "6969"))
;;                                               :secret))))) ) )
;; (znc-all)



;; (defun mike/znc ()
;;   "Connect to ZNC.
;; Prompt for password first."
;;   (interactive)






;;   )


;; (global-set-key "\C-ci"  'mike/znc)
(provide 'pkg-irc)
