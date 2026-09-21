;; Personal Information
(setq user-full-name "Jesse Mendez")
(setq user-mail-address "jmend46@lsu.edu")
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(eval-after-load 'gnus-topic
  '(progn
     (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
     (setq gnus-topic-topology '(("Gnus" visible)
                                 (("Louisiana State University" visible nil nil))
                                 (("Gmail" visible nil nil))))

     ;; key of topic is specified in my sample ".gnus.el"
     (setq gnus-topic-alist '(("Louisiana State University" ; the key of topic
                               "lsu/Inbox"
                               "lsu/Sent"
                               "lsu/Drafts"
                               "lsu/Trash")
                              ("Gmail" ; the key of topic
                               "personal/Inbox"
                               "personal/Sent Mail"
                               "personal/All Mail"
                               "personal/Drafts"
                               "personal/Trash")

                              ("Gnus")))))

;; Threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
;; Also, I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

(setq gnus-select-method '(nnimap "Mail"
                                  (nnimap-stream shell)
                                  (nnimap-shell-program "/usr/libexec/dovecot/imap -o mail_driver=maildir -o mail_path=~/.mail -o mailbox_list_layout=fs")))


;; Posting Styles and Replies
(setq gnus-posting-styles
      '(("gmail"
         (address "Jesse Mendez <jessepmendez79@gmail.com>")
         ("X-Message-SMTP-Method"
          "smtp smtp.gmail.com 587 jessepmendez79@gmail.com"))
        ("lsu"
         (address "Jesse Mendez <jmend46@lsu.edu>")
         (signature-file "~/.signature-lsu.html")
         ("X-Message-SMTP-Method"
          "smtp localhost 1025 jmend46@lsu.edu"))))
(setq message-dont-reply-to-names
      '("jmend46@lsu.edu"
        "jessepmendez79@gmail.com"))

(define-key message-mode-map (kbd "C-<tab>") 'mail-abbrev-complete-alias)

;; SMTP Servers
(setq send-mail-function 'sendmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

;; Auth-source pass
(auth-source-pass-enable)
(setq auth-sources '(password-store))
(setq auth-source-do-cache nil)
;; Gnus Register
(setq gnus-registry-max-entries 2500)
(setq gnus-refer-article-method
      '(nnregistry))
(gnus-registry-initialize)

;; Message Mode
(setq message-fill-column nil)
(add-hook 'message-mode-hook 'flyspell-mode)
(add-hook 'message-mode-hook 'visual-line-mode)



(require 'ebdb-gnus)
(require 'ebdb-message)
(ebdb-insinuate-gnus)
(add-hook 'message-mode-hook 'ebdb-complete-enable)

;; ebdb Popup Window
(gnus-add-configuration
 '(article
   (horizontal 1.0
	           (vertical 25
			             (group 1.0))
	           (vertical 1.0
			             (summary 0.25 point)
			             (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
	           (vertical 25
			             (group 1.0))
	           (vertical 1.0
			             (summary 1.0 point)))))

(require 'mbsync)
(add-hook 'mbsync-exit-hook 'gnus-group-get-new-news)
(define-key gnus-group-mode-map (kbd "f") 'mbsync)
(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
(gnus-demon-add-rescan)
(setq gnus-desktop-notify-groups 'gnus-desktop-notify-explicit)
