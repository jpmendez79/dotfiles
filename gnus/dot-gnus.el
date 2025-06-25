
;; Personal Information
(setq user-full-name "Jesse Mendez")
(setq user-mail-address "jmend46@lsu.edu")
(require 'ebdb-gnus)
(require 'ebdb-message)
(setq ebdb-message-auto-update-p 'query)
(setq ebdb-gnus-auto-update-p 'query)

;; Gnus Servers
;; (require 'gnus-desktop-notify)
;; (gnus-desktop-notify-mode)
;; (require 'gnus-demon)
;; (gnus-demon-add-handler 'gnus-demon-scan-news 1 t)
(setq gnus-select-method '(nnnil ""))
(setq gnus-desktop-notify-groups 'gnus-desktop-notify-explicit)

(setq gnus-select-method '(nnimap "Mail"
                              (nnimap-stream shell)
                              (nnimap-shell-program "/usr/libexec/dovecot/imap -o mail_location=maildir:~//Mail:LAYOUT=fs")))

(setq nnmail-expiry-target 'nnmail-fancy-expiry-target
       nnmail-fancy-expiry-targets
       '((to-from "jmend46@lsu.edu" "nnimap:Mail/lsu/Trash")
         (to-from "jessepmendez79@gmail.com" "nnimap:Mail/personal/[Gmail]/Trash")))
;; Posting Styles and Replies         
(setq gnus-posting-styles
      '(("Personal" ; Matches Gnus group called "Personal"
         (address "Jesse Mendez <jessepmendez79@gmail.com>")
	 	 ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 jessepmendez79@gmail.com")
	 )
        ("LSU" ; Matches Gnus group called "LSU"
         (address "Jesse Mendez <jmend46@lsu.edu")
	 (signature-file "~/.signature.lsu")
	 ("X-Message-SMTP-Method" "smtp localhost 1025 jmend46@lsu.edu"))
	))
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

;; Gnus Register
(setq gnus-registry-max-entries 2500)
(setq gnus-refer-article-method
      '(nnregistry))
(gnus-registry-initialize)

;; Message Mode
(setq message-fill-column nil)
(add-hook 'message-mode-hook 'flyspell-mode)

;; Prefer Plaintext
(setq mm-discouraged-alternatives
      '("text/html" "text/richtext"))

;; EBDB Popup Window
