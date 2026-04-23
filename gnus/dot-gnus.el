
;; Personal Information
(setq user-full-name "Jesse Mendez")
(setq user-mail-address "jmend46@lsu.edu")
;; (require 'ebdb-gnus)
;; (require 'ebdb-message)
;; (setq ebdb-message-auto-update-p 'query)
;; (setq ebdb-gnus-auto-update-p 'query)
;; (gnus-add-configuration
;;  '(article
;;    (vertical 1.0
;;      (summary 0.25 point)
;;      (horizontal 1.0
;;        (article 1.0)
;;        (buffer 0.4)))))

;; (gnus-add-configuration
;;  '(article
;;    (vertical 1.0
;;              (summary 0.25 point)
;;              (horizontal 1.0
;;                          (article 1.0)
;;                          (ebdb-message 0.4)))))
;; Gnus Servers
(setq gnus-select-method '(nnnil ""))
;; (setq gnus-select-method '(nnimap "Mail"
;;                               (nnimap-stream shell)
;;                               (nnimap-shell-program "/usr/libexec/dovecot/imap -o mail_location=maildir:~/Mail:LAYOUT=fs")))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                      ;; press 'E' to expire email
                      (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                      (nnmail-expiry-wait 90)))
(setq gnus-verbose 9)
(setq nnimap-record-commands t)
;; OPTIONAL, the setup for Microsoft Hotmail
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "lsu"
                      (nnimap-address "127.0.0.1")
                      (nnimap-server-port 1143)
                      (nnimap-stream plain)
		      (nnimap-user "jmend46@lsu.edu")
		      (nnmail-expiry-target "nnimap+lsu:Trash")
		      (nnimap-authenticator login)))


(setq gnus-verbose 10
      nnimap-debug t)

;; (setq nnmail-expiry-target 'nnmail-fancy-expiry-target
;;        nnmail-fancy-expiry-targets
;;        '((to-from "jmend46@lsu.edu" "nnimap:Mail/lsu/Trash")
;;          (to-from "jessepmendez79@gmail.com" "nnimap:Mail/personal/[Gmail]/Trash")))
;; Posting Styles and Replies
(setq gnus-posting-styles
      '(("gmail"
         (address "Jesse Mendez <jessepmendez79@gmail.com>")
         ("X-Message-SMTP-Method"
          "smtp smtp.gmail.com 587 jessepmendez79@gmail.com"))
        ("lsu"
         (address "Jesse Mendez <jmend46@lsu.edu>")
         (signature-file "~/.signature.lsu")
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

;; Prefer Plaintext
(setq mm-discouraged-alternatives
      '("text/html" "text/richtext"))

;; EBDB Popup Window
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
;; (require 'gnus-desktop-notify)
;; (gnus-desktop-notify-mode)
;; (gnus-demon-add-rescan)
;; (setq gnus-desktop-notify-groups 'gnus-desktop-notify-explicit)
