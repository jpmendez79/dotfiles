;; Loads config while saving this for... stuff.
;; Place this in init.el
;; (load-file "~/.emacs.d/config.el")

;; Package Manager and Use package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(require 'plstore)
(add-to-list 'load-path "~/.emacs.d/org-gantt-master")
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Application specific settings
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

   (setq browse-url-browser-function 'browse-url-generic
	 browse-url-generic-program "/usr/bin/firefox-bin")

(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  (message "WSL")
  (setq visible-bell       nil
	ring-bell-function #'ignore)
  ;; Change the font size
  ;; (add-to-list 'default-frame-alist
  ;; 	       '(font . "DejaVu Sans Mono-18"))
  (setq
   browse-url-generic-program  "~/.local/bin/wsl-browse.sh"
   browse-url-browser-function #'browse-url-generic)


  )


;; Defun Section
(defun my-org-hook ()
  (flyspell-mode 1)
  (org-fragtog-mode 1)
  (visual-line-mode 1)
  (fira-code-mode 1)
  )
(defun my-ledger-hook ()
  (setq-local tab-always-indent 'complete)
  (setq-local completion-cycle-threshold t)
  (setq-local ledger-complete-in-steps t)
  (fira-code-mode 1)
  )

(defun my-c-mode-common-hook ()
  (c-toggle-auto-newline 1)
  (display-line-numbers-mode)
  ;; (flycheck-prog-mode 1)
  )

(defun shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun print-buffer-to-pdf ()
  "Save the current buffer as a PDF."
  (interactive)
  (let* ((ps-file "/tmp/emacs-buffer.ps")
         (pdf-file (concat (file-name-base (or (buffer-file-name) "untitled")) ".pdf")))
    (ps-print-buffer ps-file)  ; Generate a PostScript file.
    (shell-command (concat "ps2pdf " ps-file " " pdf-file))  ; Convert to PDF.
    (delete-file ps-file)  ; Clean up.
    (message "PDF saved as %s" pdf-file)))


;; Look and feel
;; (require 'notifications)
(fringe-mode)
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'fringe))
(display-time-mode 1)
(display-battery-mode 1)
(column-number-mode 1)
(windmove-default-keybindings)
;; (add-to-list 'default-frame-alist
;;              '(font . "DejaVu Sans Mono-11"))

(use-package fira-code-mode
  :config
  (fira-code-mode-set-font)
  :hook prog-mode
  ;; (global-fira-code-mode)
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  )


;; Save File
(setq delete-old-versions t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Images in dired
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

;; Eshell stuff
(require 'eshell)
(add-to-list 'eshell-modules-list 'eshell-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(with-eval-after-load 'esh-mode
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  )
;; Personal Info and PIM Settings
(setq user-full-name "Jesse Mendez"
      user-mail-address "student@school.edu")
(setq compose-mail-user-agent-warnings nil)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq calendar-latitude 30.4)
(setq calendar-longitude -91.18)
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)

;; Utilities and Tools
;; Programming Settings

;; C stuff
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))
(setq-default c-electric-flag t)

(setq c-toggle-electric-state 1)
(add-hook 'c-mode-hook 'c-toggle-auto-newline 1)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Python stuff
(add-hook 'python-mode-hook 'eglot-ensure)

(use-package rainbow-mode
  :ensure t
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

(use-package envrc
  :hook (after-init . envrc-global-mode))

;; (use-package detached
;;   :init
;;   (detached-init)
;;   :bind
;;   (;; Replace `async-shell-command' with `detached-shell-command'
;;    ([remap async-shell-command] . detached-shell-command)
;;    ;; Replace `compile' with `detached-compile'
;;    ([remap compile] . detached-compile)
;;    ([remap recompile] . detached-compile-recompile)
;;    ;; Replace built in completion of sessions with `consult'
;;    ([remap detached-open-session] . detached-consult-session))
;;   ("C-c m v" . detached-view-session)
;;   ("C-c m a" . detached-attach-session)
;;   ("C-c m =" . detached-diff-session)
;;   ("C-c m c" . detached-compile-session)
;;   ("C-c m r" . detached-rerun-session)
;;   ("C-c m i" . detached-insert-session-command)
;;   ("C-c m w" . detached-copy-session-command)
;;   ("C-c m W" . detached-copy-session-output)
;;   ("C-c m k" . detached-kill-session)
;;   ("C-c m d" . detached-delete-session)
  
;;   :custom ((detached-show-output-on-attach t)
;;            (detached-terminal-data-command system-type)))


;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Setup EBDB for contacts
(use-package ebdb
  :config
  (setq ebdb-mua-auto-update-p 'query)
  (setq ebdb-gnus-auto-update-p 'query)
  (setq edbd-default-window-size 0.2)
  (setq ebdb-sources "~/Dropbox/org/ebdb")
  (require 'ebdb-message)
  (require 'ebdb-gnus)
  (require 'ebdb-org))



(use-package org-caldav
  :ensure t
  :config
  (require 'org-caldav)
  (setq org-caldav-url "http://localhost:1080/users")
  (setq org-caldav-calendar-id "student@school.edu/Calendar")
  (setq org-caldav-files nil)
  (setq org-caldav-inbox "~/Dropbox/org/cal_caldav.org")
  (setq org-caldav-uuid-extension ".EML")
  (setq org-caldav-save-directory "~/Dropbox/org/org-caldav/")
  (setq org-icalendar-timezone "US/Central")
  (setq org-caldav-calendars
  '((:calendar-id "student@school.edu/Calendar"
     :inbox "~/Dropbox/org/cal_school.org")
    (:calendar-id "student@school.edu/calendar/Personal"
     :inbox "~/Dropbox/org/cal_personal.org")) )
  )



(use-package calfw
  :ensure t)

(use-package calfw-org
  :ensure t
  ;; :bind
  ;; ("M-<f3>" . cfw:open-org-calendar)
  :config
  ;; hotfix: incorrect time range display
  ;; source: https://github.com/zemaye/emacs-calfw/commit/3d17649c545423d919fd3bb9de2efe6dfff210fe
  )

;; Tex and Latex Settings
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(add-hook 'TeX-mode-hook #'eglot-ensure)

(use-package pdf-tools
  :ensure f
  :magic ("%PDF" . pdf-view-mode)
  :pin manual ;; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (require 'pdf-occur)
  (pdf-tools-install :no-query))


;; Nov.el File associations
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Org Mode and various org packages
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (python . t)
     (plantuml . t)
     ))
  :hook (org-mode . my-org-hook)
  :custom
  (org-agenda-timegrid-use-ampm t)
  (org-agenda-include-diary t)
  (org-stuck-projects
   '("+Project-someday+LEVEL=1/-DONE-CANCELED-someday" ("NEXT" "WAITING")))
  (org-todo-keywords
   '((sequence "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-capture-templates
   '(
     ("a" "Capture an Appointment")
     ("ap" "Personal Calendar Appointment" entry (file  "~/Dropbox/org/cal_calendar.org" )
      "* %?\n\n:PROPERTIES:\n\n:END:\n\n")
     ("as" "School Calendar Appointment" entry (file  "~/Dropbox/org/cal_school.org" )
      "* %?\n\n:PROPERTIES:\n\n:END:\n\n")
     ("i" "Capture an idea to inbox" entry (file "~/Dropbox/org/inbox.org") "* %?\n")
     ("n" "Capture a next item" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks") "* NEXT %?%^G\n")
     ("j" "Journal" entry (file+datetree "~/Dropbox/org/physics_research.org") 
"** %^{Heading}" :jump-to-captured t :tree-type week) 
     ))
  (org-directory "~/Dropbox/org")
  (org-agenda-custom-commands 
   '(
     ("c" "Computer" tags-todo "@computer-someday|@laptop-someday")
     ("e" "Errands" tags-todo "@errand-someday")
     ("p" "Phone" tags-todo "@phone-someday")
     ("o" "Internet" tags-todo "@online-someday")
     ("h" "Home" tags-todo "@home-someday")
     ("l" "LSU Campus" tags-todo "@campus-someday")
     ("s" "LArASIC Lab" tags-todo "@larasic-someday")
     ("r" "Research" tags-todo "@research-someday")
     ("w" "Homework" tags-todo "@homework-someday")
     ("m" "Personal Computer mercury" tags-todo "@mercury-someday")
     ("b" "Work Computer bortan" tags-todo "@bortan-someday")
     ("g" "Agendas" tags-todo "@agenda-someday")
     ("W" "Weekly Review"
      ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
       (tags "inbox")
       (tags-todo "-someday-inbox")					  ; type "l" in the agenda to review logged items
       (stuck "") ; review stuck projects as designated by org-stuck-projects
       (tags "Project-someday+LEVEL=1") ; review all projects (assuming you use todo keywords to designate projects)
       (tags-todo "-someday+TODO=\"WAITING\"")

       (tags "someday+LEVEL=2")))))
  :config
  (setq org-agenda-files '("~/Dropbox/org/inbox.org"
			   "~/Dropbox/org/project.org"
			   "~/Dropbox/org/gtd.org"
			   "~/Dropbox/org/cal_calendar.org"))

  (setq org-refile-targets '((nil :maxlevel . 9)
			     ("~/Dropbox/org/someday.org" :maxlevel . 9)
			     ("~/Dropbox/org/gtd.org" :maxlevel . 3)
			     ("~/Dropbox/org/project.org" :maxlevel . 9)
			     ("~/Dropbox/org/cal_calendar.org" :maxlevel . 9)))
  ;; Looks
  (setq-default org-startup-indented t
		org-pretty-entities t
		org-use-sub-superscripts "{}"
		org-hide-emphasis-markers t
		org-startup-with-inline-images t
		org-image-actual-width '(300))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes t)					; Show full paths for refiling
  (setq org-outline-path-complete-in-steps nil)
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-plantuml-executable-path "/usr/bin/plantuml")

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-c." 'org-time-stamp)
  (global-set-key "\C-cp" 'org-pomodoro)
  (global-set-key "\C-co" 'org-noter)
  (global-set-key "\C-cu" 'org-reset-checkbox-state-subtree)

  (setq org-tags-exclude-from-inheritance "project")

  (setq org-todo-keywords
	'((sequence "NEXT(n)" "|" "DONE(d)" "Delegated(D)")
	  (sequence "WAITING(w)" "APPT(a)" )
	  (sequence "|" "CANCELED(c)")))
  (require 'ox-beamer)
  (require 'ox-latex)
  (setq org-export-allow-bind-keywords t)
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"))
  (setq org-latex-minted-options '(("breaklines" "true")
				   ("breakanywhere" "true")))
  (setq org-latex-pdf-process
	(mapcar
	 (lambda (s)
	   (replace-regexp-in-string "%latex " "%latex -shell-escape " s))
	 org-latex-pdf-process))
  )


(use-package org-crypt
  :ensure nil
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  )

;; Managing Bibliographies
(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t))

(use-package biblio)

(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil))
;; LaTeX previews
(use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Org-gantt
(require 'org-gantt)
;; Citar to access bibliographies
(use-package citar
  :custom
  (org-cite-global-bibliography
   (directory-files "~/Dropbox/Library/" t
		    "^[A-Z|a-z|0-9].+.bib$"))
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths '("~/Dropbox/Library"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind
  (("C-c w b o" . citar-open)
   (:map org-mode-map
         :package org
         ("C-c w C". #'org-cite-insert))))


(use-package org-noter
  :ensure t
  )

(use-package denote
  :ensure t
  :init
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (denote-rename-buffer-mode 1)
  :custom
  (denote-directory "~/Dropbox/denote")
  (denote-sort-keywords t)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c d n" . denote-create-note)
   ("C-c d j" . denote-journal-extras-new-entry)
   ("C-c d d" . denote-date)
   ("C-c d i" . denote-link-or-create)
   ("C-c d l" . denote-find-link)
   ("C-c d b" . denote-find-backlink)
   ("C-c d r" . denote-rename-file)
   ("C-c d R" . denote-rename-file-using-front-matter)
   ("C-c d k" . denote-keywords-add)
   ("C-c d K" . denote-keywords-remove))
  )

(use-package citar-denote
  :config
  (citar-denote-mode)
  :custom
  (citar-open-always-create-notes t)
  (citar-notes-paths '("~/Dropbox/denote/"))
  
  :bind (("C-c b n" . citar-create-note)
         ("C-c b o" . citar-denote-open-note)
         ("C-c b f" . citar-denote-find-citation)
         ("C-c b d" . citar-denote-dwim)
         ("C-c b e" . citar-denote-open-reference-entry)
         ("C-c b a" . citar-denote-add-citekey)
         ("C-c b k" . citar-denote-remove-citekey)
         ("C-c b r" . citar-denote-find-reference)
         ("C-c b l" . citar-denote-link-reference)
         ("C-c b x" . citar-denote-nocite)
         ("C-c b y" . citar-denote-cite-nocite)))

;; Denote Explore
(use-package denote-explore
    :custom
    ;; Where to store network data and in which format
    (denote-explore-network-directory "~/Dropbox/denote/viz/")
    (denote-explore-network-filename "denote-network")
    (denote-explore-network-format 'graphviz)
    (denote-explore-network-graphviz-filetype "pdf")
    :bind
    (;; Statistics
     ("C-c w e c" . denote-explore-count-notes)
     ("C-c w e C" . denote-explore-count-keywords)
     ("C-c w e b" . denote-explore-keywords-barchart)
     ("C-c w e x" . denote-explore-extensions-barchart)
     ;; Random walks
     ("C-c w e r" . denote-explore-random-note)
     ("C-c w e l" . denote-explore-random-link)
     ("C-c w e k" . denote-explore-random-keyword)
     ;; Denote Janitor
     ("C-c w e d" . denote-explore-identify-duplicate-notes)
     ("C-c w e z" . denote-explore-zero-keywords)
     ("C-c w e s" . denote-explore-single-keywords)
     ("C-c w e o" . denote-explore-sort-keywords)
     ("C-c w e r" . denote-explore-rename-keywords)
     ;; Visualise denote
     ("C-c w e n" . denote-explore-network)
     ("C-c w e v" . denote-explore-network-regenerate)
     ("C-c w e D" . denote-explore-degree-barchart)))

;; Denote extensions
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d ,"~/Dropbox/denote")))
  :bind
  (("C-c n f" . consult-notes)
   ("C-c n s" . consult-notes-search-in-all-notes)))

;; Eglot Server
(use-package eglot
  :ensure t
  :config
  (setq lsp-tex-server 'digestif)
  )


(use-package slack
  :bind (("C-c s K" . slack-stop)
         ("C-c s c" . slack-select-rooms)
         ("C-c s u" . slack-select-unread-rooms)
         ("C-c s U" . slack-user-select)
         ("C-c s s" . slack-search-from-messages)
         ("C-c s J" . slack-jump-to-browser)
         ("C-c s j" . slack-jump-to-app)
         ("C-c s e" . slack-insert-emoji)
         ("C-c s E" . slack-message-edit)
         ("C-c s r" . slack-message-add-reaction)
         ("C-c s t" . slack-thread-show-or-create)
         ("C-c s g" . slack-message-redisplay)
         ("C-c s G" . slack-conversations-list-update-quick)
         ("C-c s q" . slack-quote-and-reply)
         ("C-c s Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer)))
	 
         )
  :custom
  (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  
  :config
  (setq slack-buffer-emojify t)
  (slack-register-team
     :name "Microboone"
     :token (auth-source-pick-first-password
             :host "microboone.slack.com"
             :user "jmend46@lsu.edu")
     :cookie (auth-source-pick-first-password
             :host "microboone.slack.com"
             :user "jmend46@lsu.edu^cookie")
     :full-and-display-names t
     :default t
     :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
     ))

;; Ledger mode
(use-package ledger-mode
  :hook (ledger-mode . my-ledger-hook)
  )

;; use-package with package.el:
(use-package dashboard
  :ensure t
  :config
  (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
  (setq dashboard-match-agenda-entry "-tag")
  (dashboard-setup-startup-hook))

;; Remote Profile variables
;; Remote Tramp PATH
;; (add-to-list 'tramp-default-remote-path 'tramp-own-remote-path)
(use-package tramp
    :config
    (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Connection Variables
(connection-local-set-profile-variables
 'remote-detached-gpvm
 '((detached-shell-program . "/bin/bash")
   (detached-session-directory . "/path/to/somewhere/.dtach-session")
   (detached-dtach-program . "/path/to/somewhere/.bin/dtach")))
(connection-local-set-profile-variables
 'remote-bash
 '((shell-file-name . "/bin/bash")
   (shell-command-switch . "-c")
   (shell-login-switch . "-l")))
(connection-local-set-profile-variables
 'remote-root-profile
 '((explicit-shell-file-name . "/bin/bash")
   (explicit-bash-args . ("--login" "-i"))))






;; (connection-local-set-profiles
;;  '(:machine "ub1")
;;  'remote-detached-gpvm)
;; (connection-local-set-profiles
;;  '(:machine "ub2") 'remote-detached-gpvm)
;; (connection-local-set-profiles
;;  '(:machine "ub3") 'remote-detached-gpvm)
;; (connection-local-set-profiles
;;  '(:machine "ub4") 'remote-detached-gpvm)
;; (connection-local-set-profiles
;;  '(:machine "ub5") 'remote-detached-gpvm)
;; (connection-local-set-profiles
;;  '(:machine "ub6") 'remote-detached-gpvm)
;; (connection-local-set-profiles
;;  '(:machine "uboonepro") 'remote-detached-gpvm)
(connection-local-set-profiles
 '(:machine "pg8") 'remote-root-profile)

(server-start)

