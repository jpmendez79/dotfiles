;; Loads config while saving this for... stuff.
;; Place this in init.el
;; (load-file "~/.emacs.d/config.el")

;; Package Manager and Use package setup
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'org)
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

(require 'plstore)

(use-package pass
  :straight t)


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/firefox-bin")


(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  (message "WSL")
  (setq visible-bell       nil
	ring-bell-function #'ignore)
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

(defun my-denote-mode-hook ()
  (visible-mode)
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

(defun my-dired-init ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))

(defun ews--bibtex-combined-biblio-lookup ()
  "Combines `biblio-lookup' and `biblio-doi-insert-bibtex'."
  (require 'biblio)
  (let* ((dbs (biblio--named-backends))
         (db-list (append dbs '(("DOI" . biblio-doi-backend))))
         (db-selected (biblio-completing-read-alist
                       "Backend:"
                       db-list)))
    (if (eq db-selected 'biblio-doi-backend)
        (let ((doi (read-string "DOI: ")))
          (biblio-doi-insert-bibtex doi))
      (biblio-lookup db-selected))))



(defun ews-bibtex-biblio-lookup ()
  "Insert Biblio search results into current buffer or select BibTeX file."
  (interactive)
  (if-let ((current-mode major-mode)
	   org-cite-global-bibliography
	   (bibfiles (length org-cite-global-bibliography))
	   (bibfile (cond ((eq bibfiles 1) (car org-cite-global-bibliography))
			  ((equal major-mode 'bibtex-mode)
			   (buffer-file-name))
			  (t (completing-read
			      "Select BibTeX file:" org-cite-global-bibliography)))))
      (progn (find-file bibfile)
	     (goto-char (point-max))
	     (ews--bibtex-combined-biblio-lookup)
	     (save-buffer))
    (message "No BibTeX file(s) defined.")))

(defun kill-src-block-at-point ()
 (interactive)
 (kill-new (org-element-property :value (org-element-at-point))))

;; Calendar showing org-agenda entries
(defun my-open-calendar-agenda ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "medium purple"))
   :view 'block-week))

;; Calendar showing org entries from files
(defun my-open-calendar-files ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-file-source "Todos" "~/stuff/todos.org" "green")
    (cfw:org-create-file-source "Events" "~/stuff/events.org" "blue"))
   :view 'block-3-day))

;; (defun org-gcal-client-sync ()
;;   "Run on first start to retrieve and set org-gcal client from gpg encrypted authinfo. After use org-gcal-commands as n…"
;;   (setq org-gcal-client-id (auth-source-pick-first-password
;;    :user "org-gcal-client-id"))
;;   (setq org-gcal-client-secret (auth-source-pick-first-password
;;    :user "org-gcal-client-secret"))
;;   (org-gcal-reload-client-id-secret))

;; Look and feel
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode))

(fringe-mode)
(use-package alert
  :straight t
  :commands (alert)
  :init
  (setq alert-default-style 'notifications))

(display-time-mode 1)
(display-battery-mode 1)
(column-number-mode 1)
;; (windmove-default-keybindings)

(use-package fira-code-mode
  :straight t
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

;; Dired config
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-hook 'dired-mode-hook 'my-dired-init)
(setq dired-dwim-target t)

(use-package xterm-color
  :straight t
  )

;; Eshell stuff
(use-package esh-mode
  :ensure nil
  :config
)
(require 'esh-mode)
(require 'eshell)
(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(delq 'eshell-handle-ansi-color eshell-output-filter-functions)
(add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))
(setq xterm-color-use-bold-for-bright t)
(setenv "TERM" "rxvt-unicode-256color")
(add-to-list 'eshell-modules-list 'eshell-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
;; (with-eval-after-load 'esh-mode
;;   ;; Ensure xterm-color is used for processing ANSI sequences
;;   (add-hook 'eshell-before-prompt-hook
;;             (lambda ()
;;               (setq xterm-color-preserve-properties t)))
;;   (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;;   ;; Remove the default ANSI color handler
;;   (setq eshell-output-filter-functions
;;         (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))
(use-package vterm
  :straight t
  )  
;; Personal Info and PIM Settings
(setq user-full-name "Jesse Mendez"
      user-mail-address "jmend46@lsu.edu")
(setq compose-mail-user-agent-warnings nil)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq calendar-latitude 30.4)
(setq calendar-longitude -91.18)
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)

;; Utilities and Tools
;; Programming Settings
(require 'epa-file)
(epa-file-enable)
(setq org-crypt-key "jessepmendez79@gmail.com")
(setq epa-pinentry-mode 'loopback)

;; Tree-sitter
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c.git"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
     (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (csv . ("https://github.com/tree-sitter-grammars/tree-sitter-csv" "master" "csv/src"))
     (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
     (gpg-config . ("https://github.com/tree-sitter-grammars/tree-sitter-gpg-config.git"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html ppp"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua.git"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
     (properties . ("https://github.com/tree-sitter-grammars/tree-sitter-properties.git"))
     (ssh-config . ("https://github.com/tree-sitter-grammars/tree-sitter-ssh-config.git"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (udev . ("https://github.com/tree-sitter-grammars/tree-sitter-udev.git"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
     (xml . ("https://github.com/tree-sitter-grammars/tree-sitter-xml.git"))))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (c-mode . c-or-c++-ts-mode)
   (c++-mode . c-or-c++-ts-mode)))

(use-package rainbow-mode
  :straight t
  )

(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

;; Setup EBDB for contacts
(use-package ebdb
  :straight t
  :config
  (setq ebdb-mua-auto-update-p 'query)
  (setq ebdb-gnus-auto-update-p 'query)
  (setq edbd-default-window-size 0.2)
  (setq ebdb-sources "~/Dropbox/org/ebdb")
  (require 'ebdb-message)
  (require 'ebdb-gnus)
  (require 'ebdb-org))

(use-package org-caldav
  :straight t
  :config
  (require 'org-caldav)
  (setq org-caldav-url "http://localhost:1080/users")
  (setq org-caldav-calendar-id "jmend46@lsu.edu/Calendar")
  (setq org-caldav-files nil)
  (setq org-caldav-inbox "~/Dropbox/org/cal_caldav.org")
  (setq org-caldav-uuid-extension ".EML")
  (setq org-caldav-save-directory "~/Dropbox/org/org-caldav/")
  (setq org-icalendar-timezone "UTC")
  (setq org-caldav-calendars
	'((:calendar-id "jmend46@lsu.edu/Calendar"
			:inbox "~/Dropbox/org/cal_school.org")
	  (:calendar-id "jmend46@lsu.edu/calendar/Personal"
			:inbox "~/Dropbox/org/cal_personal.org")) )
  )

;; (use-package org-gcal
;;   :straight t
;;   ;; :init (org-gcal-client-sync)
;;   :config
;;   (setq org-gcal-fetch-file-alist '(("jessepmendez79@gmail.com" .  "~/Dropbox/org/cal_personal.org")))
;; 	)

;; Tex and Latex Settings
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)


(use-package auctex
  :straight t)

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  ;; :pin manual ;; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (require 'pdf-occur)
  (pdf-tools-install :no-query))

;; Yasnippets
(use-package yasnippet
  :straight t
  :config
  (add-to-list 'load-path
              "~/.emacs.d/snippets/")
  (yas-global-mode 1)
  )
;; Nov.el File associations
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Org Mode and various org packages
(use-package org
  :straight t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (python . t)
     (plantuml . t)
     (calc . t)
     ))
  (add-to-list 'org-modules 'org-habit)
  :hook ((org-mode . my-org-hook)
	 (org-agenda-mode . hl-line-mode))
  :custom
  (org-agenda-timegrid-use-ampm t)
  (org-agenda-include-diary t)
  (calendar-mark-diary-entries-flag t)
  (org-stuck-projects
   '("+Project-someday+LEVEL=1/-DONE-CANCELED-someday" ("NEXT" "WAITING")))
  (org-todo-keywords
   '((sequence "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-capture-templates
   '(
     ("a" "Capture an Appointment")
     ("ap" "Appointment" entry (file "~/Dropbox/cal_personal.org")
        "* %?\n:PROPERTIES:\n:calendar-id:\tjessepmendez79@gmail.com\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :jump-to-captured t)
     ("as" "School Calendar Appointment" entry (file  "~/Dropbox/org/cal_school.org" )
      "* %?\n\n:PROPERTIES:\n\n:END:\n\n")
     ("i" "Capture an idea to inbox" entry (file "~/Dropbox/org/inbox.org") "* %?\n")
     ("n" "Capture a next item" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks") "* NEXT %?%^G\n")
     ("j" "Journal" entry (function denote-journal-extras-new-or-existing-entry)
      "\n* %<%I:%M %p>\n%?" :jump-to-captured t :immediate-finish t)))
  (org-directory "~/Dropbox/org")
  (org-agenda-custom-commands 
   '(
     ("b" "Work Computer bortan" tags-todo "@bortan-someday")
     ("c" "Computer (any)" tags-todo "@computer-someday")
     ("d" "Reading List" tags-todo "@read-someday")
     ("e" "Errand" tags-todo "@errand-someday")
     ("h" "Home" tags-todo "@home-someday")
     ("l" "Lab" tags-todo "@lab-someday")
     ("m" "Personal Computer milo" tags-todo "@milo-someday")
     ("n" "Administrative Tasks" tags-todo "@admin-someday")
     ("o" "Office" tags-todo "@office-someday")
     ("p" "Phone" tags-todo "@phone-someday")
     ("r" "Research" tags-todo "@research-someday")
     ("s" "Brainstorm" tags-todo "@brainstorm-someday")
     ("f" "Offline" tags-todo "@offline-someday")
     ("W" "Weekly Review"
      ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
       (tags "inbox")
       (tags-todo "-someday-inbox")					  ; type "l" in the agenda to review logged items
       (stuck "") ; review stuck projects as designated by org-stuck-projects
       (tags "Project-someday+LEVEL=1") ; review all projects (assuming you use todo keywords to designate projects)
       (tags-todo "-someday+TODO=\"WAITING\"")

       (tags "someday+LEVEL=2")))))
  (org-agenda-files '("~/Dropbox/org/roam/projects/"
		      "~/Dropbox/org/roam/journals/"
		      "~/Dropbox/org/project.org"
		      "~/Dropbox/org/gtd.org"
		      "~/Dropbox/org/cal_school.org"
		      "~/Dropbox/org/cal_personal.org"))
  :config
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
  (add-to-list 'org-entities-user
      '(("nubar" "\\bar{\\nu}" nil "ν̅" "ν̅" "ν̅" "ν̅")))
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
  (global-set-key "\C-ck" 'kill-src-block-at-point)
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
  ;; (add-to-list 'org-latex-packages-alist '(("" "minted")))
					   
	       
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
  :straight t
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t))



(use-package helm-bibtex
  :straight t
  :config
  (global-set-key (kbd "C-c r r") 'helm-bibtex)
    ;; IMP: Ensure 'latexmk' installed as a system package!
  ;; see also: http://www.jonathanleroux.org/bibtex-mode.html
  (setq bibtex-completion-bibliography '("~/Dropbox/org/roam/ref/main.bib"))  ; location of .bib file containing bibliography entries
  (setq bibtex-completion-find-additional-pdfs t)                          ; support for multiple pdfs for one %citekey
  (setq bibtex-completion-pdf-field "File")                                ; in bib entry, file = {/path/to/file.pdf} could be set to locate the accompanying file
  ;; for multiple files use, file = {:/path/to/file0.pdf:PDF;:/path/to/file1.pdf:PDF}
  (setq bibtex-completion-library-path '("~/Dropbox/org/roam/ref/documents/"))  ; in this dir, %citekey-name(s).pdf would automatically attach pdf(s) to %citekey
  ;; if only !exist "file" field in bib entry
  (setq bibtex-completion-notes-path "~/Dropbox/org/roam/ref/")           ; dir to keep notes for the pdfs

  ;; BEGIN: Change insert citation (<f3>) behaviour of helm-bibtex for org-mode 
  (defun custom/bibtex-completion-format-citation-org (keys)
    "Custom cite definition for org-mode"
    (s-join ", "
	    (--map (format "cite:&%s" it) keys)))
  ;; END: Change insert citation (<f3>) behaviour of helm-bibtex for org-mode

(setq bibtex-autokey-year-length 4                          ; customisations for 'bibtex-generate-autokey'
      bibtex-autokey-name-year-separator "-"                ; press C-c C-c (bibtex-clean-entry) on a bib entry w/o %citekey
      bibtex-autokey-year-title-separator "-"               ; to automatically insert a %citekey based on meta data
      bibtex-autokey-titleword-separator "-"                ; use M-x crossref-add-bibtex-entry <ret>: to add an entry from
      bibtex-autokey-titlewords 2                           ; https://www.crossref.org/
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(setq bibtex-completion-format-citation-functions
      '((org-mode      . custom/bibtex-completion-format-citation-org)
	(latex-mode    . bibtex-completion-format-citation-cite)
	(markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	(python-mode   . bibtex-completion-format-citation-sphinxcontrib-bibtex)
	(rst-mode      . bibtex-completion-format-citation-sphinxcontrib-bibtex)
	(default       . bibtex-completion-format-citation-default))))

(use-package biblio
  :straight t
  :bind
  ("C-c b b" . ews-bibtex-biblio-lookup)
  )

;; LaTeX previews
(use-package org-fragtog
  :straight t
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

;; Citar to access bibliographies
(use-package citar
  :straight t
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
  (("C-c w b" . citar-open)
   (:map org-mode-map
         :package org
         ("C-c w C". #'org-cite-insert))))


(use-package nov
  :straight t)
(use-package org-roam
  :straight t
  :bind
  (("C-c r d" . org-roam-dailies-capture-today)
   ("C-c r f" . org-roam-node-find)
   ("C-c r i" . org-roam-node-insert))
  :config
  (cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
  (setq org-roam-directory "~/Dropbox/org/roam")
  (setq org-roam-dailies-directory "journals/")
  (setq org-roam-file-exclude-regexp "\\.st[^/]*\\|logseq/.*$")
  ;; ensure org-roam is creating nodes similarly to Logseq
;; bear in mind that it won't be exact mapping due to Logseq's built-in
;;    :file/name-format :triple-lowbar
  (setq org-roam-capture-templates '(("d" "default"
                                      plain
                                      "%?"
                                      :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
				     ("b" "bibliography notes" plain             ; Org-noter integration
				      (file "~/Dropbox/org/roam/ref/notes-template.org")
				      :target (file+head "ref/${citekey}.org"
							 "#+title: ${title}\n")
				      :empty-lines 1)))

;; ensure your org-roam daily template follows the journal settings in Logseq
;;    :journal/page-title-format "yyyy-MM-dd"
;;    :journal/file-name-format "yyyy_MM_dd"
(setq org-roam-dailies-capture-templates '(("d" "default"
                                            entry
                                            "* %?"
                                            :target (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n"))))
(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
#("${type:15} ${title:*} ${tags:10}" 22 32 (face org-tag)))


(use-package org-roam-ui
  :straight t)
(use-package consult-org-roam
   :straight t
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key "M-.")
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c r e" . consult-org-roam-file-find)
   ("C-c r b" . consult-org-roam-backlinks)
   ("C-c r B" . consult-org-roam-backlinks-recursive)
   ("C-c r l" . consult-org-roam-forward-links)
   ("C-c r s" . consult-org-roam-search))

(use-package org-roam-bibtex
  :after org-roam
  :straight t
  :config
  ;(setq bibtex-completion-edit-notes-function 'bibtex-completion-edit-notes-default) ; default to org-ref for notes
  (setq bibtex-completion-edit-notes-function 'orb-bibtex-completion-edit-note) ; use org-roam-capture-templates for notes
  )

(use-package org-noter
  :straight t
  :config
  (setq org-noter-notes-search-path '("~/Dropbox/org/roam/ref/")) ; V IMPORTANT: SET FULL PATH!

  (setq orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file") ; customisation for notes, org-noter integration
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf")))


(use-package prog-mode
  :hook (prog-mode . display-line-numbers-mode))

;; Eglot Server
(use-package eglot
  :ensure t
  :config
  (setq lsp-tex-server 'digestif)
  )

(use-package flymake
  :hook (prog-mode . flymake-mode))

;; C stuff
(use-package c-ts-mode
  :hook (c-or-c++-ts-mode . eglot-ensure)
  :config
  (setq c-toggle-auto-newline 1)
  (setq c-toggle-electric-state 1)
  (setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))
(setq-default c-electric-flag t))

;; Other

(use-package yaml-ts-mode
    :mode "\\.yaml\\'")


(use-package lisp-mode
  :hook (lisp-mode . electric-pair-mode))


;; (add-hook '

;; (use-package weechat
;;   :straight t
;;   :config
;;   (require 'rx)
;;   )



(use-package slack
  :straight (:host github :repo "emacs-slack/emacs-slack")
  ;; :hook (slack-mode-hook . emojify-mode)
  :bind (("C-c s K" . slack-stop)
         ("C-c s c" . slack-select-rooms)
         ("C-c s u" . slack-select-unread-rooms)
         ("C-c s U" . slack-user-select)
         ("C-c s s" . slack-search-from-messages)
         ("C-c s J" . slack-jump-to-browser)
         ("C-c s p" . slack-room-pins-list)
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
               (("C-c '" . slack-message-send-from-buffer))))
  ;; (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  :custom
  (slack-buffer-emojify t)
  (slack-modeline t)
  (slack-enable-global-modeline-string t)
  :config
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
   :subscribed-channels '((wirecell_elee general "DM: Hanyu Wei" dm_team))
   ) ;; using slack-extra-subscribed-channels because I can change it dynamically
   (slack-register-team
   :name "LSU Neutrino Group"
   :token (auth-source-pick-first-password
           :host "lsuneutrinophysics.slack.com"
           :user "jmend46@lsu.edu")
   :cookie (auth-source-pick-first-password
            :host "lsuneutrinophysics.slack.com"
            :user "jmend46@lsu.edu^cookie")
   :full-and-display-names t
   :default t
   :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
   ))

;; Ledger mode
(use-package ledger-mode
  :straight t
  :hook (ledger-mode . my-ledger-hook)
  )

;; use-package with package.el:
;; (use-package dashboard
;;   :straight t
;;   :config
;;   (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
;;   (setq dashboard-match-agenda-entry "-tag")
;;   (dashboard-setup-startup-hook))

(use-package org-gantt
  :straight (:host github :repo "swillner/org-gantt"))

;; (use-package calfw
;;   :straight (:host github :repo "kiwanami/emacs-calfw" :includes calfw-org)
;;   :config
;;   (require 'calfw-org))
(use-package calfw
  :straight t
  :config
  ;; hotfix: incorrect time range display
  ;; source: https://github.com/zemaye/emacs-calfw/commit/3d17649c545423d919fd3bb9de2efe6dfff210fe
  (defun cfw:org-get-timerange (text)
  "Return a range object (begin end text).
If TEXT does not have a range, return nil."
  (let* ((dotime (cfw:org-tp text 'dotime)))
    (and (stringp dotime) (string-match org-ts-regexp dotime)
	 (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
           (start-date (nth 1 (car matches)))
           (end-date (nth 1 (nth 1 matches)))
	       (extra (cfw:org-tp text 'extra)))
	   (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
       ( list( calendar-gregorian-from-absolute
       (time-to-days
       (org-read-date nil t start-date))
       )
       (calendar-gregorian-from-absolute
       (time-to-days
       (org-read-date nil t end-date))) text)))))))


(use-package calfw-org
  :after (calfw org)
  :straight t)

(use-package calfw-blocks
  :straight (:host github :repo "ml729/calfw-blocks")
  :config
  (setq calfw-blocks-earliest-visible-time '(8 0)))


(use-package htmlize
  :straight t
  )
;; Remote Profile variables
;; Remote Tramp PATH
;; (add-to-list 'tramp-default-remote-path 'tramp-own-remote-path)
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; required to make tramp directory tracking work correctly
  (setopt tramp-remote-path '(tramp-own-remote-path))
  (setq tramp-default-method "ssh")
  (setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
)

(use-package moe-theme
  :straight t
  :config
  (moe-light))

;; Global startup commands
(org-roam-db-autosync-mode)
(server-start)

