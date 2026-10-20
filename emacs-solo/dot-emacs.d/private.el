;;; private.el --- Emacs Solo (external packages) Configuration --- Init  -*- lexical-binding: t; byte-compile-warnings: (not free-vars unresolved make-local); -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities` and `package-pinned-packages`.
;; Most users will not need or want to do this.
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; Allow project-aware grep/search tools work without using consult in org roam (or any directory)
(add-to-list 'project-vc-extra-root-markers ".project-root")
(use-package org-roam
  :ensure t
  :hook ((org-mode . visual-line-mode))
  :bind
  (("C-c d c" . org-roam-dailies-capture-today)
   ("C-c d ." . org-roam-dailies-goto-today)
   ("C-c d n" . org-roam-dailies-goto-next-note)
   ("C-c d p" . org-roam-dailies-goto-previous-note)
   ("C-c d d" . org-roam-dailies-goto-date)
   ("C-c d t" . org-roam-dailies-goto-tomorrow)
   ("C-c d y" . org-roam-dailies-goto-yesterday)
   ("C-c r f" . org-roam-node-find)
   ("C-c r t" . org-roam-tag-add)
   ("C-c r i" . org-roam-node-insert)
   )
  :custom
  (org-roam-directory "~/Sync/org/roam")
  :config
  (defun org-roam-insert-created-property ()
  "Insert :created: property for an Org-roam node.

Does not override the property if it already exists.

Calculation of the creation date is based on the filename of the note,
and assumes the default Org-roam naming scheme."
  (interactive)
  (when (org-roam-file-p)
    ;; Don't update if the created property already exists
    (unless (org-entry-get (point-min) "created" t)
      (let ((creation-time (org-roam-extract-timestamp-from-filepath
                            (buffer-file-name))))
        ;; Don't error if the filename doesn't contain a timestamp
        (when creation-time
          (save-excursion
            ;; Ensure point is at the beginning of the buffer
            (goto-char (point-min))
            (org-set-property "created" creation-time)))))))

(defun org-roam-extract-timestamp-from-filepath (filepath)
  "Extract timestamp from the Org-roam FILEPATH assuming it follows the default naming scheme."
  (let ((filename (file-name-nondirectory filepath)))
    (when (string-match "\\([0-9]\\{8\\}\\)\\([0-9]\\{4\\}\\)" filename)
      (let ((year (substring filename (match-beginning 1) (+ (match-beginning 1) 4)))
            (month (substring filename (+ (match-beginning 1) 4) (+ (match-beginning 1) 6)))
            (day (substring filename (+ (match-beginning 1) 6) (+ (match-beginning 1) 8)))
            (hour (substring filename (match-beginning 2) (+ (match-beginning 2) 2)))
            (minute (substring filename (+ (match-beginning 2) 2) (+ (match-beginning 2) 4))))
        (format "[%s-%s-%s %s:%s]" year month day hour minute)))))

(defun org-roam-insert-modified-property ()
  "Update the :modified: property for an Org-roam node upon saving."
  (when (org-roam-file-p)
    (save-excursion
      ;; Ensure property is applied to the whole file
      (goto-char (point-min))
      (org-set-property
       "modified" (format-time-string "[%Y-%m-%d %a %H:%M]")))))

(org-roam-db-autosync-mode)
(add-hook 'before-save-hook #'org-roam-insert-created-property)
(add-hook 'before-save-hook #'org-roam-insert-modified-property)
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:30}" 'face 'org-tag)))
(setq org-roam-node-template-prefixes `(("tags" . "::") ("todo" . "t:")))

(setq org-roam-dailies-directory "dailies/")
(setq org-roam-dalies-capture-templates '(("d" "default" entry "* %?" :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>
"))))
;; (setq org-roam-dailies-capture-templates '(("j" "journal"
;; entry
;; "* %?"
;; :target (file+head "%<%Y%m%d>.org" "#+title: %<%Y-%m-%d>\n"))))
(setq org-roam-capture-templates '(("d" "default"
                                    plain
                                    "%?"
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n - sequence :: ")
                                    :unnarrowed t)
                                   ("p" "project"
                                    plain
                                    "%?"
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n - sequence :: ")
                                    :unnarrowed t)
                                   ("a" "area"
                                    plain
                                    "%?"
                                    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                    :unnarrowed t)
                                   ;; ("p" "project"
                                   ;;  plain
                                   ;;  "%?"
                                   ;;  :target (file+head "project/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                   ;;  :unnarrowed t)
                                   ("r" "bibliography reference" plain
                                    "%?"
                                    :if-new
                                    (file+head "ref/${citekey}.org" "#+title: ${title}\n
#+filetags: ${entry-type}

* Analysis of ${entry-type} by ${author}
  :PROPERTIES:
  :URL: ${url}
  :NOTER_DOCUMENT: ~/Sync/org/roam/ref/documents/${citekey}.pdf
  :NOTER_PAGE:
  :END:")
                                    :unnarrowed t
                                    :jump-to-captured t)
                                   )))
;; Org Mode Customizations
(setq
 org-log-into-drawer t
 org-attach-store-link-p 'file
 org-attach-id-dir "~/Sync/org/roam/assets/"
 org-agenda-files '("~/Sync/org/gtd.org" "~/Sync/org/project.org" "~/Sync/org/todo.org" "~/Sync/org/cal_calendar.org" "~/Sync/org/calendar-beorg.org")
 org-directory "~/Sync/org"
 org-capture-templates
   '(("a" "Capture an Appointment" entry (file "~/Sync/org/cal_calendar.org") "* %?\n%^T")
     ("r" "note" entry (file+datetree "~/Sync/org/research.org")  "* %?\nEntered on %U\n  %i")
     ("i" "Capture an idea to inbox" entry (file "~/Sync/org/inbox.org") "* %?\n"))
   org-agenda-custom-commands
   '(
     ("b" "Work Computer [b]ortan" tags-todo "@bortan-someday")
     ("c" "[C]omputer (any)" tags-todo "@computer-someday")
     ("d" "Rea[d]ing List" tags-todo "@read-someday")
     ("e" "[E]rrand" tags-todo "@errand-someday")
     ("h" "[H]ome" tags-todo "@home-someday")
     ("l" "[L]ab" tags-todo "@lab-someday")
     ("u" "Camp[u]s" tags-todo "@campus-someday")
     ("m" "Personal Computer [m]ilo" tags-todo "@milo-someday")
     ("o" "[O]ffice" tags-todo "@office-someday")
     ("p" "[P]hone" tags-todo "@phone-someday")
     ("r" "[R]esearch" tags-todo "@research-someday")
     ("s" "Brain[s]torm" tags-todo "@brainstorm-someday")
     ("W" "Weekly Review"
      ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
       (tags "inbox")
       (tags-todo "-someday-inbox")					  ; type "l" in the agenda to review logged items
       (stuck "") ; review stuck projects as designated by org-stuck-projects
       (tags "Project-someday+LEVEL=1") ; review all projects (assuming you use todo keywords to designate projects)
       (tags-todo "-someday+TODO=\"WAITING\"")

       (tags "someday+LEVEL=2")))
     ("A" "Ana" agenda ""
      ((org-agenda-files '("~/Sync/org/calendar-beorg.org"))
       (org-agenda-tag-filter-preset '("+aveneriotrejo@gmailcom"))
       (org-agenda-remove-tags t)
       (org-agenda-overriding-header "Ana's Calendar")))
     ("D" "Daily Review "
      ((agenda ""
               ((org-agenda-overriding-header "Completed Tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                (org-agenda-span 'day))))))
     org-agenda-tag-filter-preset '("-aveneriotrejo@gmailcom")
     org-refile-allow-creating-parent-nodes t
     org-refile-use-outline-path 'file
     org-outline-path-complete-in-steps nil
     org-refile-targets '((nil :maxlevel . 9)
                        ("~/Sync/org/someday.org" :maxlevel . 9)
                        ("~/Sync/org/gtd.org" :maxlevel . 3)
                        ("~/Sync/org/research.org" :maxlevel . 3)
                        ("~/Sync/org/todo.org" :maxlevel . 3)
                        ("~/Sync/org/project.org" :maxlevel . 9)
                        ("~/Sync/org/cal_calendar.org" :maxlevel . 9))
     org-tags-exclude-from-inheritance '(("project"))
     org-attach-id-dir "~/Sync/org/roam/assets/"
     org-export-allow-bind-keywords t
     org-cite-global-bibliography '("~/Sync/org/roam/ref/zot.bib"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c." 'org-time-stamp)
(global-set-key "\C-co" 'org-noter)
(global-set-key "\C-ck" 'kill-src-block-at-point)
(global-set-key "\C-cu" 'org-reset-checkbox-state-subtree)
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '((tex-mode) "digestif")))
 (add-hook 'tex-mode-hook #'eglot-ensure)
;; (use-package tex-mode
;;   :ensure nil
;;   :hook (latex-mode . eglot-ensure))
;; (require 'oauth2)

  ;;     message-self-insert-commands nil
  ;;     message-expand-name-standard-ui t))
;; (use-package message
;;   :ensure nil
;;   :custom
;;   (message-mail-alias-type 'ecomplete)
;;   ;; (message-self-insert-commands nil)   ; don't pop up completions while typing
;;   (message-expand-name-standard-ui t)  ; use normal completion-at-point / TAB
;;   :hook
;;   ;; Record To/Cc/From/Reply-To of every message you send
;;   (message-sent-hook . message-put-addresses-in-ecomplete))
(use-package message
  :ensure nil
  :custom
  (message-mail-alias-type 'ecomplete)
  ;; (message-self-insert-commands nil)   ; don't pop up completions while typing
  (message-expand-name-standard-ui t)  ; use normal completion-at-point / TAB
  :hook
  ;; Record To/Cc/From/Reply-To of every message you send
  (message-sent . message-put-addresses-in-ecomplete))

(setq mendez/zettle-dir '("~/Sync/org/roam" "~/Sync/org/roam/dailies"))
(defun mendez/zettle-search ()
  "Search Directory"
  (interactive)
  (let ((org-agenda-files mendez/zettle-dir))
  (message "%s"
           org-agenda-files)
  (org-search-view)
  ))
