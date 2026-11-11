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
(use-package pdf-tools
  :ensure t
 )
(setq doc-view-resolution 600)
;; Org Mode Customizations
(setq
 org-log-into-drawer t
 org-attach-store-link-p 'file
 org-attach-id-dir "~/Sync/org/roam/assets/"
 org-agenda-files '("~/Sync/org/gtd.org" "~/Sync/org/project.org" "~/Sync/org/todo.org" "~/Sync/org/cal_calendar.org" "~/Sync/org/calendar-beorg.org")
 org-directory "~/Sync/org"
 org-capture-templates
   '(("a" "Capture an Appointment" entry (file "~/Sync/org/cal_calendar.org") "* %?\n%^T")
     ("r" "note" entry (file+datetree "~/Sync/org/zettelkasten/research.org")  "* %^{PROMPT}\n%<%r>\n  %?")
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

;; Function by Yann Herklotz from his package org-zettelkasten https://git.sr.ht/~ymherklotz/org-zettelkasten/tree/main
;; (defun org-zettelkasten--incr-alpha (ident-part)
;;   "Increments a string of lowercase letters in IDENT-PART."
;;   (let ((carry 0)
;;         (new-list nil)
;;         (rev-list (reverse (string-to-list ident-part))))
;;     (let ((new-val (dolist (ident-el
;;                             (cons (+ (car rev-list) 1) (cdr rev-list))
;;                             (concat new-list))
;;                      (let ((incr-val (+ ident-el carry)))
;;                        (if (<= incr-val ?z)
;;                            (progn
;;                              (setq new-list (cons incr-val new-list))
;;                              (setq carry 0))
;;                          (setq new-list (cons ?a new-list))
;;                          (setq carry 1))))))
;;       (if (< 0 carry) (concat "a" new-val) new-val)
;;       (message new-val))))

;; (require 'org-zettelkasten)
(setq org-zettelkasten-directory "~/Sync/org/zettelkasten/")
(setq org-zettelkasten-mapping-file "~/Sync/org/zettelkasten/org-zettelkasten-index")
;; (org-zettelkasten--incr-alpha "3a4b2")
(defun emacspeak-wizards-execute-asynchronously (key)
  "Read key-sequence, then execute its command on a new thread."
  (interactive (list (read-key-sequence "Key Sequence: ")))
  (let ((l  (local-key-binding key))
        (g (global-key-binding key))
        (k
         (when-let (map(get-text-property (point) 'keymap))
           (lookup-key map key))))
    (cl-flet
        ((do-it (command)
                (make-thread command)
                (message "Running %s on a new thread." command)))
      (cond
       ( (commandp k) (do-it k))
       ( (commandp l) (do-it l))
       ((commandp g) (do-it g))
       (t (error "%s is not bound to a command." key))))))

(global-set-key (kbd "C-' a") 'emacspeak-wizards-execute-asynchronously)
;;→ \chi^{2}

(define-abbrev-table 'global-abbrev-table
    '(;; Arrows
      ("ra" "→")
      ("la" "←")
      ("ua" "↑")
      ("da" "↓")
      ;; Latex neutrino symbols
      ("numu" "\\nu_{\\mu}")
      ("nue" "\\nu_{e}")
      ("nus" "\\nu_{s}")
      ("dm2" "\\Delta{}m^{2}")
      ("dm41" "\\Delta{}m^{2}_{41}")
      ("chi2" "\\chi^{2}")
      ("dchi" "\\Delta\\chi^{2}")
      ("suu" "\\sin{\\theta_{\\mu\\mu}}^{2}")
      ("g2" "g^{2}")
      ;; Latex Structure
      ("ft" "\\frametitle{###1###}\n###@###"
       emacs-solo/abbrev--replace-placeholders)
      ;; Utils
      ("isodate" ""
       (lambda () (insert (format "%s" (format-time-string "%Y-%m-%dT%H:%M:%S")))))

      ("uuid" ""
       (lambda () (insert (org-id-uuid))))

      ;; Markdown
      ("cb" "```@\n\n```"
       (lambda () (search-backward "@") (delete-char 1)))

      ;; ORG
      ("ocb" "#+BEGIN_SRC @\n\n#+END_SRC"
       (lambda () (search-backward "@") (delete-char 1)))
      ("oheader" "#+TITLE: ###1###\n#+AUTHOR: ###2###\n#+EMAIL: ###3###\n#+OPTIONS: toc:nil\n"
       emacs-solo/abbrev--replace-placeholders)))
