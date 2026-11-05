(setq mendez/zettle-dir '("~/Sync/org/roam"))

;; Increment the last character of the id (internal)
(defun ymhg/incr-id (ident)
  (let ((ident-list (string-to-list ident)))
    (cl-incf (car (last ident-list)))
    (concat ident-list)))


;; Branch off of existing id (internal)
(defun ymhg/branch-id (ident)
  (if (string-match-p ".*[0-9]$" ident)
      (concat ident "a")
    (concat ident "1")))

;; Create a new id by using the reading the current id (internal)
(defun ymhg/org-zettelkasten-create (incr newheading)
  (let* ((current-id (org-entry-get nil "CUSTOM_ID"))
         (next-id (funcall incr current-id)))
    (funcall newheading)
    (org-set-property "CUSTOM_ID" next-id)))

;; Create a heading of same level and insert id
(defun mendez/org-zettelkasten-create-branch ()
  (interactive)
  (ymhg/org-zettelkasten-create
   'ymhg/incr-id 'org-insert-heading))

;; Create a subheading and insert id
(defun mendez/org-zettelkasten-create-next ()
  (interactive)
  (ymhg/org-zettelkasten-create
   'ymhg/branch-id '(lambda () (org-insert-subheading ""))))

;; Automatically create same or sublevel Heading
;; Logic: Go to the beginning of current heading, go forward to the first sub-heading
;; If the the position of current/1st sub heading is the same create a new heading of the same level
;; If the position of current/1st sub heading is different, create a new branch off of the current level
(global-set-key "\C-cn" 'mendez/org-zettelkasten-create-next)
(global-set-key "\C-cb" 'mendez/org-zettelkasten-create-branch)
(defun mendez/zettle-search ()
  "Search Directory"
  (interactive)
  (let ((org-agenda-files mendez/zettle-dir))
  (message "%s"
           org-agenda-files)
  (org-search-view)
  ))
