(setq mendez/zettle-dir '("~/Sync/org/roam"))



;; (defun org-roam-insert-created-property ()
;;   "Insert :created: property for an Org-roam node.

;; Does not override the property if it already exists.

;; Calculation of the creation date is based on the filename of the note,
;; and assumes the default Org-roam naming scheme."
;;   (interactive)
;;   (when (org-roam-file-p)
;;     ;; Don't update if the created property already exists
;;     (unless (org-entry-get (point-min) "created" t)
;;       (let ((creation-time (org-roam-extract-timestamp-from-filepath
;;                             (buffer-file-name))))
;;         ;; Don't error if the filename doesn't contain a timestamp
;;         (when creation-time
;;           (save-excursion
;;             ;; Ensure point is at the beginning of the buffer
;;             (goto-char (point-min))
;;             (org-set-property "created" creation-time)))))))

;; (defun org-roam-extract-timestamp-from-filepath (filepath)
;;   "Extract timestamp from the Org-roam FILEPATH assuming it follows the default naming scheme."
;;   (let ((filename (file-name-nondirectory filepath)))
;;     (when (string-match "\\([0-9]\\{8\\}\\)\\([0-9]\\{4\\}\\)" filename)
;;       (let ((year (substring filename (match-beginning 1) (+ (match-beginning 1) 4)))
;;             (month (substring filename (+ (match-beginning 1) 4) (+ (match-beginning 1) 6)))
;;             (day (substring filename (+ (match-beginning 1) 6) (+ (match-beginning 1) 8)))
;;             (hour (substring filename (match-beginning 2) (+ (match-beginning 2) 2)))
;;             (minute (substring filename (+ (match-beginning 2) 2) (+ (match-beginning 2) 4))))
;;         (format "[%s-%s-%s %s:%s]" year month day hour minute)))))

(defun mendez/zettel-modified-property ()
  "Update the :modified: property for an Org-roam node upon saving."
  (when (org-roam-file-p)
    (save-excursion
      ;; Ensure property is applied to the whole file
      (goto-char (point-min))
      (org-set-property
       "modified" (format-time-string "[%Y-%m-%d %a %H:%M]")))))

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
