;;; pdftk-bm.el --- Edit PDF bookmarks (AKA outline or table of contents) -*- lexical-binding: t; -*-
;; Version: 1.0.0
;; Keywords: pdf pdftk
;; Package-Requires: ((emacs "26.1"))
(setq pdftk-bm-filepath "~/elisp/pdftk-bm/metadata")

(cl-defstruct
    ;; TODO: rename to pdftk-bm--bookmark
    (pdftk-bm-bookmark (:constructor pdftk-bm-bookmark-create)
		       (:copier nil))
  (title :type 'string)
  (level 1 :type 'integer)
  (page-number :type 'integer))

;; (add-one :: (function (int) string))
(defun add-one (x) (+ 1 x))

;; Begin > Title > Level > PageNumber
(defun pdftk-bm--parse-metadata (md)
  "MD is a pdftk dump_data file.
Returns a list of pdftk-bm-bookmark."
  (declare (side-effect-free t))
  (setq temp nil)
  (setq result-list nil)
  (dolist (line md result-list)
    (pcase (split-string line ": ")
      ('("BookmarkBegin") (progn
			    (setf temp (pdftk-bm-bookmark-create))))
      (`("BookmarkTitle" ,val) (setf (pdftk-bm-bookmark-title temp) val))
      (`("BookmarkLevel" ,val) (setf (pdftk-bm-bookmark-level temp) (string-to-number val)))
      (`("BookmarkPageNumber" ,val) (progn
				      (setf (pdftk-bm-bookmark-page-number temp) (string-to-number val))
				      (setf result-list (cons temp result-list))))
      (e (error "Unexpected metadata field %s" e)))))

(defun pdftk-bm-parse ()
  (let* ((file-content (with-temp-buffer
			 (insert-file-contents pdftk-bm-filepath)
			 (buffer-substring-no-properties (point-min) (point-max))))
	 (lines (split-string file-content "\n"))
	 (lines-filtered (seq-filter (apply-partially #'string-match "^Bookmark*") lines))
	 ;; TODO: instead of filtering, can use wildcard arm of --parse-metadata to keep the excess lines
	 (bm-list (pdftk-bm--parse-metadata lines-filtered)))
    ;; Parsing adds latter elems to top, so reverse for proper order
    (reverse bm-list)))

(cl-defgeneric pdftk-bm-to-heading (prefix level text)
  (:doc "Format an outline-mode heading"))

;; TODO: remove unused 'pdftk-bm-level/page-number props
(cl-defmethod pdftk-bm-to-heading (&key prefix-char level text page-number bookmark-obj)
  (propertize text
	      'line-prefix (concat (make-string level prefix-char) " ")
	      'pdftk-bm-level level 'pdftk-bm-page-number page-number
	      'pdftk-bm-bookmark-obj bookmark-obj
	      'pdftk-bm-prefix-char prefix-char))

(cl-defmethod pdftk-bm-to-heading ((bookmark pdftk-bm-bookmark))
  (pdftk-bm-to-heading :prefix-char ?*
		       :level (pdftk-bm-bookmark-level bookmark)
		       :text (pdftk-bm-bookmark-title bookmark)
		       :page-number (pdftk-bm-bookmark-page-number bookmark)
		       :bookmark-obj bookmark))

;; (pdftk-bm-to-heading :prefix-char ?- :level 2 :text "Asdf" :page-number 10)
;; (pdftk-bm-to-heading (pdftk-bm-bookmark-create :title "Foo" :level 3 :page-number 50))

;; Buffer & Text Properties & Overlays ----------------

(defvar pdftk-bm--data nil
  " list of (:Bookmark object :title Text overlay :page-number Page Number overlay)")

(defun pdftk-bm--sort-data ()
  "Inplace sort, by page-number."
  (setq pdftk-bm--data
	(seq-sort-by (lambda (elem) (pdftk-bm-bookmark-page-number (plist-get elem :obj)))
		     #'<= pdftk-bm--data)))

(defun pdftk-bm--data-title (obj)
  (plist-get (seq-find (lambda (x) (eq (plist-get x :obj) obj)) pdftk-bm--data) :title))

(defun pdftk-bm--data-page-number (obj)
  (plist-get (seq-find (lambda (x) (eq (plist-get x :obj) obj)) pdftk-bm--data) :page-number))

(defun pdftk-bm--insert-heading (bookmark update-data-flag)
  (let* ((bol (line-beginning-position))
	 (eol (line-end-position))
         (olay-title (make-overlay bol bol))
	 (olay-pn (make-overlay eol eol)))
    (overlay-put olay-title 'after-string
		 (propertize (pdftk-bm-bookmark-title bookmark)
                             'face '(:foreground "pink" :weight bold)))
    (overlay-put olay-pn 'after-string
		 (propertize (concat " " (number-to-string (pdftk-bm-bookmark-page-number bookmark)))
                             'face '(:foreground "red" :weight bold)))
    (when update-data-flag
      (add-to-list 'pdftk-bm--data (list :obj bookmark :title olay-title :page-number olay-pn)))))

(defun pdftk-bm--make-buffer (bookmark-list update-data-flag)
  (let ((buf (get-buffer-create "*pdftk-bm*")))
    (with-current-buffer buf
      (fundamental-mode)
      (erase-buffer)
      (remove-overlays (point-min) (point-max))
      (when update-data-flag (setq pdftk-bm--data nil))
      (seq-doseq (bookmark bookmark-list)
	(insert (apply 'propertize "" (text-properties-at 0 (pdftk-bm-to-heading bookmark))))
	(pdftk-bm--insert-heading bookmark update-data-flag)
	(insert (apply 'propertize "\n" (text-properties-at 0 (pdftk-bm-to-heading bookmark))))))
    (pop-to-buffer buf)
    (pdftk-bm-mode)
    (goto-char (point-min))))

(defun pdftk-bm--make-buffer-fresh ()
  "Re-parse metadata and create fresh data."
  (interactive)
  (let ((inhibit-read-only t))
    (pdftk-bm--make-buffer (pdftk-bm-parse) t)))

(defun pdftk-bm--make-buffer-view ()
  "Construct buffer from existing data :obj fields,"
  (interactive)
  (pdftk-bm--sort-data)
  (let ((inhibit-read-only t))
    (pdftk-bm--make-buffer (mapcar (lambda (elem) (plist-get elem :obj)) pdftk-bm--data) nil)))

;;

(defun pdftk-bm--obj-at-point ()
  (get-text-property (point) 'pdftk-bm-bookmark-obj))

(defun pdftk-bm--update-props ()
  (let* ((obj (pdftk-bm--obj-at-point))
	 (olay-title (pdftk-bm--data-title obj))
	 (olay-pn (pdftk-bm--data-page-number obj))
	 (title (pdftk-bm-bookmark-title obj))
	 (level (pdftk-bm-bookmark-level obj))
	 (page-number (pdftk-bm-bookmark-page-number obj))
	 (prefix-char (get-text-property (point) 'pdftk-bm-prefix-char)))
    (add-text-properties (line-beginning-position) (1+ (line-end-position))
			 `(line-prefix ,(concat (make-string level prefix-char) " ")
				       pdftk-bm-level ,level
				       pdftk-bm-page-number ,page-number
				       pdftk-bm-prefix-char ,prefix-char))
    (overlay-put olay-pn 'after-string (propertize (concat " " (number-to-string page-number))
						   'face '(:foreground "blue" :weight bold)))
    (overlay-put olay-title 'after-string (propertize title
						      'face '(:foreground "purple" :weight bold)))))

(defun pdftk-bm--change-level (new-level)
  (let* ((obj (pdftk-bm--obj-at-point))
	 (inhibit-read-only t))
    (setf (pdftk-bm-bookmark-level obj) new-level)
    (pdftk-bm--update-props)))

(defun pdftk-bm-promote ()
  (interactive)
  (let* ((obj (pdftk-bm--obj-at-point))
	 (cur-level (pdftk-bm-bookmark-level obj))
	 (new-level (if (<= cur-level 1)
			(progn (message "Bookmark Level is already at 1") 1)
		      (1- cur-level))))
    (pdftk-bm--change-level new-level)))

(defun pdftk-bm-demote ()
  (interactive)
  (let* ((obj (pdftk-bm--obj-at-point))
	 (cur-level (pdftk-bm-bookmark-level obj))
	 (new-level (1+ cur-level)))
    (pdftk-bm--change-level new-level)))

(defun pdftk-bm-edit-page-number (new-page-number)
  (interactive (list (read-number "New Page Number: "
				  (pdftk-bm-bookmark-page-number (get-text-property (point) 'pdftk-bm-bookmark-obj)))))
  (let* ((obj (pdftk-bm--obj-at-point))
	 (inhibit-read-only t))
    (setf (pdftk-bm-bookmark-page-number obj) new-page-number)
    (pdftk-bm--update-props)))

(defun pdftk-bm-edit-title (new-title)
  (interactive (list (read-string "New Title: "
				  (pdftk-bm-bookmark-title (get-text-property (point) 'pdftk-bm-bookmark-obj)))))
  (let* ((obj (pdftk-bm--obj-at-point))
	 (cur-title (pdftk-bm-bookmark-title obj))
	 (inhibit-read-only t))
    (setf (pdftk-bm-bookmark-title obj) new-title)
    (pdftk-bm--update-props)))

(defun pdftk-bm-insert-new-bookmark ()
  (interactive)
  (let ((inhibit-read-only t)
	(prev-props (text-properties-at (point)))
	(prev-obj (pdftk-bm--obj-at-point))
	(bookmark (pdftk-bm-bookmark-create :title (read-string "Title: ")
					    :level (read-number "Level: ")
					    :page-number (read-number "Page Number: "))))
    (insert (apply 'propertize "\n" prev-props))
    (pdftk-bm--insert-heading bookmark t)
    (put-text-property (line-beginning-position) (1+ (line-end-position))
		       'pdftk-bm-bookmark-obj bookmark)
    (pdftk-bm--update-props)))


(defun pdftk-bm--bookmark-serialize (bookmark)
  (let ((title (pdftk-bm-bookmark-title bookmark))
	(level (pdftk-bm-bookmark-level bookmark))
	(page-number (pdftk-bm-bookmark-page-number bookmark)))
    (string-join (list "BookmarkBegin"
		       (format "BookmarkTitle: %s" title)
		       (format "BookmarkLevel: %d" level)
		       (format "BookmarkPageNumber: %d" page-number))
		 "\n")))

;; (pdftk-bm--bookmark-serialize (pdftk-bm-bookmark-create :title "foo" :level 2 :page-number 33))

(defun pdftk-bm--data-serialize ()
  (pdftk-bm--sort-data)
  (let ((objs (mapcar (lambda (elem) (plist-get elem :obj)) pdftk-bm--data)))
    (string-join (mapcar 'pdftk-bm--bookmark-serialize objs) "\n")))

(pdftk-bm--data-serialize)

;; TODO: Need to splice serialized data back into metadata file
;;       If order does not matter, can just filter out Bookmark lines like (pdftk-bm-parse)
;;       IF order does matter, need to keep points on metadata string


(defun pdftk-bm-check ()
  (unless (executable-find "pdftk") (error "pdftk executable cannot be found.")))

;; TODO:
(defun pdftk-bm-find-pdf ()
  (interactive)
  (pdftk-bm-check)
  ;; find-file, store as pdftk-bm-pdf-file-name
  ;; TODO: Maybe store *pdftk-bm-old-md* as var too
  ;; 1. run `pdftk input.pdf dump_data_utf8`
  ;; 2. (pdftk-bm-parse pdftk-bm--metadata) [or some variant of parse that takes in whole file]
  ;; 3. (pdftk-bm--make-buffer-fresh) [or some variant that takes in whole file]
  ;; 4. run `pdftk input.pdf update_info_utf8 new.metadata output output.pdf`
  )

(shell-command-to-string "pdftk TRaAT.pdf dump_data_utf8")

(let ((process (make-process :name "pdftk-bm-old-md"
                             :buffer "*pdftk-bm-old-md*"
                             :command '("pdftk" "TRaAT.pdf" "dump_data_utf8")
			     ;; Remove "Process ... finished" https://stackoverflow.com/a/42845321
			     :sentinel #'ignore
			     )))
  (process-send-eof process)
  (process-buffer process))

;; NOTE: Always default to making a copy of pdf, let user check and delete original
(let ((process (make-process :name "pdftk-bm-update-md"
                             :buffer nil
                             :command '("pdftk" "TRaAT.pdf"
					"update_info_utf8" "-"
					"output" "TRaAT_modified.pdf")
                             :connection-type 'pipe
			     :sentinel #'ignore)))
  (process-send-string process
		       (string-join
			(list (pdftk-bm--bookmark-serialize
			       (pdftk-bm-bookmark-create :title "From Emacs" :level 1 :page-number 12))
			      (pdftk-bm--bookmark-serialize
			       (pdftk-bm-bookmark-create :title "Foo" :level 2 :page-number 14))
			      (pdftk-bm--bookmark-serialize
			       (pdftk-bm-bookmark-create :title "Bar" :level 2 :page-number 15)))
			"\n"))
  (process-send-eof process))

(defun pdftk-bm-save-to-pdf)

(define-derived-mode pdftk-bm-mode read-only-mode "pdftk-bm"
  "Major mode for pdftk-bm buffers."
  ;; TODO: add face-colors for line-prefix, title-overlay, and page-number-overlay
  :interactive nil)


;;
;;


(pdftk-bm-bookmark-create :title "foo" :page-number 20)
(progn
  (setq foo (pdftk-bm-bookmark-create :title nil :page-number nil))
  (setf (pdftk-bm-bookmark-title foo) "a title")
  (cl-incf (pdftk-bm-bookmark-level foo))
  foo)


(defun pdftk-bm---extract-with-prefix (prefix string)
  (when (string-match (concat "^" prefix "\\(.*\\)") string)
    (match-string 1 string)))

(pdftk-bm---extract-with-prefix "BookmarkTitle: " "BookmarkTitle: 1. Equivalence and reduction")
(pdftk-bm---extract-with-prefix "BookmarkTitle: " "BookmarkLevel: 1")

(split-string "BookmarkBegin" ": ")
(split-string "BookmarkTitle: 2" ": ")
(split-string "BookmarkTitle: Contents" ": ")


(let ((string "BookmarkTitle: 1. Equivalence and reduction"))
  (string-match "^BookmarkTitle: \\(.*\\)" string)
  (match-string 1 string))

(let ((string "hello world"))
  (when (string-match "\\(hello\\) \\(world\\)" string)
    (message "Full match: %s" (match-string 0 string))
    (message "First group: %s" (match-string 1 string))
    (message "Second group: %s" (match-string 2 string))))

;;; pdftk-bm.el ends here
