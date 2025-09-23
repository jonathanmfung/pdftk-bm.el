;;; pdftk-bm.el --- Edit PDF bookmarks (AKA outline or table of contents) -*- lexical-binding: t; -*-
;; Version: 1.0.0
;; Keywords: pdf pdftk
;; Package-Requires: ((emacs "26.1"))
;;; Structs
(setq pdftk-bm-filepath "~/elisp/pdftk-bm/metadata")

(cl-defstruct
    ;; TODO: rename to pdftk-bm--bookmark
    (pdftk-bm-bookmark (:constructor pdftk-bm-bookmark-create)
		       (:copier nil))
  (title "" :type 'string)
  (level 1 :type 'integer)
  (page-number 1 :type 'integer))

;; Elsa annotations
;; (add-one :: (function (number) number))
(defun add-one (x) (+ 1 x))

;; (return-plist :: (function () (list (or symbol number))))
(defun return-plist () '(:foo 1 :bar 2))

(lambda (index arg)
  ;; (var arg :: (struct elsa-form))
  (progn
    ;; arg here is now of type elsa-form
    (oref arg start)))

;; (foo-bookmark :: (function ((struct pdftk-bm-bookmark)) string))
(defun foo-bookmark (bm)
  (pdftk-bm-bookmark-title bm))

;;; Parsing

;; Begin > Title > Level > PageNumber
(defun pdftk-bm--parse-metadata (md)
  "MD is a list of metadata file lines.
Returns a list of pdftk-bm-bookmark."
  (declare (side-effect-free t))
  ;; (var result-list :: (list pdftk-bm-bookmark))
  (let (temp result-list)
    (dolist (line md result-list)
      (pcase (split-string line ": ")
	('("BookmarkBegin") (setf temp (pdftk-bm-bookmark-create)))
	(`("BookmarkTitle" ,val) (setf (pdftk-bm-bookmark-title temp) val))
	(`("BookmarkLevel" ,val) (setf (pdftk-bm-bookmark-level temp) (string-to-number val)))
	(`("BookmarkPageNumber" ,val) (progn
					(setf (pdftk-bm-bookmark-page-number temp) (string-to-number val))
					(setf result-list (cons temp result-list))))
	(e (error "Unexpected metadata field %s" e))))))

(defvar pdftk-bm--metadata-remainder nil
  "Single string of non-Bookmark metadata.")

(defun pdftk-bm-parse (file-content)
  "FILE-CONTENT is output of `pdftk dump_data`"
  (let* ((lines (split-string file-content "\n"))
	 (pred (apply-partially #'string-match "^Bookmark*"))
	 (lines-filtered (seq-filter pred lines))
	 (lines-remainder (seq-remove (lambda (x) (or (funcall pred x) (string-empty-p x))) lines))
	 ;; possible TODO: instead of filtering, can use wildcard arm of --parse-metadata to keep the excess lines
	 (bm-list (pdftk-bm--parse-metadata lines-filtered)))
    (setq pdftk-bm--metadata-remainder (string-join lines-remainder "\n"))
    ;; Parsing adds latter elems to top, so reverse for proper order
    (seq-reverse bm-list)))

;;; Buffer Creation

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
  ;; TODO: Since multiple bookmarks can be on same page, need a way to preserve order
  ;;       E.g. running pdftk-bm--make-buffer-view multiple times results in different orders
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
    ;; TODO: add 'modified field to bookmark that conditions the color
    ;;       also needs same logic in --update-props
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

(defun pdftk-bm-make-buffer-fresh ()
  "Re-parse old metadata and create fresh data."
  (interactive)
  (let ((inhibit-read-only t)
	(file-content (pdftk-bm-get-metadata pdftk-bm-pdf-filepath)))
    (pdftk-bm--make-buffer (pdftk-bm-parse file-content) t)))

(defun pdftk-bm-make-buffer-view ()
  "Construct buffer from existing data :obj fields,"
  (interactive)
  (pdftk-bm--sort-data)
  (let ((inhibit-read-only t))
    (pdftk-bm--make-buffer (mapcar (lambda (elem) (plist-get elem :obj)) pdftk-bm--data) nil)))

;;; Buffer Manipulation
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

;;;; Interactive Commands

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
  "Prompt for creation of bookmark, then insert in next line."
  ;; TODO: this fails when used at (point-max), specifically put-text-property goes out of bounds.
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

(defun pdftk-bm-delete-bookmark ()
  "Delete bookmark at point."
  (interactive)
  (let ((inhibit-read-only t)
	(obj (pdftk-bm--obj-at-point)))
    (remove-overlays (line-beginning-position) (line-end-position))
    (delete-char 1)
    ;; get obj at point and delete from data
    (setq pdftk-bm--data (seq-remove (lambda (x) (eq (plist-get x :obj) obj)) pdftk-bm--data))))

;;; Serialization

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

(defun pdftk-bm--metadata-new ()
  ;; NOTE: Even though data-serialize is being appended to remainder,
  ;;       it seems that `pdftk update_info` does some innate sorting--
  ;;       Bookmark section always appears after Info & NumberOfPages
  (string-join (list pdftk-bm--metadata-remainder (pdftk-bm--data-serialize))
	       "\n"))

(defun pdftk-bm-check ()
  (unless (executable-find "pdftk") (error "pdftk executable cannot be found.")))

(defvar pdftk-bm-pdf-filepath nil "Absolute filepath of pdf whose bookmarks are being modified.")

(defun pdftk-bm-get-metadata (filepath)
  (shell-command-to-string
   (string-join (list "pdftk" (shell-quote-argument (expand-file-name filepath)) "dump_data_utf8") " ")))

;; (pdftk-bm-get-metadata (read-file-name ""))

(defun pdftk-bm--filepath-with-suffix (filepath suffix)
  (let* ((dir (file-name-directory filepath))
         (base (file-name-sans-extension (file-name-nondirectory filepath)))
         (ext (file-name-extension filepath)))
    (concat dir base suffix "." ext)))
;; (pdftk-bm--filepath-with-suffix "~/elisp/pdftk-bm/TRaAT.pdf" "_modified")

;;; Main Commands
;; Flow
;; 1. User selects file through read-file-name
;; 2. *pdftk-bm<FILE>* buffer of bookmark headings pops up
;; 3. User edits buffer with insert/delete/promote/demote
;;    Buffer refreshes with -view, can restart process with -fresh
;; 4. User does C-c C-c that runs `pdftk update_info` and creates a new pdf file

;; TODO: overlays cannot be searched with isearch

;; TODO:
(defun pdftk-bm-find-pdf ()
  (interactive)
  (pdftk-bm-check)
  (setq pdftk-bm-pdf-filepath (expand-file-name (read-file-name "PDF: ")))
  (pdftk-bm--make-buffer-fresh))

;; NOTE: Always default to making a copy of pdf, let user check and delete original
(defun pdftk-bm-do-update-pdf ()
  (interactive)
  (let* ((new-filepath (pdftk-bm--filepath-with-suffix pdftk-bm-pdf-filepath "_modified"))
	 (process (make-process :name "pdftk-bm-update"
				:buffer nil
				:command `("pdftk" ,pdftk-bm-pdf-filepath
					   "update_info_utf8" "-"
					   "output" ,new-filepath)
				:connection-type 'pipe
				:sentinel #'ignore)))
    (process-send-string process (pdftk-bm--metadata-new))
    (process-send-eof process)
    (message "Successfully created %s" new-filepath)))

(define-derived-mode pdftk-bm-mode read-only-mode "pdftk-bm"
  "Major mode for pdftk-bm buffers."
  ;; TODO: add face-colors for line-prefix, title-overlay, and page-number-overlay
  :interactive nil)

;;; pdftk-bm.el ends here
