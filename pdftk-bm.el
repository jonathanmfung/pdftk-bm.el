;;; pdftk-bm.el --- Edit PDF bookmarks (AKA outline or table of contents) -*- lexical-binding: t; -*-
;; Version: 1.0.0
;; URL: https://github.com/jonathanmfung/pdftk-bm.el
;; Keywords: pdf, pdftk, tools
;; Package-Requires: ((emacs "26.1") (transient "0.7"))

;;; Commentary:
;; Tool to modify PDF bookmarks (aka Table of Contents, Outline).
;; Wrapper over `pdftk`'s `dump_data` and `update_info`.
;;
;; Flow
;; 1. User selects file through `pdftk-bm-find-pdf`
;; 2. *pdftk-bm* buffer of bookmark headings pops up.
;; 3. User edits buffer with insert/delete/promote/demote.
;;    Buffer refreshes with -make-buffer-view,
;;    can restart process with -make-buffer-fresh.
;;    Keybinds can be viewed with `pdftk-bm-transient`, bound to '?'.
;; 4. User does C-c C-c (or 'A' in transient) that runs `pdftk update_info` and
;;    creates a new pdf file.
;; 5. User verifies "<filename>_modifed.pdf".

;;; Code:

(require 'transient)

;;;; Vars
(defvar pdftk-bm-pdf-filepath nil
  "Absolute filepath of pdf whose bookmarks are being modified.")

;;;;; Customs
(defgroup pdftk-bm nil "Options for pdftk-bm.el."
  :prefix "pdftk-bm-"
  :group 'data
  :version "1.0.0")

(defcustom pdftk-bm-page-offset 0
  "Page offset, e.g. when Book page 1 is PDF page 21."
  :type 'integer :group 'pdftk-bm)

(defcustom pdftk-bm-modified-suffix "_modified"
  "Suffix to add to filename of updated PDF."
  :type 'string :group 'pdftk-bm)

(defgroup pdftk-bm-faces nil "Faces for pdftk-bm."
  :group 'pdftk-bm
  :group 'faces)

(defface pdftk-bm-title-face
  '((t :inherit default))
  "Face for Title overlay"
  :group 'pdftk-bm-faces)

(defface pdftk-bm-page-number-face
  '((t :inherit default))
  "Face for Page Number overlay"
  :group 'pdftk-bm-faces)

(defface pdftk-bm-title-modified-face
  '((((class color) (background light)) :inherit italic :foreground "grey30")
    (((class color) (background  dark)) :inherit italic :foreground "grey80"))
  "Face for modified Title overlay"
  :group 'pdftk-bm-faces)

(defface pdftk-bm-page-number-modified-face
  '((((class color) (background light)) :inherit italic :foreground "red2")
    (((class color) (background  dark)) :inherit italic :foreground "IndianRed1"))
  "Face for modified Page Number overlay"
  :group 'pdftk-bm-faces)

;;;; Structs
(cl-defstruct
    (pdftk-bm--bookmark (:constructor pdftk-bm--bookmark-create)
			(:copier nil))
  (title "" :type 'string)
  (level 1 :type 'integer)
  (page-number 1 :type 'integer)
  (modified nil :type 'bool))

;;;; Parsing
(defun pdftk-bm--split-string-first (string delim)
  "Split STRING on first occurence of DELIM.
Always returns a 2-length list.
If DELIM does not appear in STRING, second elem is nil."
  (let* ((splits (split-string string delim))
	 (tail (cdr splits)))
    (list (car splits) (when tail (string-join tail delim)))))

;; Begin > Title > Level > PageNumber
(defun pdftk-bm--parse-metadata (md)
  "MD is a list of metadata file lines.
Returns a list of pdftk-bm--bookmark."
  (declare (side-effect-free t))
  (let (temp result-list)
    (dolist (line md result-list)
      (pcase (pdftk-bm--split-string-first line ": ")
	('("BookmarkBegin" nil) (setf temp (pdftk-bm--bookmark-create)))
	(`("BookmarkTitle" ,val) (setf (pdftk-bm--bookmark-title temp) val))
	(`("BookmarkLevel" ,val) (setf (pdftk-bm--bookmark-level temp) (string-to-number val)))
	(`("BookmarkPageNumber" ,val)
	 (progn
	   (setf (pdftk-bm--bookmark-page-number temp) (string-to-number val))
	   (setf result-list (cons temp result-list))))
	(e (error "Unexpected metadata field %s" e))))))

(defvar pdftk-bm--metadata-remainder nil
  "Single string of non-Bookmark metadata.")

(defun pdftk-bm-parse (file-content)
  "FILE-CONTENT is output of `pdftk dump_data`."
  (let* ((lines (split-string file-content "\n"))
	 (pred (apply-partially #'string-match "^Bookmark*"))
	 (lines-filtered (seq-filter pred lines))
	 (lines-remainder
	  (seq-remove (lambda (x) (or (funcall pred x) (string-empty-p x))) lines))
	 ;; possible TODO: instead of filtering, can use wildcard
	 ;;   arm of --parse-metadata to keep the excess lines
	 (bm-list (pdftk-bm--parse-metadata lines-filtered)))
    (setq pdftk-bm--metadata-remainder (string-join lines-remainder "\n"))
    ;; Parsing adds latter elems to top, so reverse for proper order
    (seq-reverse bm-list)))

;;;; Buffer Creation
(cl-defgeneric pdftk-bm-to-heading (prefix level text)
  (:doc "Format an outline-mode heading"))

(cl-defmethod pdftk-bm-to-heading
  (&key prefix-char level text bookmark-obj)
  (propertize text
	      'line-prefix (concat (make-string level prefix-char) " ")
	      'pdftk-bm--bookmark-obj bookmark-obj
	      'pdftk-bm-prefix-char prefix-char))

(cl-defmethod pdftk-bm-to-heading ((bookmark pdftk-bm--bookmark))
  (pdftk-bm-to-heading :prefix-char ?*
		       :level (pdftk-bm--bookmark-level bookmark)
		       :text (pdftk-bm--bookmark-title bookmark)
		       :bookmark-obj bookmark))

(defvar pdftk-bm--data nil
  "List of (:Bookmark object
:title-olay Text overlay
:page-number-olay Page Number overlay).")

(defun pdftk-bm--sort-data ()
  "Inplace sort, by page-number."
  ;; TODO: Since multiple bookmarks can be on same page, need a way to preserve order.
  ;;       E.g. running pdftk-bm--make-buffer-view multiple times results in different orders.
  ;; TODO: If pdftk adds support for positions, then that is enough for unique sort.
  ;;       https://gitlab.com/pdftk-java/pdftk/-/issues/130
  (setq pdftk-bm--data
	(seq-sort-by (lambda (elem) (pdftk-bm--bookmark-page-number (plist-get elem :obj)))
		     #'<= pdftk-bm--data)))

(defun pdftk-bm--data-title-olay (obj)
  "Extract OBJ's title overlay from pdftk-bm--data."
  (plist-get (seq-find (lambda (x) (eq (plist-get x :obj) obj)) pdftk-bm--data) :title-olay))

(defun pdftk-bm--data-page-number-olay (obj)
  "Extract OBJ's page-number overlay from pdftk-bm--data."
  (plist-get (seq-find (lambda (x) (eq (plist-get x :obj) obj)) pdftk-bm--data) :page-number-olay))

(defun pdftk-bm--update-title-olay (olay title modified)
  (overlay-put olay 'after-string
	       (propertize title 'face (if modified 'pdftk-bm-title-modified-face 'pdftk-bm-title-face))))

(defun pdftk-bm--update-page-number-olay (olay page-number modified)
  (overlay-put olay 'after-string
	       (propertize (format " [%d]" page-number)
			   'face (if modified 'pdftk-bm-page-number-modified-face 'pdftk-bm-page-number-face))))

(defun pdftk-bm--insert-heading (bookmark update-data-flag)
  "Insert BOOKMARK as heading at point.
When UPDATE-DATA-FLAG is non-nil, pdftk-bm--data is modified."
  (let* ((bol (line-beginning-position))
	 (eol (line-end-position))
         (olay-title (make-overlay bol bol))
	 (olay-pn (make-overlay eol eol))
	 (modified (pdftk-bm--bookmark-modified bookmark)))
    (pdftk-bm--update-title-olay olay-title (pdftk-bm--bookmark-title bookmark) modified)
    (pdftk-bm--update-page-number-olay olay-pn (pdftk-bm--bookmark-page-number bookmark) modified)
    (when update-data-flag
      (add-to-list 'pdftk-bm--data (list :obj bookmark :title-olay olay-title :page-number-olay olay-pn)))))

(defun pdftk-bm--make-buffer (bookmark-list update-data-flag)
  "Create buffer populated with BOOKMARK-LIST.
When UPDATE-DATA-FLAG is non-nil, pdftk-bm--data is modified."
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
  "Re-parse old metadata and create fresh pdftk-bm--data."
  (interactive)
  (let ((inhibit-read-only t)
	(file-content (pdftk-bm-get-metadata pdftk-bm-pdf-filepath)))
    (pdftk-bm--make-buffer (pdftk-bm-parse file-content) t)))

(defun pdftk-bm-make-buffer-view ()
  "Construct buffer from existing pdftk-bm--data :obj fields."
  (interactive)
  (pdftk-bm--sort-data)
  (let ((inhibit-read-only t))
    (pdftk-bm--make-buffer (mapcar (lambda (elem) (plist-get elem :obj)) pdftk-bm--data) nil)))

;;;; Buffer Manipulation
(defun pdftk-bm--obj-at-point ()
  "Get bookmark object at point."
  (get-text-property (point) 'pdftk-bm--bookmark-obj))

(defun pdftk-bm--update-props ()
  "Rerender line at point."
  (let* ((obj (pdftk-bm--obj-at-point))
	 (level (pdftk-bm--bookmark-level obj))
	 (modified (pdftk-bm--bookmark-modified obj))
	 ;; TODO: Just make prefix-char a global defcustom
	 ;;       Default case is for empty buffer (there are no current text-properties
	 (prefix-char (or (get-text-property (point) 'pdftk-bm-prefix-char) ?*)))
    (add-text-properties (line-beginning-position) (1+ (line-end-position))
			 `(line-prefix ,(concat (make-string level prefix-char) " ")
				       pdftk-bm-prefix-char ,prefix-char))
    (pdftk-bm--update-title-olay (pdftk-bm--data-title-olay obj)
				 (pdftk-bm--bookmark-title obj) modified)
    (pdftk-bm--update-page-number-olay (pdftk-bm--data-page-number-olay obj)
				       (pdftk-bm--bookmark-page-number obj) modified)))

(defun pdftk-bm--change-level (new-level)
  "Modify bookmark object at point to have NEW-LEVEL."
  (let* ((obj (pdftk-bm--obj-at-point))
	 (inhibit-read-only t))
    (setf (pdftk-bm--bookmark-level obj) new-level)
    (pdftk-bm--update-props)))

;;;;; Interactive Commands
(defun pdftk-bm-promote ()
  "Decrease level of bookmark object at point."
  (interactive)
  (let* ((obj (pdftk-bm--obj-at-point))
	 (cur-level (pdftk-bm--bookmark-level obj))
	 (new-level (if (<= cur-level 1)
			(progn (message "Bookmark Level is already at 1") 1)
		      (1- cur-level))))
    (pdftk-bm--change-level new-level)))

(defun pdftk-bm-demote ()
  "Increase level of bookmark object at point."
  (interactive)
  (let* ((obj (pdftk-bm--obj-at-point))
	 (cur-level (pdftk-bm--bookmark-level obj))
	 (new-level (1+ cur-level)))
    (pdftk-bm--change-level new-level)))

(defun pdftk-bm--prompt-page-number ()
  (read-number "New Page Number: "
	       (pdftk-bm--bookmark-page-number (pdftk-bm--obj-at-point))))

(defun pdftk-bm-edit-page-number (new-page-number)
  "Modify bookmark object at point to have NEW-PAGE-NUMBER."
  (interactive (list (funcall 'pdftk-bm--prompt-page-number)))
  (let* ((obj (pdftk-bm--obj-at-point))
	 (inhibit-read-only t)
	 (save-point (point)))
    (setf (pdftk-bm--bookmark-page-number obj) (+ new-page-number pdftk-bm-page-offset))
    (setf (pdftk-bm--bookmark-modified obj) t)
    (pdftk-bm-make-buffer-view)
    (goto-char save-point)))

(defun pdftk-bm-edit-title (new-title)
  "Modify bookmark object at point to have NEW-TITLE."
  (interactive (list (read-string "New Title: "
				  (pdftk-bm--bookmark-title (get-text-property (point) 'pdftk-bm--bookmark-obj)))))
  (let* ((obj (pdftk-bm--obj-at-point))
	 (inhibit-read-only t)
	 (save-point (point)))
    (setf (pdftk-bm--bookmark-title obj) new-title)
    (setf (pdftk-bm--bookmark-modified obj) t)
    (pdftk-bm-make-buffer-view)
    (goto-char save-point)))

(defun pdftk-bm--read-bookmark ()
  (pdftk-bm--bookmark-create :title (read-string "Title: ")
			     :level (read-number "Level: ")
			     :page-number (+ (read-number "Page Number: ")
					     pdftk-bm-page-offset)))

(defun pdftk-bm-insert-new-bookmark (bookmark)
  "Prompt for creation of bookmark, then insert in next line."
  (interactive (list (pdftk-bm--read-bookmark)))
  (let ((inhibit-read-only t)
	(prev-props (text-properties-at (point)))
	(save-point (point)))
    (setf (pdftk-bm--bookmark-modified bookmark) t)
    (if (= (buffer-size) 0)
	(progn
	  (pdftk-bm--insert-heading bookmark t)
	  (insert (apply 'propertize "\n" prev-props))
	  (backward-char 1)
	  (put-text-property (line-beginning-position) (1+ (line-end-position))
			     'pdftk-bm--bookmark-obj bookmark))
      (progn
	;; Special case when after last heading.
	(when (= (point) (point-max)) (backward-char 1))
	(insert (apply 'propertize "\n" (text-properties-at (point))))
	(pdftk-bm--insert-heading bookmark t)
	(put-text-property (line-beginning-position) (1+ (line-end-position))
			   'pdftk-bm--bookmark-obj bookmark)))
    (pdftk-bm--update-props)
    (goto-char save-point)))

(defun pdftk-bm-delete-bookmark ()
  "Delete bookmark at point."
  (interactive)
  (let ((inhibit-read-only t)
	(obj (pdftk-bm--obj-at-point)))
    (remove-overlays (line-beginning-position) (line-end-position))
    (delete-char 1)
    ;; get obj at point and delete from data
    (setq pdftk-bm--data (seq-remove (lambda (x) (eq (plist-get x :obj) obj)) pdftk-bm--data))))

;;;; Serialization
(defun pdftk-bm--bookmark-serialize (bookmark)
  "Convert BOOKMARK to pdftk info format."
  (let ((title (pdftk-bm--bookmark-title bookmark))
	(level (pdftk-bm--bookmark-level bookmark))
	(page-number (pdftk-bm--bookmark-page-number bookmark)))
    (string-join (list "BookmarkBegin"
		       (format "BookmarkTitle: %s" title)
		       (format "BookmarkLevel: %d" level)
		       (format "BookmarkPageNumber: %d" page-number))
		 "\n")))

(defun pdftk-bm--data-serialize ()
  "Convert pdftk-bm--data to pdftk info format."
  (pdftk-bm--sort-data)
  (let ((objs (mapcar (lambda (elem) (plist-get elem :obj)) pdftk-bm--data)))
    (string-join (mapcar 'pdftk-bm--bookmark-serialize objs) "\n")))

(defun pdftk-bm--metadata-new ()
  "Merge PDF file's remainder metadata with pdftk-bm--data
into full pdftk info format."
  ;; NOTE: Even though data-serialize is being appended to remainder,
  ;;       it seems that `pdftk update_info` does some innate sorting--
  ;;       Bookmark section always appears after Info & NumberOfPages
  (string-join (list pdftk-bm--metadata-remainder (pdftk-bm--data-serialize))
	       "\n"))

(defun pdftk-bm-check ()
  "Check if pdftk program is accessible."
  (unless (executable-find "pdftk") (error "pdftk executable cannot be found")))

(defun pdftk-bm-get-metadata (filepath)
  "Get metadata of pdf FILEPATH."
  (shell-command-to-string
   (string-join (list "pdftk" (shell-quote-argument (expand-file-name filepath)) "dump_data_utf8") " ")))

(defun pdftk-bm--filepath-with-suffix (filepath suffix)
  "Add SUFFIX to filename portion of FILEPATH."
  (let* ((dir (file-name-directory filepath))
         (base (file-name-sans-extension (file-name-nondirectory filepath)))
         (ext (file-name-extension filepath)))
    (concat dir base suffix "." ext)))

;;;; Main Commands
;; TODO: Overlays cannot be searched with isearch.
;;       May want to convert title from overlay to regular text to allow searching

;;;###autoload
(defun pdftk-bm-find-pdf ()
  "Select a PDF to create a pdftk-bm buffer from."
  (interactive)
  (pdftk-bm-check)
  (setq pdftk-bm-pdf-filepath (expand-file-name (read-file-name "PDF: ")))
  (setq pdftk-bm-page-offset 0)
  (pdftk-bm-make-buffer-fresh))

;; NOTE: Always default to making a copy of pdf, let user check and delete original
(defun pdftk-bm-do-update-pdf ()
  "Apply pdftk-bm--data to registered PDF.
Modifies a copy of original file, suffixed with '_modified'."
  (interactive)
  (let* ((new-filepath
	  (pdftk-bm--filepath-with-suffix pdftk-bm-pdf-filepath pdftk-bm-modified-suffix))
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

;;;; Transient
;; NOTE: If -page-offset is defvar-local, then transient reverts to
;;       default value after each action.
;;       So need to manually reset to 0 in -find-pdf.
(transient-define-infix pdftk-bm--page-offset-transient ()
  "Set Page Offset."
  :class 'transient-lisp-variable
  :key "-o"
  :description "Page Offset"
  :variable 'pdftk-bm-page-offset)

(transient-define-prefix pdftk-bm-transient ()
  "Transient for pdftk-bm mode."

  ["Arguments" (pdftk-bm--page-offset-transient)]

  [["Edit At Point"
    ("b" "Promote Heading" pdftk-bm-promote :transient t)
    ("f" "Demote Heading" pdftk-bm-demote :transient t)]
   [""
    ("t" "Edit Title" pdftk-bm-edit-title)
    ("p" "Edit Page Number" pdftk-bm-edit-page-number)]]

  [["Commands"
    ("i" "Insert New Bookmark" pdftk-bm-insert-new-bookmark)
    ("x" "Delete Bookmark" pdftk-bm-delete-bookmark)]
   [""
    ("A" "Apply Changes (make a copy of PDF)" pdftk-bm-do-update-pdf)]]

  ["Display" ("g" "Refresh Buffer" pdftk-bm-make-buffer-view)])

(defvar pdftk-bm-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "?" #'pdftk-bm-transient)
    (keymap-set map "C-c C-c" #'pdftk-bm-do-update-pdf)
    map))

(define-derived-mode pdftk-bm-mode read-only-mode "pdftk-bm"
  "Major mode for pdftk-bm buffers."
  :interactive nil)

(provide 'pdftk-bm)
;;; pdftk-bm.el ends here
