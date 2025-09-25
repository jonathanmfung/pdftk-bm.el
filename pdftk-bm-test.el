;;; pdftk-bm-test.el -*- lexical-binding: t; -*-
(require 'ert)
(require 'pdftk-bm)

(ert-deftest split-string-first-test ()
  (should (equal (pdftk-bm--split-string-first "Foo" "-")
		 '("Foo" nil)))
  (should (equal (pdftk-bm--split-string-first "Foo-Bar" "-")
		 '("Foo" "Bar")))
  (should (equal (pdftk-bm--split-string-first "Foo-Bar-Qux" "-")
		 '("Foo" "Bar-Qux"))))

(ert-deftest parse-metadata ()
  (let ((expected (list (pdftk-bm--bookmark-create :title "Foo" :level 1 :page-number 2))))
    (should (equal (pdftk-bm--parse-metadata '("BookmarkBegin" "BookmarkTitle: Foo" "BookmarkLevel: 1" "BookmarkPageNumber: 2"))
		   expected))
    (should (equal (pdftk-bm--parse-metadata '("BookmarkBegin" "BookmarkLevel: 1" "BookmarkPageNumber: 2" "BookmarkTitle: Foo"))
		   expected))
    (should (equal (pdftk-bm--parse-metadata '("BookmarkBegin" "BookmarkTitle: Foo" "BookmarkPageNumber: 2" "BookmarkLevel: 1"))
		   expected))
    (should (equal (pdftk-bm--parse-metadata '("BookmarkBegin" "BookmarkPageNumber: 2" "BookmarkLevel: 1" "BookmarkTitle: Foo"))
		   expected))
    (should-error
     (equal (pdftk-bm--parse-metadata '("BookmarkTitle: Foo" "BookmarkBegin"  "BookmarkPageNumber: 2" "BookmarkLevel: 1"))
	    expected)
     :type 'wrong-type-argument)))

(ert-deftest insert-new-bookmark ()
  "pdftk-bm-insert-new-bookmark inserts in next line without moving point,
unless at point-max."
  (let ((pdftk-bm--data nil)
	(bookmark1 (pdftk-bm--bookmark-create :title "Foo" :level 1 :page-number 2))
	(bookmark2 (pdftk-bm--bookmark-create :title "Bar" :level 3 :page-number 4)))
    ;; Empty Buffer
    (with-temp-buffer
      (pdftk-bm-insert-new-bookmark bookmark1)
      (should (equal (pdftk-bm--bookmark-title (pdftk-bm--obj-at-point)) "Foo")))
    (with-temp-buffer
      (pdftk-bm-insert-new-bookmark bookmark1)
      (pdftk-bm-insert-new-bookmark bookmark2)
      (should (equal (pdftk-bm--bookmark-title (pdftk-bm--obj-at-point)) "Foo")))
    (with-temp-buffer
      (pdftk-bm-insert-new-bookmark bookmark1)
      (pdftk-bm-insert-new-bookmark bookmark2)
      (forward-char 1)
      (should (equal (pdftk-bm--bookmark-title (pdftk-bm--obj-at-point)) "Bar")))
    ;; At point-max
    (with-temp-buffer
      (pdftk-bm-insert-new-bookmark bookmark1)
      (goto-char (point-max))
      (pdftk-bm-insert-new-bookmark bookmark2)
      (should (equal (pdftk-bm--bookmark-title (pdftk-bm--obj-at-point)) "Bar")))))

(ert-deftest bookmark-serialize ()
  (should (equal (pdftk-bm--bookmark-serialize (pdftk-bm--bookmark-create :title "Foo" :level 1 :page-number 2))
		 "BookmarkBegin\nBookmarkTitle: Foo\nBookmarkLevel: 1\nBookmarkPageNumber: 2")))

(ert-deftest filepath-with-suffix ()
  (should (equal (pdftk-bm--filepath-with-suffix "/foo/bar/file.pdf" "_modified")
		 "/foo/bar/file_modified.pdf"))
  (should (equal (pdftk-bm--filepath-with-suffix "file.pdf" "_modified")
		 "file_modified.pdf"))
  (should (equal (pdftk-bm--filepath-with-suffix "~/foo/file.pdf" "_modified")
		 "~/foo/file_modified.pdf")))

(provide 'pdftk-bm-test)
