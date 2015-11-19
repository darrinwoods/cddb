;; A function for adding CD information
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; Creates a db to hold the records
(defvar *db* nil)

;; A function for adding individual records (in the REPL)
(defun add-record (cd) (push cd *db*))

;; A function to pretty print the contents of the db
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; Makes prompts for user to enter information
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; Making it a little easier for the user:
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))
	      

;; Making it easier to add a bunch of CD's at one time:
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; Makes it possible to save the database
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; Makes it possible to load the db back in for use in the REPL.
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Allows user to select records by artist
(defun select-by-artist (artist)
  (remove-if-not
   #' (lambda (cd) (equal (getf cd :artist) artist))
      *db*))

;; a more general select function that takes a function as an argument.
;; this would be used like (select #' (lambda (cd) (equal (getf cd :artist) "Lyle Lovett")))
;; and translated to regular English as "..."
;; which means that (select #' (lambda (cd) (equal (getf cd :title) "Lyle Lovett"))) will return
;; the same record.
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; another way of doing the above, but a little prettier according to the author of the text
;; The use of this method  at the REPL is (select (artist-selector "whichever artist"))
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; a method of setting up a function allowing the user to choose any or a combination of
;; parameters for a search using 'where'.
;; Such as (select (where :rating 8)) or
;; (select (where :rating 8 :ripped nil))
;; read over http://www.gigamonkeys.com/book/practical-a-simple-database.html "Querying
;; the Database" again to better understand what's going on here.
;;(defun where (&key title artist rating (ripped nil ripped-p))
;;  #'(lambda (cd)
;;      (and
;;       (if title    (equal (getf cd :title)  title)  t)
;;       (if artist   (equal (getf cd :artist) artist) t)
;;       (if rating   (equal (getf cd :rating) rating) t)
;;       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; A section allowing updates to the db
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

;; USE CAUTION
;; a function to delete rows from the database
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; pure fucking magic from the Removing Duplication and Winning Big section
(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

;; wraps results from make-comparisons-list in an AND and an anonymous function
;; this code is where it needs to be, and doing what is expected, but, I had to comment
;; out the entire (where) function above to get around an error message in the REPL
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
