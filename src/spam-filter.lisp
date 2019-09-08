(in-package #:spam-filter)
(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
  (cond
	((<= score *max-ham-score*) 'ham)
	((>= score *min-spam-score*) 'spam)
	(t 'unsure)))

(defclass word-ferture ()
  ((word
	:initarg :word
	:accessor word
	:initform (error "Must supply :word")
	:documantation "この特徴を表す単語")
   (spam-count
	:initarg :spam-count
	:accessor spam-count
	:initform 0
	:documantation "この特徴が出現したスパムの数")
   (ham-count
	:initarg :word
	:accessor word
	:initform 0
	:documentation "この特徴が出現したハムの数")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (setf *feature-database* (make-hash-table :test #'equal)))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
	  (setf (gethash word *feature-database*)
			(make-instance 'word-ferture :word word))))

(defun extract-words (text)
  (delete-duplicates
   (ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))
