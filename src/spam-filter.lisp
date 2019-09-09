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

(defmethod print-object ((object word-ferture) stream)
  (print-unreadable-object (object stream :type t)
	(with-slots (word ham-count spam-count) object
	  (format stream "~s :hams ~d :spams ~d" word ham-count spam-count))))

(defun train (text type)
  (dolist (feature (extract-features text))
	(increment-count feature type))
  (increment-total-count type))

(defun increment-count (feature type)
  (ecase type
	(ham (incf (ham-count feature)))
	(spam (incf (spam-count feature)))))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun increment-total-count (type)
  (ecase type
	(ham (incf *total-hams*))
	(spam (incf *total-spams*))))

(defun clear-database ()
  (setf
   *feature-database* (make-hash-table :test #'equal)
   *total-hams* 0
   *total-spams* 0))

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
	(let ((spma-frequency (/ spam-count (max 1 *total-spams*)))
		  (ham-frequency (/ ham-count (max 1 *total-hams*))))
	  (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional
											(assumed-probability 1/2)
											(weight 1))
  (let ((basic-probability (spam-probability feature))
		(data-points (+ (spam-count feature) (ham-count feature))))
	(/ (+ (* weight assmed-probability)
		  (* data-points basic-probability))
	   (+ weight data-points))))

(defun score (features)
  (let ((spam-probs  ())
		(ham-probs ())
		(number-of-probs 0))
	(dolist (feature features)
	  (unless (untrained-p feature)
		(let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
		  (push spam-prob spam-probs)
		  (push (- 1.0d0 spam-prob) ham-probs)
		  (incf number-of-probs))))
	(let ((h (- 1 (fisher spam-probs number-of-probs)))
		  (s (- 1 (fisher ham-probs number-of-probs))))
	  (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
	(and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  (inverse-chi-square
   (* -2 (log (reduce #'* probs)))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
	  for i below (/ degrees-of-freedom 2)
	  for prob = (exp (- m)) then (* prob (/ m i))
	  summing prob)
   1.0))
