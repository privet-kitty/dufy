;;;
;;; General routines and constants
;;;

(in-package :dufy)

(define-constant TWO-PI (float (+ PI PI) 1d0))

(deftype single-valued-function () '(function * (values t &optional)))
(deftype spectrum-function () '(function * (values double-float &optional)))
(deftype matrix33 () '(simple-array double-float (3 3)))

(defun empty-function ()
  "Used instead of NIL."
  0d0)


;;;
;;; General macros
;;;

(defmacro subseq-values (start end number form)
  (let ((vars (loop for i from 0 below number collect (gensym))))
    `(multiple-value-bind ,vars ,form
       (declare (ignore ,@(subseq vars 0 start)
                        ,@(subseq vars end number)))
       (values ,@(subseq vars start end)))))

(defmacro with-double-float (vars &body body)
  "Ensures that variables are double-float."
  (labels ((expand (var-lst)
	     (if (null var-lst)
		 nil
		 (cons `(,(car var-lst) (float ,(car var-lst) 1d0))
		       (expand (cdr var-lst))))))
    `(let ,(expand vars)
       (declare (type double-float ,@vars))
       ,@body)))


(defmacro simple-time (&body body)
  "For devel. Simpler alternative to TIME."
  (let ((start (gensym)))
    `(let ((,start (get-internal-run-time)))
       ,@body
       (/ (float (- (get-internal-run-time) ,start) 1d0)
	  internal-time-units-per-second))))


(defmacro time-after-gc (&body body)
  `(progn
    #+sbcl(sb-ext:gc :full t)
    (time ,@body)))

#+sbcl
(defmacro with-profile (&body body)
  "For devel."
  `(progn (sb-profile:profile "DUFY")
	  ,@body
	  (sb-profile:report :print-no-call-list nil)
	  (sb-profile:unprofile "DUFY")))



;;; Comparison operators

(defun nearly= (threshold number &rest more-numbers)
  (if (null more-numbers)
      t
      (and (<= (abs (- number (car (the cons more-numbers)))) threshold)
	   (apply #'nearly= threshold more-numbers))))

(defun nearly-equal (threshold lst1 lst2)
  (if (null lst1)
      t
      (and (nearly= threshold (car lst1) (car lst2))
	   (nearly-equal threshold (cdr lst1) (cdr lst2)))))

(defun nearly<= (threshold number &rest more-numbers)
  (if (null more-numbers)
      t
      (and (<= (- number (car (the cons more-numbers))) threshold)
	   (apply #'nearly<= threshold more-numbers))))




;;;
;;; Some arithmetic in a circle group
;;;


(declaim (inline subtract-with-mod
		 circular-nearer
		 circular-clamp
		 circular-lerp
		 circular-lerp-loose
		 ))
(defun subtract-with-mod (x y &optional (divisor TWO-PI))
  "(X - Y) mod DIVISOR."
  (mod (- x y) divisor))

(defun circular-nearer (theta1 x theta2 &optional (perimeter TWO-PI))
  "Compares counterclockwise distances between THETA1 and X and
between X and THETA2; returns THETA1 or THETA2, whichever is nearer."
  (if (<= (subtract-with-mod x theta1 perimeter)
	  (subtract-with-mod theta2 x perimeter))
      theta1
      theta2))

(defun circular-clamp (number min max &optional (perimeter TWO-PI))
  "A clamp function in a circle group. If NUMBER is not in
the (counterclockwise) closed interval [MIN, MAX], CIRCULAR-CLAMP
returns MIN or MAX, whichever is nearer to NUMBER."
  (let ((number$ (mod number perimeter))
	(min$ (mod min perimeter))
	(max$ (mod max perimeter)))
    (if (<= min$ max$)
	(if (<= min$ number$ max$)
	    number$ ; [min, number, max]
	    (circular-nearer max$ number$ min$))   ; [min, max, number] or [number, min, max]
	(if (or (<= number$ max$)  (<= min$ number$))
	    number$ ;[number, max, min] or [max, min, number]
	    (circular-nearer max$ number$ min$))))) ; [max, number, min]

(defun circular-lerp-loose (coef theta1 theta2 &optional (perimeter TWO-PI))
  "Counterclockwise linear interpolation from THETA1 to THETA2 in a
circle group. There is a possibility that the return value slightly
exceeds the interval [THETA1, THETA2], due to floating-point error. If
that is incovenient, use CIRCULAR-LERP instead."
  (let ((dtheta (subtract-with-mod theta2 theta1 perimeter)))
    (mod (+ theta1 (* dtheta coef)) perimeter)))

(defun circular-lerp (coef theta1 theta2 &optional (perimeter TWO-PI))
  "Counterclockwise linear interpolation from THETA1 to THETA2 in a
circle group. It doesn't exceed the given interval from THETA1 to
THETA2, if COEF is in [0, 1]. It is, however, slower than
CIRCULAR-LERP-LOOSE."
  (let ((dtheta (subtract-with-mod theta2 theta1 perimeter)))
    (circular-clamp (+ theta1 (* dtheta coef))
		    theta1
		    theta2
		    perimeter)))

(defun circular-member (x theta1 theta2 &optional (perimeter TWO-PI))
  "Returns true, if X is in the counterclockwise closed interval [THETA1,
THETA2] in a circle group."
  (let ((x-m (mod x perimeter))
	(theta1-m (mod theta1 perimeter))
	(theta2-m (mod theta2 perimeter)))
    (if (<= theta1-m theta2-m)
	(and (<= theta1-m x-m)
	     (<= x-m theta2))
	(or (<= theta1-m x-m)
	    (<= x-m theta2)))))



;;;
;;; Matrix operation
;;;

(defparameter +identity-matrix+
  (make-array '(3 3) :element-type 'double-float
	      :initial-contents '((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 1d0))))

(defparameter +empty-matrix+
  (make-array '(3 3) :element-type 'double-float)
  "Used instead of NIL")

(declaim (inline determinant))
(defun determinant (mat)
  (declare (optimize (speed 3) (safety 1))
	   (matrix33 mat))
  (+ (* (aref mat 0 0) (aref mat 1 1) (aref mat 2 2))
     (* (aref mat 1 0) (aref mat 2 1) (aref mat 0 2))
     (* (aref mat 2 0) (aref mat 0 1) (aref mat 1 2))
     (- (* (aref mat 0 0) (aref mat 2 1) (aref mat 1 2)))
     (- (* (aref mat 2 0) (aref mat 1 1) (aref mat 0 2)))
     (- (* (aref mat 1 0) (aref mat 0 1) (aref mat 2 2)))))

(defun invert-matrix33 (mat)
  (declare (optimize (speed 3) (safety 1))
	   (matrix33 mat))
  (let ((/det (/ (determinant mat)))
	(invmat (make-array '(3 3) :element-type 'double-float)))
    (setf (aref invmat 0 0) (* (- (* (aref mat 1 1) (aref mat 2 2))
				  (* (aref mat 1 2) (aref mat 2 1))) /det)
	  (aref invmat 0 1) (* (- (* (aref mat 0 2) (aref mat 2 1))
				  (* (aref mat 0 1) (aref mat 2 2))) /det)
	  (aref invmat 0 2) (* (- (* (aref mat 0 1) (aref mat 1 2))
				  (* (aref mat 0 2) (aref mat 1 1))) /det)
	  (aref invmat 1 0) (* (- (* (aref mat 1 2) (aref mat 2 0))
				  (* (aref mat 1 0) (aref mat 2 2))) /det)
	  (aref invmat 1 1) (* (- (* (aref mat 0 0) (aref mat 2 2))
				  (* (aref mat 0 2) (aref mat 2 0))) /det)
	  (aref invmat 1 2) (* (- (* (aref mat 0 2) (aref mat 1 0))
				  (* (aref mat 0 0) (aref mat 1 2))) /det)
	  (aref invmat 2 0) (* (- (* (aref mat 1 0) (aref mat 2 1))
				  (* (aref mat 1 1) (aref mat 2 0))) /det)
	  (aref invmat 2 1) (* (- (* (aref mat 0 1) (aref mat 2 0))
				  (* (aref mat 0 0) (aref mat 2 1))) /det)
	  (aref invmat 2 2) (* (- (* (aref mat 0 0) (aref mat 1 1))
				  (* (aref mat 0 1) (aref mat 1 0))) /det))
    invmat))

(defun bench-invmat (&optional (num 3000000))
  (time (let ((mat (make-array '(3 3)
			       :element-type 'double-float
			       :initial-contents '((1d0 2d0 3d0) (4d0 5d0 6d0) (7d0 8d0 10d0)))))
	  (dotimes (i num)
	    (invert-matrix33 mat)))))

(declaim (ftype (function * (values double-float double-float double-float)) multiply-mat-vec)
	 (inline multiply-mat-vec))
(defun multiply-mat-vec (matrix x y z)
  (declare (optimize (speed 3) (safety 1))
  	   (matrix33 matrix))
  (with-double-float (x y z)
    (values (+ (* x (aref matrix 0 0))
	       (* y (aref matrix 0 1))
	       (* z (aref matrix 0 2)))
	    (+ (* x (aref matrix 1 0))
	       (* y (aref matrix 1 1))
	       (* z (aref matrix 1 2)))
	    (+ (* x (aref matrix 2 0))
	       (* y (aref matrix 2 1))
	       (* z (aref matrix 2 2))))))

(defmacro dotimes-unroll ((var count &optional result) &body body)
  `(block nil
     ,@(loop for i from 0 below count
	  collect `(let ((,var ,i)) ,@body))
     ,result))

(declaim (inline multiply-mat-mat))
(defun multiply-mat-mat (mat1 mat2)
  (declare (optimize (speed 3) (safety 1))
  	   (matrix33 mat1 mat2))
  (let ((ret-mat (make-array '(3 3) :element-type 'double-float)))
    (dotimes-unroll (i 3)
      (dotimes-unroll (j 3)
	(setf (aref ret-mat i j)
	      (+ (* (aref mat1 i 0) (aref mat2 0 j))
		 (* (aref mat1 i 1) (aref mat2 1 j))
		 (* (aref mat1 i 2) (aref mat2 2 j))))))
    ret-mat))

(defun multiply-matrices (mat1 &rest mats)
  (declare (optimize (speed 3) (safety 1)))
  (if (null mats)
      mat1
      (multiply-mat-mat mat1
			(apply #'multiply-matrices (car mats) (cdr mats)))))


(defun bench-mult-mat (&optional (num 3000000))
  (time (let ((mat1 (make-array '(3 3)
				:element-type 'double-float
				:initial-contents '((1d0 2d0 3d0) (1d0 2d0 3d0) (4d0 5d0 6d0))))
	      (mat2 (make-array '(3 3)
				:element-type 'double-float
				:initial-contents '((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 -1d0)))))
	  (dotimes (x num)
	    (dufy::multiply-matrices mat1 mat2)))))

