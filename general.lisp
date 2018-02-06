;;; General routines and constants


(in-package :dufy)

(define-constant TWO-PI (+ PI PI))

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

(defparameter *empty-function* ())


;;;
;;; Some arithmetic in a circle group
;;;


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

(defun invert-matrix33 (mat)
  (let ((det (+ (* (aref mat 0 0) (aref mat 1 1) (aref mat 2 2))
		(* (aref mat 1 0) (aref mat 2 1) (aref mat 0 2))
		(* (aref mat 2 0) (aref mat 0 1) (aref mat 1 2))
		(- (* (aref mat 0 0) (aref mat 2 1) (aref mat 1 2)))
		(- (* (aref mat 2 0) (aref mat 1 1) (aref mat 0 2)))
		(- (* (aref mat 1 0) (aref mat 0 1) (aref mat 2 2)))))
	(invmat (make-array '(3 3) :element-type 'double-float)))
    (setf (aref invmat 0 0) (/ (- (* (aref mat 1 1) (aref mat 2 2))
				  (* (aref mat 1 2) (aref mat 2 1))) det)
	  (aref invmat 0 1) (/ (- (* (aref mat 0 2) (aref mat 2 1))
				  (* (aref mat 0 1) (aref mat 2 2))) det)
	  (aref invmat 0 2) (/ (- (* (aref mat 0 1) (aref mat 1 2))
				  (* (aref mat 0 2) (aref mat 1 1))) det)
	  (aref invmat 1 0) (/ (- (* (aref mat 1 2) (aref mat 2 0))
				  (* (aref mat 1 0) (aref mat 2 2))) det)
	  (aref invmat 1 1) (/ (- (* (aref mat 0 0) (aref mat 2 2))
				  (* (aref mat 0 2) (aref mat 2 0))) det)
	  (aref invmat 1 2) (/ (- (* (aref mat 0 2) (aref mat 1 0))
				  (* (aref mat 0 0) (aref mat 1 2))) det)
	  (aref invmat 2 0) (/ (- (* (aref mat 1 0) (aref mat 2 1))
				  (* (aref mat 1 1) (aref mat 2 0))) det)
	  (aref invmat 2 1) (/ (- (* (aref mat 0 1) (aref mat 2 0))
				  (* (aref mat 0 0) (aref mat 2 1))) det)
	  (aref invmat 2 2) (/ (- (* (aref mat 0 0) (aref mat 1 1))
				  (* (aref mat 0 1) (aref mat 1 0))) det))
	  invmat))

(defun multiply-matrix-and-vec (matrix x y z)
  (list (+ (* x (aref matrix 0 0))
	   (* y (aref matrix 0 1))
	   (* z (aref matrix 0 2)))
	(+ (* x (aref matrix 1 0))
	   (* y (aref matrix 1 1))
	   (* z (aref matrix 1 2)))
	(+ (* x (aref matrix 2 0))
	   (* y (aref matrix 2 1))
	   (* z (aref matrix 2 2)))))
