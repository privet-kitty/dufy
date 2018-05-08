(in-package :dufy-internal)

;;;
;;; Matrix operations
;;;

(deftype matrix33 () '(simple-array double-float (3 3)))

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

(declaim (ftype (function * (values double-float double-float double-float &optional)) multiply-mat-vec)
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


(defun bench-mult-mat (&optional (num 20000000))
  (time-after-gc
   (let ((mat1 (make-array '(3 3)
                           :element-type 'double-float
                           :initial-contents '((1d0 2d0 3d0) (1d0 2d0 3d0) (4d0 5d0 6d0))))
         (mat2 (make-array '(3 3)
                           :element-type 'double-float
                           :initial-contents '((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 -1d0)))))
     (dotimes (x num)
       (multiply-matrices mat1 mat2)))))

