;;;
;;; LMS, chromatic adaptation
;;;

(in-package :dufy)

(defstruct (cat (:constructor $make-cat))
  "Model of chromatic adaptation transformation. Currently only linear
models are available."
  (matrix +empty-matrix+ :type (simple-array double-float (3 3)))
  (inv-matrix +empty-matrix+ :type (simple-array double-float (3 3))))

(defun make-cat (mat)
  "Generates a (linear) CAT model by a 3*3 matrix."
  (let ((mat-arr (make-array '(3 3)
			     :element-type 'double-float
			     :initial-contents mat)))
    ($make-cat :matrix mat-arr
	       :inv-matrix (invert-matrix33 mat-arr))))

(defparameter +bradford+
  (make-cat '((0.8951d0 0.2664d0 -0.1614d0)
	      (-0.7502d0 1.7135d0 0.0367d0)
	      (0.0389d0 -0.0685d0 1.0296d0))))

(defparameter +xyz-scaling+
  (make-cat '((1d0 0d0 0d0)
	      (0d0 1d0 0d0)
	      (0d0 0d0 1d0))))

(defparameter +von-kries+
  (make-cat '((0.4002d0 0.7076d0 -0.0808d0)
	      (-0.2263d0 1.1653d0 0.0457d0)
	      (0.0000d0 0.0000d0 0.9182d0))))

(defparameter +cmccat97+
  (make-cat '((0.8951d0 -0.7502d0 0.0389d0)
	      (0.2664d0 1.7135d0 0.0685d0)
	      (-0.1614d0 0.0367d0 1.0296d0))))

(defparameter +cmccat2000+
  (make-cat '((0.7982d0 0.3389d0 -0.1371d0)
	      (-0.5918d0 1.5512d0 0.0406d0)
	      (0.0008d0 0.0239d0 0.9753d0))))

(defparameter +cat97s-revised+
  (make-cat '((0.8562d0 0.3372d0 -0.1934d0)
	      (-0.8360d0 1.8327d0 0.0033d0)
	      (0.0357d0 -0.0469d0 1.0112d0)))
  "Fairchild, Mark D. (2001).\"A Revision of CIECAM97s for Practical Applications\"
http://rit-mcsl.org/fairchild//PDFs/PAP10.pdf")

(defparameter +cat02+
  (make-cat '((0.7328d0 0.4296d0 -0.1624d0)
	      (-0.7036d0 1.6975d0 0.0061d0)
	      (0.0030d0 0.0136d0 0.9834d0))))


(declaim (inline xyz-to-lms))
(defun xyz-to-lms (x y z &key (illuminant nil) (cat +bradford+))
  "Note: The default illuminant is **not** D65; if ILLUMINANT is NIL,
the transform is virtually equivalent to that of illuminant E. "
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (x y z)
    (if illuminant
	(let* ((mat (cat-matrix cat))
	       (factor-l (+ (* (illuminant-x illuminant) (aref mat 0 0))
			    (aref mat 0 1)
			    (* (illuminant-z illuminant) (aref mat 0 2))))
	       (factor-m (+ (* (illuminant-x illuminant) (aref mat 1 0))
			    (aref mat 1 1)
			    (* (illuminant-z illuminant) (aref mat 1 2))))
	       (factor-s (+ (* (illuminant-x illuminant) (aref mat 2 0))
			    (aref mat 2 1)
			    (* (illuminant-z illuminant) (aref mat 2 2)))))
	  (multiple-value-bind (l m s)
	      (multiply-mat-vec mat x y z)
	    (values (/ l factor-l)
		    (/ m factor-m)
		    (/ s factor-s))))
	(multiply-mat-vec (cat-matrix cat) x y z))))
	    

(declaim (inline lms-to-xyz))
(defun lms-to-xyz (l m s &key (illuminant nil) (cat +bradford+))
  "Note: The default illuminant is **not** D65; if ILLUMINANT is NIL,
the transform is virtually equivalent to that of illuminant E. "
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (l m s)
    (if illuminant
	(let* ((mat (cat-matrix cat))
	       (factor-l (+ (* (illuminant-x illuminant) (aref mat 0 0))
			    (aref mat 0 1)
			    (* (illuminant-z illuminant) (aref mat 0 2))))
	       (factor-m (+ (* (illuminant-x illuminant) (aref mat 1 0))
			    (aref mat 1 1)
			    (* (illuminant-z illuminant) (aref mat 1 2))))
	       (factor-s (+ (* (illuminant-x illuminant) (aref mat 2 0))
			    (aref mat 2 1)
			    (* (illuminant-z illuminant) (aref mat 2 2)))))
	  (multiply-mat-vec (cat-inv-matrix cat)
			    (* l factor-l)
			    (* m factor-m)
			    (* s factor-s)))
	(multiply-mat-vec (cat-inv-matrix cat) l m s))))
  

(defun calc-cat-matrix  (from-illuminant to-illuminant &optional (cat +bradford+))
  "Returns a 3*3 chromatic adaptation matrix between FROM-ILLUMINANT
and TO-ILLUMINANT in XYZ space."
  (declare (optimize (speed 3) (safety 1)))
  (let ((from-white-x (illuminant-x from-illuminant))
	(from-white-y (illuminant-y from-illuminant))
	(from-white-z (illuminant-z from-illuminant))
	(to-white-x (illuminant-x to-illuminant))
	(to-white-y (illuminant-y to-illuminant))
	(to-white-z (illuminant-z to-illuminant))
	(tmatrix (cat-matrix cat)))
    (let ((source-L (+ (* from-white-x (aref tmatrix 0 0))
		       (* from-white-y (aref tmatrix 0 1))
		       (* from-white-z (aref tmatrix 0 2))))
	  (source-M (+ (* from-white-x (aref tmatrix 1 0))
		       (* from-white-y (aref tmatrix 1 1))
		       (* from-white-z (aref tmatrix 1 2))))
	  (source-S (+ (* from-white-x (aref tmatrix 2 0))
		       (* from-white-y (aref tmatrix 2 1))
		       (* from-white-z (aref tmatrix 2 2))))
	  (dest-L (+ (* to-white-x (aref tmatrix 0 0))
		     (* to-white-y (aref tmatrix 0 1))
		     (* to-white-z (aref tmatrix 0 2))))
	  (dest-M (+ (* to-white-x (aref tmatrix 1 0))
		     (* to-white-y (aref tmatrix 1 1))
		     (* to-white-z (aref tmatrix 1 2))))
	  (dest-S (+ (* to-white-x (aref tmatrix 2 0))
		     (* to-white-y (aref tmatrix 2 1))
		     (* to-white-z (aref tmatrix 2 2)))))
      (let ((L-ratio (/ dest-L source-L))
	    (M-ratio (/ dest-M source-M))
	    (S-ratio (/ dest-S source-S))
	    (matrix1 (make-array '(3 3) :element-type 'double-float)))
	(setf (aref matrix1 0 0) (* L-ratio (aref tmatrix 0 0)))
	(setf (aref matrix1 0 1) (* L-ratio (aref tmatrix 0 1)))
	(setf (aref matrix1 0 2) (* L-ratio (aref tmatrix 0 2)))
	(setf (aref matrix1 1 0) (* M-ratio (aref tmatrix 1 0)))
	(setf (aref matrix1 1 1) (* M-ratio (aref tmatrix 1 1)))
	(setf (aref matrix1 1 2) (* M-ratio (aref tmatrix 1 2)))
	(setf (aref matrix1 2 0) (* S-ratio (aref tmatrix 2 0)))
	(setf (aref matrix1 2 1) (* S-ratio (aref tmatrix 2 1)))
	(setf (aref matrix1 2 2) (* S-ratio (aref tmatrix 2 2)))
	(multiply-mat-mat (cat-inv-matrix cat)
			  matrix1)))))


(declaim (ftype (function * (function * (values double-float double-float double-float &optional))) gen-cat-function))
(defun gen-cat-function (from-illuminant to-illuminant &key (cat +bradford+) (target :xyz))
  "Returns a chromatic adaptation function. An example for xyY spaces:
> (funcall (gen-cat-function +illum-a+ +illum-e+ :target :xyy) 0.44757 0.40745 1)
=> 0.3333333257806802d0
0.33333331733957294d0
1.0000000029690765d0 ; transforms white point

TARGET can be :XYZ, :XYY, :LAB, :LUV, :LCHAB or :LCHUV. If you want to
choose RGB as target, you should use GEN-RGBSPACE-CHANGER instead.
"
  (declare (optimize (speed 3) (safety 1)))
  (macrolet ((gen-lambda (args repr)
	       (let* ((term (symbol-name repr))
		      (xyz-to-repr (intern (format nil "XYZ-TO-~A" term) :dufy))
		      (repr-to-xyz (intern (format nil "~A-TO-XYZ" term) :dufy)))
		 `#'(lambda ,args
		      (with-double-float ,args
			(multiple-value-call #',xyz-to-repr
			  (multiple-value-call #'multiply-mat-vec
			    mat
			    (,repr-to-xyz ,@args from-illuminant))
			  to-illuminant))))))
    (let ((mat (calc-cat-matrix from-illuminant to-illuminant cat)))
      (ecase target
	(:xyz
	 #'(lambda (x y z)
	     (with-double-float (x y z)
	       (multiply-mat-vec mat x y z))))
	(:xyy
	 #'(lambda (small-x small-y y)
	     (with-double-float (small-x small-y y)
	       (multiple-value-call #'xyz-to-xyy
		 (multiple-value-call #'multiply-mat-vec
		   mat
		   (xyy-to-xyz small-x small-y y))))))
	(:lab (gen-lambda (lstar astar bstar) :lab))
	(:lchab (gen-lambda (lstar cstarab hab) :lchab))
	(:luv (gen-lambda (lstar ustar vstar) :luv))
	(:lchuv (gen-lambda (lstar cstaruv huv) :lchuv))))))


(defmacro def-cat-function (name from-illuminant to-illuminant &key (cat +bradford+) (target :xyz))
  "DEF-macro of GEN-CAT-FUNCTION.
> (def-cat-function d65-to-e +illum-d65+ +illum-e+ :target :xyz)
> (d65-to-e 0.9504d0 1.0d0 1.0889d0)
=> 0.9999700272441295d0
0.999998887365445d0
0.9999997282885571d0

TARGET can be :XYZ, :XYY, :LAB, :LUV, :LCHAB or :LCHUV."
  (macrolet ((def-converter (args target)
	       (let* ((term (symbol-name target))
		      (xyz-to-target (intern (format nil "XYZ-TO-~A" term) :dufy))
		      (target-to-xyz (intern (format nil "~A-TO-XYZ" term) :dufy)))
		 ``(defun ,,'name ,',args
		     (declare (optimize (speed 3) (safety 1)))
		     (let ((mat (load-time-value
				(calc-cat-matrix ,,'from-illuminant
						 ,,'to-illuminant
						 ,,'cat)
				t)))
		      (with-double-float ,',args
			(multiple-value-call #',',xyz-to-target
			  (multiple-value-call #'multiply-mat-vec
			    mat
			    (,',target-to-xyz ,@',args ,,'from-illuminant))
			  ,,'to-illuminant)))))))
    (unless (and (symbolp from-illuminant)
                 (symbolp to-illuminant))
      (error "FROM-ILLUMINANT and TO-ILLUMINANT must be symbols"))
    `(progn
       (declaim (ftype (function (t t t)
				 (values double-float double-float double-float &optional))
		       ,name))
       ,(ecase target
	  (:xyz `(defun ,name (x y z)
		   (declare (optimize (speed 3) (safety 1)))
		   (let ((mat (load-time-value
			       (calc-cat-matrix ,from-illuminant ,to-illuminant ,cat)
			       t)))
		     (with-double-float (x y z)
		       (multiply-mat-vec mat x y z)))))
	  (:xyy `(defun ,name (small-x small-y y)
		   (declare (optimize (speed 3) (safety 1)))
		   (let ((mat (load-time-value
			       (calc-cat-matrix ,from-illuminant ,to-illuminant ,cat)
			       t)))
		     (with-double-float (small-x small-y y)
		       (multiple-value-call #'xyz-to-xyy
			 (multiple-value-call #'multiply-mat-vec
			   mat
			   (xyy-to-xyz small-x small-y y)))))))
	  (:lab (def-converter (lstar astar bstar) :lab))
	  (:lchab (def-converter (lstar cstarab hab) :lchab))
	  (:luv (def-converter (lstar ustar vstar) :luv))
	  (:lchuv (def-converter (lstar cstaruv huv) :lchuv))))))



(declaim (inline calc-cat-matrix-for-lrgb))
(defun calc-cat-matrix-for-lrgb (from-rgbspace to-rgbspace &optional (cat +bradford+))
  (multiply-matrices (rgbspace-from-xyz-matrix to-rgbspace)
		     (calc-cat-matrix (rgbspace-illuminant from-rgbspace)
				      (rgbspace-illuminant to-rgbspace)
				      cat)
		     (rgbspace-to-xyz-matrix from-rgbspace)))

(defun gen-rgbspace-changer (from-rgbspace to-rgbspace &optional (target :lrgb) (cat +bradford+))
  "Returns a function for changing RGB working space.
> (funcall (gen-rgbspace-changer +srgb+ +adobe+ :rgb) 0 1 0)
=> 0.28488056007809415d0
1.0000000000000002d0
0.041169364382683385d0 ; change from sRGB to Adobe RGB.
TARGET can be :LRGB, :RGB, :QRGB or :INT.

Note about clamping:
LRGB case: no clamping;
RGB case: no clamping;
QRGB case: with clamping;
INT case: with clamping."
  (declare (optimize (speed 3) (safety 1)))
  (let ((mat (calc-cat-matrix-for-lrgb from-rgbspace to-rgbspace cat)))
    (ecase target
      (:lrgb #'(lambda (lr lg lb)
		 (multiply-mat-vec mat (float lr 1d0) (float lg 1d0) (float lb 1d0))))
      (:rgb #'(lambda (r g b)
		(multiple-value-call #'lrgb-to-rgb
		  (multiple-value-call #'multiply-mat-vec
		    mat
		    (rgb-to-lrgb (float r 1d0) (float g 1d0) (float b 1d0) from-rgbspace))
		  to-rgbspace)))
      (:qrgb #'(lambda (qr qg qb)
		 (multiple-value-call #'lrgb-to-qrgb
		   (multiple-value-call #'multiply-mat-vec
		     mat
		     (qrgb-to-lrgb qr qg qb from-rgbspace))
		   :rgbspace to-rgbspace)))
      (:int #'(lambda (int)
		(multiple-value-call #'lrgb-to-int
		  (multiple-value-call #'multiply-mat-vec
		    mat
		    (int-to-lrgb int from-rgbspace))
		  to-rgbspace))))))

