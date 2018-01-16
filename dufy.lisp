;; Core Routines of Dufy

(in-package :dufy)

(define-constant TWO-PI (+ PI PI))

(defun nearly= (threshold number &rest more-numbers)
  (if (null more-numbers)
      t
      (and (<= (abs (- number (car (the cons more-numbers)))) threshold)
	   (apply #'nearly= threshold more-numbers))))

(defun nearly<= (threshold number &rest more-numbers)
  (if (null more-numbers)
      t
      (and (<= (- number (car (the cons more-numbers))) threshold)
	   (apply #'nearly<= threshold more-numbers))))

(defun subtract-with-mod (x y &optional (divisor TWO-PI))
  "(x mod divisor) - (y mod divisor) = (x - y) mod divisor."
  (mod (- x y) divisor))

(defun circular-nearer (theta1 x theta2 &optional (perimeter TWO-PI))
  "Compares counterclockwise distances between THETA1 and X and
between X and THETA2; returns THETA1 or THETA2, whichever is nearer."
  (if (<= (subtract-with-mod x theta1 perimeter) (subtract-with-mod theta2 x perimeter))
      theta1
      theta2))

(defun circular-clamp (number min max &optional (perimeter TWO-PI))
  "A clamp function in a circle group. If NUMBER is not in
the (counterclockwise) closed interval [min, max], CIRCULAR-CLAMP
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

(defun circular-lerp-loose (theta1 theta2 coef &optional (perimeter TWO-PI))
  "Counterclockwise linear interpolation from THETA1 to THETA2 in a
circle group. There is a possibility that the return value slightly
exceeds the interval [THETA1, THETA2], due to floating-point error. If
that is incovenient, use CIRCULAR-LERP instead."
  (let ((dtheta (subtract-with-mod theta2 theta1 perimeter)))
    (mod (+ theta1 (* dtheta coef)) perimeter)))

(defun circular-lerp (theta1 theta2 coef &optional (perimeter TWO-PI))
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


;;; Standard Illuminant, XYZ, xyY
;;; The nominal range of X, Y, Z, x, y is always [0, 1].
(defstruct illuminant
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (largex 0.0 :type double-float)
  (largey 0.0 :type double-float)
  (largez 0.0 :type double-float))

(defun xyy-to-xyz (x y largey)
  (if (zerop y)
      (list 0d0 0d0 0d0)
      (list (/ (* x largey) y) 
	    largey
	    (/ (* (- 1 x y) largey) y))))


(defun xyz-to-xyy (x y z)
  (let ((sum (+ x y z)))
    (if (= sum 0)
	(list 0d0 0d0 y)
	(list (/ x sum) (/ y sum) y))))


(defmacro new-illuminant (x y)
  (let ((largex (gensym))
	(largey (gensym))
	(largez (gensym)))
    `(destructuring-bind (,largex ,largey ,largez) (xyy-to-xyz ,x ,y 1d0)
       (make-illuminant :x (float ,x 1d0) :y (float ,y 1d0)
			:largex (float ,largex 1d0)
			:largey (float ,largey 1d0)
			:largez (float ,largez 1d0)))))

(defparameter illum-a (new-illuminant 0.44757d0 0.40745d0))
(defparameter illum-c (new-illuminant 0.31006d0 0.31616d0))
(defparameter illum-d50 (new-illuminant 0.34567d0 0.35850d0))
(defparameter illum-d65 (new-illuminant 0.31271d0 0.32902d0))
(defparameter illum-e (new-illuminant #.(float 1/3 1d0) #.(float 1/3 1d0)))

(defparameter bradford
  (make-array '(3 3)
	      :element-type 'double-float
	      :initial-contents  '((0.8951d0 0.2664d0 -0.1614d0)
				   (-0.7502d0 1.7135d0 0.0367d0)
				   (0.0389d0 -0.0685d0 1.0296d0))))
(defparameter xyz-scaling
  (make-array '(3 3)
	      :element-type 'double-float
	      :initial-contents  '((1d0 0d0 0d0)
				   (0d0 1d0 0d0)
				   (0d0 0d0 1d0))))
(defparameter von-kries
  (make-array '(3 3)
	      :element-type 'double-float
	      :initial-contents  '((0.4002d0 0.7076d0 -0.0808d0)
				   (-0.2263d0 1.1653d0 0.0457d0)
				   (0.0000d0 0.0000d0 0.9182d0))))

(defparameter cmccat97
  (make-array '(3 3)
	      :element-type 'double-float
	      :initial-contents  '((0.8951d0 -0.7502d0 0.0389d0)
				   (0.2664d0 1.7135d0 0.0685d0)
				   (-0.1614d0 0.0367d0 1.0296d0))))

(defparameter cmccat2000
  (make-array '(3 3)
	      :element-type 'double-float
	      :initial-contents  '((0.7982d0 0.3389d0 -0.1371d0)
				   (-0.5918d0 1.5512d0 0.0406d0)
				   (0.0008d0 0.0239d0 0.9753d0))))

(defparameter cat02
  (make-array '(3 3)
	      :element-type 'double-float
	      :initial-contents '((0.7328d0 0.4296d0 -0.1624d0)
				  (-0.7036d0 1.6975d0 0.0061d0)
				  (0.0030d0 0.0136d0 0.9834d0))))

;; (defparameter inverted-bradford-matrix
;;   #2A((0.98699290546671d0 -0.147054256421d0 0.15996265166373d0)
;;       (0.4323052697234d0 0.51836027153678d0 0.049291228212856d0)
;;       (-0.0085286645751773d0 0.040042821654085d0 0.96848669578755d0)))

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

(defun calc-ca-matrix  (from-illuminant to-illuminant &optional (tmatrix bradford))
  (let ((from-white-x (illuminant-largex from-illuminant))
	(from-white-y (illuminant-largey from-illuminant))
	(from-white-z (illuminant-largez from-illuminant))
	(to-white-x (illuminant-largex to-illuminant))
	(to-white-y (illuminant-largey to-illuminant))
	(to-white-z (illuminant-largez to-illuminant)))
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
	(let ((inv-tmatrix (invert-matrix33 tmatrix))
	      (matrix2 (make-array '(3 3) :element-type 'double-float)))
	  (dotimes (i 3)
	    (dotimes (j 3)
	      (do ((sum 0 (+ sum (* (aref inv-tmatrix i k)
				    (aref matrix1 k j))))
		   (k 0 (1+ k)))
		  ((= k 3) 
		   (setf (aref matrix2 i j) sum)))))
	  matrix2)))))


;; get a function for chromatic adaptation
(declaim (ftype (function * function) gen-ca-converter))
(defun gen-ca-converter (from-illuminant to-illuminant &optional (tmatrix bradford))
  (let ((mat (calc-ca-matrix from-illuminant to-illuminant tmatrix)))
    #'(lambda (x y z)
	(multiply-matrix-and-vec mat x y z))))

(defun gen-ca-converter-xyy (from-illuminant to-illuminant &optional (tmatrix bradford))
  (let ((mat (calc-ca-matrix from-illuminant to-illuminant tmatrix)))
    #'(lambda (x y largey)
	(apply #'xyz-to-xyy
	       (apply (curry #'multiply-matrix-and-vec mat)
		      (xyy-to-xyz x y largey))))))


;;; RGB Color Space
(defparameter identity-matrix
  (make-array '(3 3) :element-type 'double-float
	      :initial-contents '((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 1d0))))

(defun gen-linearizer (gamma)
  (let ((gamma$ (float gamma 1d0)))
    #'(lambda (x) (expt (clamp x 0d0 1d0) gamma$))))

(defun gen-delinearizer (gamma)
  (let ((gamma-recipro (/ 1d0 (float gamma 1d0))))
    #'(lambda (x) (expt (clamp x 0d0 1d0) gamma-recipro))))

(defstruct (rgbspace (:copier nil))
  (xr 0d0 :type double-float) (yr 0d0 :type double-float)
  (xg 0d0 :type double-float) (yg 0d0 :type double-float)
  (xb 0d0 :type double-float) (yb 0d0 :type double-float)
  (illuminant illum-d65)
  (linearizer #'identity)
  (delinearizer #'identity)
  (to-xyz-matrix identity-matrix :type (simple-array double-float (3 3)))
  (from-xyz-matrix identity-matrix :type (simple-array double-float (3 3))))

(defun new-rgbspace (xr yr xg yg xb yb &key (illuminant illum-d65) (linearizer #'identity) (delinearizer #'identity))
  (let ((coordinates
	 (make-array '(3 3)
		     :element-type 'double-float
		     :initial-contents (list (list xr xg xb)
					     (list yr yg yb)
					     (list (- 1d0 xr yr) (- 1d0 xg yg) (- 1d0 xb yb))))))
    (destructuring-bind (sr sg sb)
	(multiply-matrix-and-vec (invert-matrix33 coordinates)
				 (illuminant-largex illuminant)
				 (illuminant-largey illuminant)
				 (illuminant-largez illuminant))
      (let ((m
	     (make-array '(3 3)
			 :element-type 'double-float
			 :initial-contents (list (list (* sr (aref coordinates 0 0))
						       (* sg (aref coordinates 0 1))
						       (* sb (aref coordinates 0 2)))
						 (list (* sr (aref coordinates 1 0))
						       (* sg (aref coordinates 1 1))
						       (* sb (aref coordinates 1 2)))
						 (list (* sr (aref coordinates 2 0))
						       (* sg (aref coordinates 2 1))
						       (* sb (aref coordinates 2 2)))))))
	(make-rgbspace :xr xr :yr yr :xg xg :yg yg :xb xb :yb yb
		       :illuminant illuminant
		       :linearizer linearizer
		       :delinearizer delinearizer
		       :to-xyz-matrix m
		       :from-xyz-matrix (invert-matrix33 m))))))

(defun copy-rgbspace (rgbspace &optional (illuminant nil))
  "This copier can copy RGBSPACE with different ILLUMINANT. If
ILLUMINANT is nil, it is a trivial copier."
  (if illuminant
      (let ((ca-func (gen-ca-converter (rgbspace-illuminant rgbspace) illuminant)))
	(destructuring-bind (new-xr new-yr nil)
	    (apply #'xyz-to-xyy
		   (apply ca-func
			  (lrgb-to-xyz 1 0 0 rgbspace)))
	  (destructuring-bind (new-xg new-yg nil)
	      (apply #'xyz-to-xyy
		     (apply ca-func
			    (lrgb-to-xyz 0 1 0 rgbspace)))
	    (destructuring-bind (new-xb new-yb nil)
		(apply #'xyz-to-xyy
		       (apply ca-func
			      (lrgb-to-xyz 0 0 1 rgbspace)))
	      (new-rgbspace new-xr new-yr new-xg new-yg new-xb new-yb
			    :illuminant illuminant
			    :linearizer (rgbspace-linearizer rgbspace)
			    :delinearizer (rgbspace-delinearizer rgbspace))))))
      (new-rgbspace (rgbspace-xr rgbspace) (rgbspace-yr rgbspace)
		    (rgbspace-xg rgbspace) (rgbspace-yg rgbspace)
		    (rgbspace-xb rgbspace) (rgbspace-yb rgbspace)
		    :illuminant (rgbspace-illuminant rgbspace)
		    :linearizer (rgbspace-linearizer rgbspace)
		    :delinearizer (rgbspace-delinearizer rgbspace))))


(defun srgb-linearizer (x)
  (clamp (if (<= x 0.04045d0)
	     (/ x 12.92d0)
	     (expt (/ (+ x 0.055d0) 1.055d0) 2.4d0))
	 0d0 1d0))

(defun srgb-delinearizer (x)
  (clamp (if (<= x 0.0031308d0)
	     (* x 12.92d0)
	     (- (* 1.055d0 (expt x #.(/ 1 2.4d0))) 0.055d0))
	 0d0 1d0))

(defparameter srgb
  (new-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:linearizer #'srgb-linearizer				
		:delinearizer #'srgb-delinearizer))

(defparameter srgbd65 srgb)

(defparameter srgbd50
  (new-rgbspace 0.6484318256422797d0 0.3308548910484657d0
		0.32116034975649527d0 0.5978620931221631d0
		0.15588602412126393d0 0.06604312820634879d0
		:linearizer #'srgb-linearizer
		:delinearizer #'srgb-delinearizer
		:illuminant dufy:illum-d50))

(defun adobe-linearizer (x)
  (clamp (if (<= x 0.0556d0)
	     (* x #.(float 1/32 1d0))
	     (expt x 2.2d0))
	 0d0 1d0))

(defun adobe-delinearizer (x)
  (clamp (if (<= x 0.00174d0)
	     (* x 32d0)
	     (expt x #.(/ 1 2.2d0)))
	 0d0 1d0))

(defparameter adobe
  (new-rgbspace 0.64d0 0.33d0 0.21d0 0.71d0 0.15d0 0.06d0
		:linearizer #'adobe-linearizer
		:delinearizer #'adobe-delinearizer))

(defparameter adobed65 adobe)

(defparameter adobed50
  (new-rgbspace 0.6484318256422797d0 0.3308548910484657d0
		0.23016411634130152d0 0.7015617915499972d0
		0.15588602412126396d0 0.06604312820634878d0
		:linearizer #'adobe-linearizer
		:delinearizer #'adobe-delinearizer
		:illuminant dufy:illum-d50))

(defparameter ntsc1953
  (new-rgbspace 0.67d0 0.33d0 0.21d0 0.71d0 0.14d0 0.08d0
		:illuminant illum-c
		:linearizer (gen-linearizer 2.2d0)
		:delinearizer (gen-delinearizer 2.2d0)))

(defparameter pal/secam
  (new-rgbspace 0.64d0 0.33d0 0.29d0 0.60d0 0.15d0 0.06d0
		:linearizer (gen-linearizer 2.8d0)
		:delinearizer (gen-delinearizer 2.8d0)))


(defparameter prophoto
  (new-rgbspace 0.7347d0 0.2653d0 0.1596d0 0.8404d0 0.0366d0 0.0001d0
		:illuminant illum-d50
		:linearizer #'(lambda (x)
				(clamp (if (<= x #.(* 1/512 16d0))
					   (* x #.(float 1/16 1d0))
					   (expt x 1.8d0))
				       0 1))
		:delinearizer #'(lambda (x)
				  (clamp (if (<= x (float 1/512 1d0))
					     (* x 16d0)
					     (expt x #.(/ 1 1.8d0)))
					 0 1))))			      
 

;; the nominal range of x is [0, 1]
(defun linearize (x &optional (rgbspace srgb))
  (funcall (rgbspace-linearizer rgbspace) x))

(defun delinearize (x &optional (rgbspace srgb))
  (funcall (rgbspace-delinearizer rgbspace) x))

;; convert XYZ to linear RGB in [0, 1]
(defun xyz-to-lrgb (x y z &key (rgbspace srgb) (threshold 0))
  "Returns multiple values: (LR LG LB), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of LR , LG and LB are outside
the interval [-THRESHOLD, 1+THRESHOLD]."
  (destructuring-bind (lr lg lb)
      (multiply-matrix-and-vec (rgbspace-from-xyz-matrix rgbspace)
				x y z)
    (let ((out-of-gamut (not (and  (nearly<= threshold 0 lr 1)
				   (nearly<= threshold 0 lg 1)
				   (nearly<= threshold 0 lb 1)))))
      (values (list lr lg lb) out-of-gamut))))

(defun lrgb-to-xyz (lr lg lb &optional (rgbspace srgb))
  (multiply-matrix-and-vec (rgbspace-to-xyz-matrix rgbspace)
			    lr lg lb))		       

(defun lrgb-to-rgb (lr lg lb &optional (rgbspace srgb))
  (let ((delin (rgbspace-delinearizer rgbspace)))
    (list (funcall delin lr)
	  (funcall delin lg)
	  (funcall delin lb))))

(defun rgb-to-lrgb (r g b &optional (rgbspace srgb))
  (let ((lin (rgbspace-linearizer rgbspace)))
    (list (funcall lin r)
	  (funcall lin g)
	  (funcall lin b))))

(defun xyz-to-rgb (x y z &key (rgbspace srgb) (threshold 0))
  "Returns multiple values: (R G B), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (lrgb out-of-gamut)
      (xyz-to-lrgb x y z :rgbspace rgbspace :threshold threshold)
    (values (mapcar (rgbspace-delinearizer rgbspace) lrgb)
	    out-of-gamut)))

(defun rgb-to-xyz (r g b &optional (rgbspace srgb))
  (apply (rcurry #'lrgb-to-xyz rgbspace)
	 (rgb-to-lrgb r g b rgbspace)))


(defun rgb-to-rgb255 (r g b)
  "Quantizes RGB values from [0, 1] to {0, 1, ..., 255}, though it
accepts all the real values."
  (list (round (* r 255))
	(round (* g 255))
	(round (* b 255))))

(defun rgb255-to-rgb (r255 g255 b255)
  (list (* r255 #.(float 1/255 1d0))
	(* g255 #.(float 1/255 1d0))
	(* b255 #.(float 1/255 1d0))))

(defun xyz-to-rgb255 (x y z &key (rgbspace srgb) (threshold 0))
  "Returns multiple values: (R255 G255 B255), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (mapcar #'(lambda (x) (round (* (clamp x 0 1) 255d0))) rgb)
	    out-of-gamut)))

;; convert RGB ({0, 1, ..., 255}) to XYZ ([0, 1])
(defun rgb255-to-xyz (r255 g255 b255 &optional (rgbspace srgb))
  (rgb-to-xyz (* r255 #.(float 1/255 1d0))
	      (* g255 #.(float 1/255 1d0))
	      (* b255 #.(float 1/255 1d0))
	      rgbspace))

(defun rgb255-to-hex (r255 g255 b255)
  (+ (ash r255 16) (ash g255 8) b255))

(defun hex-to-rgb255 (hex)
  (list (logand (ash hex -16) #xff)
	(logand (ash hex -8) #xff)
	(logand hex #xff)))

(defun hex-to-xyz (hex &optional (rgbspace srgb))
  (apply (rcurry #'rgb255-to-xyz rgbspace)
	 (hex-to-rgb255 hex)))

(defun xyz-to-hex (x y z &key (rgbspace srgb) (threshold 0))
  "Returns multiple values: HEX, OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (rgb255 out-of-gamut)
      (xyz-to-rgb255 x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb255-to-hex rgb255)
	    out-of-gamut)))
	 
  
(defmacro rgb1+ (x)
  `(clamp (1+ ,x) 0 255))

(defmacro rgb1- (x)
  `(clamp (1- ,x) 0 255))



;;; L*a*b*, L*u*v*, LCH, Delta E
(defun function-f (x)
  (declare (optimize (speed 3) (safety 0))
	   (double-float x))
  (if (> x #.(float 216/24389 1d0))
      (expt x #.(float 1/3 1d0))
      (+ (* #.(/ 24389/27 116d0) x) #.(float 16/116 1d0))))

(defun xyz-to-lab (x y z &optional (illuminant illum-d65))
  (let ((fx (function-f (/ (float x 1d0) (illuminant-largex illuminant))))
	(fy (function-f (float y 1d0)))
	(fz (function-f (/ (float z 1d0) (illuminant-largez illuminant)))))
    (list (- (* 116d0 fy) 16d0)
	  (* 500d0 (- fx fy))
	  (* 200d0 (- fy fz)))))

(defun xyy-to-lab (x y largey &optional (illuminant illum-d65))
  (destructuring-bind (x y z) (xyy-to-xyz x y largey)
    (xyz-to-lab x y z illuminant)))

(defun lab-to-xyz (lstar astar bstar &optional (illuminant illum-d65))
  (let* ((fy (* (+ lstar 16d0) 0.008620689655172414d0))
	 (fx (+ fy (* astar 0.002d0)))
	 (fz (- fy (* bstar 0.005d0))))
    (list (if (> fx 0.20689655172413793d0)
	      (* (illuminant-largex illuminant) fx fx fx)
	      (* (- fx 0.13793103448275862d0) 0.12841854934601665d0 (illuminant-largex illuminant)))
	  (if (> fy 0.20689655172413793d0)
	      (* (illuminant-largey illuminant) fy fy fy)
	      (* (- fy 0.13793103448275862d0) 0.12841854934601665d0 (illuminant-largey illuminant)))
	  (if (> fz 0.20689655172413793d0)
	      (* (illuminant-largez illuminant) fz fz fz)
	      (* (- fz 0.13793103448275862d0) 0.12841854934601665d0 (illuminant-largez illuminant))))))

(defun lstar-to-y (lstar)
  (let* ((fy (* (+ lstar 16) 0.008620689655172414d0)))
    (if (> fy 0.20689655172413793d0)
	(* fy fy fy)
	(* (- fy 0.13793103448275862d0) 0.12841854934601665d0))))
 
(defun lab-to-xyy (lstar astar bstar &optional (illuminant illum-d65))
  (destructuring-bind (x y z) (lab-to-xyz lstar astar bstar illuminant)
    (xyz-to-xyy x y z)))

(define-constant CONST-TWO-PI/360 (/ TWO-PI 360))
(define-constant CONST-360/TWO-PI (/ 360 TWO-PI))

(defun lab-to-lchab (lstar astar bstar)
  (list lstar
	(sqrt (+ (* astar astar) (* bstar bstar)))
	(mod (* (atan bstar astar) CONST-360/TWO-PI) 360d0)))

(defun lchab-to-lab (lstar cstarab hab)
  (let ((hue-two-pi (* hab CONST-TWO-PI/360)))
    (list lstar (* cstarab (cos hue-two-pi)) (* cstarab (sin hue-two-pi)))))

(defun xyz-to-lchab (x y z &optional (illuminant illum-d65))
  (apply #'lab-to-lchab (xyz-to-lab x y z illuminant)))

(defun xyy-to-lchab (x y largey &optional (illuminant illum-d65))
  (apply #'lab-to-lchab (xyy-to-lab x y largey illuminant)))

(defun lchab-to-xyz (lstar cstarab hab &optional (illuminant illum-d65))
  (destructuring-bind (l a b) (lchab-to-lab lstar cstarab hab)
    (lab-to-xyz l a b illuminant)))

(defun lchab-to-xyy (lstar cstarab hab &optional (illuminant illum-d65))
  (destructuring-bind (x y z) (lchab-to-xyz lstar cstarab hab illuminant)
    (xyz-to-xyy x y z)))

(defun rgb255-to-lab (r255 g255 b255 &optional (rgbspace srgb))
  (apply (rcurry #'xyz-to-lab (rgbspace-illuminant rgbspace))
	 (rgb255-to-xyz r255 g255 b255 rgbspace)))


(defun calc-uvprime (x y)
  (let ((denom (+ (* -2d0 x) (* 12d0 y) 3d0)))
    (list (/ (* 4d0 x) denom)
	  (/ (* 9d0 y) denom))))

(defun calc-uvprime-from-xyz (x y z)
  (let ((denom (+ x (* 15d0 y) (* 3d0 z))))
    (list (/ (* 4d0 x) denom)
	  (/ (* 9d0 y) denom))))

(defun xyz-to-luv (x y z &optional (illuminant illum-d65))
  (destructuring-bind (uprime vprime) (calc-uvprime-from-xyz x y z)
    (destructuring-bind (urprime vrprime) (calc-uvprime (illuminant-x illuminant) (illuminant-y illuminant))
      (let* ((yr (/ y (illuminant-largey illuminant)))
	     (lstar (if (> yr 0.008856451679035631d0)
			(- (* 116d0 (expt yr 0.3333333333333333d0)) 16d0)
			(* 903.2962962962963d0 yr))))
	(list lstar
	      (* 13d0 lstar (- uprime urprime))
	      (* 13d0 lstar (- vprime vrprime)))))))

(defun luv-to-xyz (lstar ustar vstar &optional (illuminant illum-d65))
  (destructuring-bind (urprime vrprime) (calc-uvprime (illuminant-x illuminant) (illuminant-y illuminant))
    (let* ((uprime (+ (/ ustar (* 13d0 lstar)) urprime))
	   (vprime (+ (/ vstar (* 13d0 lstar)) vrprime))
	   (l (/ (+ lstar 16d0) 116d0))
	   (y (if (<= lstar 8d0)
		  (* (illuminant-largey illuminant)
		     lstar
		     0.008856451679035631d0)
		  (* (illuminant-largey illuminant)
		     (* l l l)))))
      (list (* y (/ (* 9d0 uprime) (* 4d0 vprime)))
	    y
	    (* y (/ (- 12d0 (* 3d0 uprime) (* 20d0 vprime)) (* 4d0 vprime)))))))
	    
(defun luv-to-lchuv (lstar ustar vstar)
  (list lstar
	(sqrt (+ (* ustar ustar) (* vstar vstar)))
	(mod (* (atan vstar ustar) CONST-360/TWO-PI) 360d0)))

(defun lchuv-to-luv (lstar cstaruv huv)
  (let ((hue-two-pi (* huv CONST-TWO-PI/360)))
    (list lstar (* cstaruv (cos hue-two-pi)) (* cstaruv (sin hue-two-pi)))))

(defun xyz-to-lchuv (x y z &optional (illuminant illum-d65))
  (apply #'luv-to-lchuv (xyz-to-luv x y z illuminant)))

(defun lchuv-to-xyz (lstar cstaruv huv &optional (illuminant illum-d65))
  (destructuring-bind (l u v) (lchuv-to-luv lstar cstaruv huv)
    (luv-to-xyz l u v illuminant)))



;; obsolete
(defun polar-mean-of-xy (x1 y1 x2 y2)
  (destructuring-bind (r1 theta1) (xy-to-polar x1 y1)
    (destructuring-bind (r2 theta2) (xy-to-polar x2 y2)
      (polar-to-xy (* 0.5d0 (+ r1 r2))
		   (circular-lerp theta1 theta2 0.5d0)))))

(defun xy-to-polar (x y)
  (let ((dx (- x 0.31006d0))
	(dy (- y 0.31616d0)))
    (list (sqrt (+ (* dx dx) (* dy dy)))
	  (mod (atan dy dx) TWO-PI))))

(defun polar-to-xy (r theta)
  (let ((dx (* r (cos theta)))
	(dy (* r (sin theta))))
    (list (+ dx 0.31006d0) (+ dy 0.31616d0))))


;;; HSV/HSL

;; H is in R/360. S and V are in [0, 1].
(defun hsv-to-rgb (hue sat val)
  (let* ((c (coerce (* val sat) 'double-float))
	 (h-prime (/ (mod hue 360d0) 60d0))
	 (h-prime-int (floor h-prime))
	 (x (* c (- 1d0 (abs (- (mod h-prime 2d0) 1d0)))))
	 (base (- val c)))
    (cond ((= sat 0d0) (list base base base))
	  ((= 0 h-prime-int) (list (+ base c) (+ base x) base))
	  ((= 1 h-prime-int) (list (+ base x) (+ base c) base))
	  ((= 2 h-prime-int) (list base (+ base c) (+ base x)))
	  ((= 3 h-prime-int) (list base (+ base x) (+ base c)))
	  ((= 4 h-prime-int) (list (+ base x) base (+ base c)))
	  ((= 5 h-prime-int) (list (+ base c) base (+ base x))))))
	 
(defun hsv-to-rgb255 (hue sat val)
  (mapcar #'(lambda (x) (round (* x 255d0)))
	  (hsv-to-rgb hue sat val)))

;; The given HSV color is regarded as converted from a non-linear (i.e. gamma-corrected) RGB color.
(defun hsv-to-xyz (hue sat val &optional (rgbspace srgb))
  (destructuring-bind (r g b) (hsv-to-rgb hue sat val)
    (rgb-to-xyz r g b rgbspace)))

;; R, G and B should be in [0, 1].
(defun rgb-to-hsv (r g b)
  (let* ((maxrgb (coerce (max r g b) 'double-float))
	 (minrgb (coerce (min r g b) 'double-float))
	 (s (if (= maxrgb 0) 0d0 (/ (- maxrgb minrgb) maxrgb)))
	 (h (cond ((= minrgb maxrgb) 0d0)
		  ((= minrgb b) (+ (* 60d0 (/ (- g r) (- maxrgb minrgb))) 60d0))
		  ((= minrgb r) (+ (* 60d0 (/ (- b g) (- maxrgb minrgb))) 180d0))
		  ((= minrgb g) (+ (* 60d0 (/ (- r b) (- maxrgb minrgb))) 300d0)))))
    (list h s maxrgb)))
	 
(defun rgb255-to-hsv (r255 g255 b255)
  (rgb-to-hsv (* r255 #.(/ 1 255d0))
	      (* g255 #.(/ 1 255d0))
	      (* b255 #.(/ 1 255d0))))

(defun xyz-to-hsv (x y z &key (rgbspace srgb) (threshold 0))
  "Returns multiple values: (H S V), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of **linear** RGB values are
outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-hsv
		   (mapcar #'(lambda (i) (clamp i 0d0 1d0)) rgb))
	    out-of-gamut)))
  

(defun hsl-to-rgb (hue sat lum)
  (let* ((tmp (* 0.5d0 sat (- 1d0 (abs (- (* lum 2d0) 1d0)))))
	 (max (+ lum tmp))
	 (min (- lum tmp))
	 (delta (- max min))
	 (h-prime (/ (mod hue 360d0) 60d0))
	 (h-prime-int (floor h-prime)))
    (cond ((= sat 0d0) (list max max max))
	  ((= 0 h-prime-int) (list max
				   (+ min (* delta hue 0.016666666666666667d0))
				   min))
	  ((= 1 h-prime-int) (list (+ min (* delta (- 120d0 hue) 0.016666666666666667d0))
				   max
				   min))
	  ((= 2 h-prime-int) (list min
				   max
				   (+ min (* delta (- hue 120d0) 0.016666666666666667d0))))
	  ((= 3 h-prime-int) (list min
				   (+ min (* delta (- 240d0 hue) 0.016666666666666667d0))
				   max))
	  ((= 4 h-prime-int) (list (+ min (* delta (- hue 240d0) 0.016666666666666667d0))
				   min
				   max))
	  ((= 5 h-prime-int) (list max
				   min
				   (+ min (* delta (- 360d0 hue) 0.016666666666666667d0)))))))
				   

(defun hsl-to-rgb255 (hue sat lum)
  (mapcar #'(lambda (x) (round (* x 255d0)))
	  (hsl-to-rgb hue sat lum)))

;; The given HSV color is regarded as converted from a non-linear (i.e. gamma-corrected) RGB color.
(defun hsl-to-xyz (hue sat lum &optional (rgbspace srgb))
  (destructuring-bind (r g b) (hsl-to-rgb hue sat lum)
    (rgb-to-xyz r g b rgbspace)))

(defun rgb-to-hsl (r g b)
  (let ((min (min r g b))
	(max (max r g b)))
    (let ((hue (cond ((= min max) 0d0)
		     ((= min b) (+ 60d0 (* 60d0 (/ (- g r) (- max min)))))
		     ((= min r) (+ 180d0 (* 60d0 (/ (- b g) (- max min)))))
		     ((= min g) (+ 300d0 (* 60d0 (/ (- r b) (- max min))))))))
      (list hue
	    (/ (- max min) (- 1d0 (abs (+ max min -1d0))))
	    (* 0.5d0 (+ max min))))))
	  

(defun rgb255-to-hsl (r255 g255 b255)
  (rgb-to-hsl (* r255 #.(/ 1 255d0))
	      (* g255 #.(/ 1 255d0))
	      (* b255 #.(/ 1 255d0))))

(defun xyz-to-hsl (x y z &key (rgbspace srgb) (threshold 0))
    "Returns multiple values: (H S L), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-hsl
		   (mapcar #'(lambda (i) (clamp i 0d0 1d0)) rgb))
	    out-of-gamut)))
  

;; CIE 1931/1964 Color Matching Functions


(defun color-matching-x (wavelength &optional (observer :1931))
  (if (or (< wavelength 360) (< 830 wavelength))
      0
      (multiple-value-bind (quot rem) (floor wavelength)
	(if (eq observer :1931)
	    (lerp rem
		  (aref color-matching-arr-1931 (- quot 360) 0)
		  (aref color-matching-arr-1931 (1+ (- quot 360)) 0))
	    (lerp rem
		  (aref color-matching-arr-1964 (- quot 360) 0)
		  (aref color-matching-arr-1964 (1+ (- quot 360)) 0))))))

(defun color-matching-y (wavelength &optional (observer :1931))
  (if (or (< wavelength 360) (< 830 wavelength))
      0
      (multiple-value-bind (quot rem) (floor wavelength)
	(if (eq observer :1931)
	    (lerp rem
		  (aref color-matching-arr-1931 (- quot 360) 1)
		  (aref color-matching-arr-1931 (1+ (- quot 360)) 1))
	    (lerp rem
		  (aref color-matching-arr-1964 (- quot 360) 1)
		  (aref color-matching-arr-1964 (1+ (- quot 360)) 1))))))
	
(defun color-matching-z (wavelength &optional (observer :1931))
  (if (or (< wavelength 360) (< 830 wavelength))
      0
      (multiple-value-bind (quot rem) (floor wavelength)
	(if (eq observer :1931)
	    (lerp rem
		  (aref color-matching-arr-1931 (- quot 360) 2)
		  (aref color-matching-arr-1931 (1+ (- quot 360)) 2))
	    (lerp rem
		  (aref color-matching-arr-1964 (- quot 360) 2)
		  (aref color-matching-arr-1964 (1+ (- quot 360)) 2))))))

(defun color-matching (wavelength &optional (observer :1931))
  (if (or (< wavelength 360) (< 830 wavelength))
      0
      (multiple-value-bind (quot rem) (floor wavelength)
	(if (eq observer :1931)
	    (list (lerp rem
			(aref color-matching-arr-1931 (- quot 360) 0)
			(aref color-matching-arr-1931 (1+ (- quot 360)) 0))
		  (lerp rem
			(aref color-matching-arr-1931 (- quot 360) 1)
			(aref color-matching-arr-1931 (1+ (- quot 360)) 1))
		  (lerp rem
			(aref color-matching-arr-1931 (- quot 360) 2)
			(aref color-matching-arr-1931 (1+ (- quot 360)) 2)))
	    (list (lerp rem
			(aref color-matching-arr-1964 (- quot 360) 0)
			(aref color-matching-arr-1964 (1+ (- quot 360)) 0))
		  (lerp rem
			(aref color-matching-arr-1964 (- quot 360) 1)
			(aref color-matching-arr-1964 (1+ (- quot 360)) 1))
		  (lerp rem
			(aref color-matching-arr-1964 (- quot 360) 2)
			(aref color-matching-arr-1964 (1+ (- quot 360)) 2)))))))


;; (defun color-matching-int (wavelength)
;;   (if (or (< wavelength 360) (< 830 wavelength))
;;       0
;;       (list (aref color-matching-arr (- wavelength 360) 0)
;; 	    (aref color-matching-arr (- wavelength 360) 1)
;; 	    (aref color-matching-arr (- wavelength 360) 2))))


(defun spectrum-sum (spectrum-func &optional (band 1))
  (loop for wl from 360 to 780 by band
     sum (funcall spectrum-func wl)))

;; Illuminant E
;; SPECTRUM-FUNC must be normalized.
(defun spectrum-to-xyz (spectrum-func &key (band 1) (observer :1931))
  (let ((const 0.009358326136267765d0)
	(x 0)
	(y 0)
	(z 0))
    (do ((wl 360 (+ wl band)))
	((< 780 wl) (list (* band x const)
			  (* band y const)
			  (* band z const)))
      (let ((p (funcall spectrum-func wl))
	    (idx (- wl 360)))
	(if (eq observer :1931)
	    (progn
	      (incf x (* (aref color-matching-arr-1931 idx 0) p))
	      (incf y (* (aref color-matching-arr-1931 idx 1) p))
	      (incf z (* (aref color-matching-arr-1931 idx 2) p)))
	    (progn
	      (incf x (* (aref color-matching-arr-1964 idx 0) p))
	      (incf y (* (aref color-matching-arr-1964 idx 1) p))
	      (incf z (* (aref color-matching-arr-1964 idx 2) p))))))))


;; the spectrum of a black body
(defun bb-spectrum (wavelength &optional (temperature 5000))
  (let ((wlm (* wavelength 1d-9)))
    (/ (* 3.74183d-16 (expt wlm -5d0))
       (- (exp (/ 1.4388d-2 (* wlm temperature))) 1d0))))

(defun optimal-spectrum (wavelength &optional (wl-begin 360) (wl-end 780))
  (if (<= wl-begin wl-end)
      (if (<= wl-begin wavelength wl-end) 1 0)
      (if (or (<= wavelength wl-end) (<= wl-begin wavelength)) 1 0)))

(defun scale-xyz (x y z &optional (scale-to 1))
  "X, Y, Z are scaled so as to satisfy X+Y+Z = SCALE-TO."
  (let ((factor (* scale-to (/ (+ x y z)))))
    (list (* x factor)
	  (* y factor)
	  (* z factor))))

(defun scale-lrgb-until-saturated (lr lg lb)
  "LR, LG, LB are scaled so that one of them is saturated."
  (let ((max (max lr lg lb)))
    (if (<= max 0)
	(list lr lg lb)
	(let ((factor (/ max)))
	  (list (* lr factor)
		(* lg factor)
		(* lb factor))))))

(let ((e-to-d65 (gen-ca-converter illum-e illum-d65)))
  (defun temperature-test (temp)
    (apply #'rgb-to-rgb255
	   (apply #'lrgb-to-rgb
		  (apply #'scale-lrgb-until-saturated
			 (mapcar (rcurry #'clamp 0 1)
				 (apply #'xyz-to-lrgb
					(apply e-to-d65
					       (spectrum-to-xyz (compose (rcurry #'* (/ (spectrum-sum (rcurry #'bb-spectrum temp))))
									 (rcurry #'bb-spectrum temp)))))))))))
  
