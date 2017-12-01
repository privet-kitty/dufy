(in-package :cl-user)

(defpackage :clcl
  (:use :common-lisp)
  (:export :xyy-to-xyz
	   :xyz-to-xyy
	   :illuminant
	   :new-illuminant
	   :defilluminant
	   :c :d50 :d65
	   :calculate-ca-matrix
	   :chromatic-adaptation
	   :bradford
	   :xyz-scaling
	   :von-kries
	   :cmccat97
	   :cmccat2000
	   :rgbspace
	   :srgb
	   :srgbd65
	   :adobe
	   :new-rgbspace
	   :rgbspace-linearizer
	   :rgbspace-delinearizer
	   :genlinearizer
	   :gendelinearizer
	   :xyz-to-lab
	   :srgb-to-lab
	   :deltae
	   :xyz-deltae
	   :srgb-deltae
	   :xyzc-to-xyzd65
	   :xyzd65-to-xyzc
	   :bound
	   :delinearize
	   :xyz-to-lrgb
	   :xyy-to-lrgb
	   :nearly=
	   :nearly<=
	   :xyz-to-srgb
	   :xyy-to-srgb
	   :srgb-to-hex
	   :hex-to-srgb
	   :linearize
	   :srgb-to-xyz
	   :two-pi
	   :subtract-with-mod
	   :interpolate-in-circle-group
	   :polar-mean-of-xy
	   :xy-to-polar
	   :polar-to-xy
	   :rgb1+
	   :rgb1-

	   :mrd-filename
	   :mrd-pathname
	   :value-to-y
	   :y-to-value
	   :hvc-to-xyy
	   :hvc-to-srgb
	   :munsellspec-to-xyy
	   :munsellspec-to-hvc
	   :munsellspec-to-srgb
	   :max-chroma
))

(in-package :clcl)

(defconstant TWO-PI (+ PI PI))

(defun bound (x min max)
  (cond ((< x min) min)
	((> x max) max)
	(t x)))

(defun nearly= (threshold number &rest more-numbers)
  (if (null more-numbers)
      t
      (and (<= (abs (- number (car more-numbers))) threshold)
	   (apply #'nearly= threshold more-numbers))))

(defun nearly<= (threshold number &rest more-numbers)
  (if (null more-numbers)
      t
      (and (<= (- number (car more-numbers)) threshold)
	   (apply #'nearly<= threshold more-numbers))))

;;; Standard Illuminant, XYZ, xyY
;;; The nominal range of X, Y, Z, x, y is always [0, 1]

(defstruct illuminant
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (largex 0.0 :type double-float)
  (largey 0.0 :type double-float)
  (largez 0.0 :type double-float)
  (index 0 :type fixnum))

(defun xyy-to-xyz (x y largey)
  (if (zerop y)
      (list 0d0 0d0 0d0)
      (list (/ (* x largey) y) 
	    largey
	    (/ (* (- 1 x y) largey) y))))

;; number of registered standard illuminats
(defparameter *number-of-illuminants* 0)

(defun get-new-illuminant-index ()
  (prog1 *number-of-illuminants*
    (incf *number-of-illuminants*)))

(defparameter illuminant-array
  (make-array 10 :fill-pointer 0 :adjustable t))

(defun get-illuminant-by-index (index)
  (aref illuminant-array index))

;; a function to define user own standard illuminant
(defmacro new-illuminant (x y)
  (let ((largex (gensym))
	(largey (gensym))
	(largez (gensym)))
    `(destructuring-bind (,largex ,largey ,largez) (xyy-to-xyz ,x ,y 1.0d0)
       (make-illuminant :x ,x :y ,y
			:largex ,largex
			:largey ,largey
			:largez ,largez
			:index -1))))

;; a function to define default standard illuminant
(defmacro defilluminant (name x y)
  (let ((largex (gensym))
	(largey (gensym))
	(largez (gensym)))
    `(progn
       (defparameter ,name
	 (destructuring-bind (,largex ,largey ,largez) (xyy-to-xyz ,x ,y 1.0d0)
	   (make-illuminant :x ,x :y ,y
			  :largex ,largex
			  :largey ,largey
			  :largez ,largez
			  :index (get-new-illuminant-index))))
       (vector-push-extend ,name illuminant-array))))

(defilluminant a 0.44757d0 0.40745d0)
(defilluminant c 0.31006d0 0.31616d0)
(defilluminant d50 0.34567d0 0.35850d0)
(defilluminant d65 0.31271d0 0.32902d0)
(defilluminant e 0.33333d0 0.33333d0)

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
				  (* (aref mat 1 2) (aref mat 2 1))) det))
    (setf (aref invmat 0 1) (/ (- (* (aref mat 0 2) (aref mat 2 1))
				  (* (aref mat 0 1) (aref mat 2 2))) det))
    (setf (aref invmat 0 2) (/ (- (* (aref mat 0 1) (aref mat 1 2))
				  (* (aref mat 0 2) (aref mat 1 1))) det))
    (setf (aref invmat 1 0) (/ (- (* (aref mat 1 2) (aref mat 2 0))
				  (* (aref mat 1 0) (aref mat 2 2))) det))
    (setf (aref invmat 1 1) (/ (- (* (aref mat 0 0) (aref mat 2 2))
				  (* (aref mat 0 2) (aref mat 2 0))) det))
    (setf (aref invmat 1 2) (/ (- (* (aref mat 0 2) (aref mat 1 0))
				  (* (aref mat 0 0) (aref mat 1 2))) det))
    (setf (aref invmat 2 0) (/ (- (* (aref mat 1 0) (aref mat 2 1))
				  (* (aref mat 1 1) (aref mat 2 0))) det))
    (setf (aref invmat 2 1) (/ (- (* (aref mat 0 1) (aref mat 2 0))
				  (* (aref mat 0 0) (aref mat 2 1))) det))
    (setf (aref invmat 2 2) (/ (- (* (aref mat 0 0) (aref mat 1 1))
				  (* (aref mat 0 1) (aref mat 1 0))) det))
    invmat))

(defun multiply-matrix-and-list (matrix lst)
  (destructuring-bind (x y z) lst
    (list (+ (* x (aref matrix 0 0))
	     (* y (aref matrix 0 1))
	     (* z (aref matrix 0 2)))
	  (+ (* x (aref matrix 1 0))
	     (* y (aref matrix 1 1))
	     (* z (aref matrix 1 2)))
	  (+ (* x (aref matrix 2 0))
	     (* y (aref matrix 2 1))
	     (* z (aref matrix 2 2))))))

(defun calculate-ca-matrix  (from-illuminant to-illuminant &optional (tmatrix bradford))
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
    
(defparameter bradford-dictionary
  (make-array (list *number-of-illuminants* *number-of-illuminants*)
	      :element-type '(simple-array double-float (3 3))))
	      
(defun reconstruct-bradford-dictionary ()
  (dotimes (from-index *number-of-illuminants*)
    (dotimes (to-index *number-of-illuminants*)
      (setf (aref bradford-dictionary from-index to-index)
	    (calculate-ca-matrix
	     (aref illuminant-array from-index)
	     (aref illuminant-array to-index))))))

(reconstruct-bradford-dictionary)

;; get a transformation matrix by the above constructed dictionary
(defun get-bradford-transformation-matrix (from-illuminant to-illuminant)
  (aref bradford-dictionary
	(illuminant-index from-illuminant)
	(illuminant-index to-illuminant)))

;; general function for chromatic adaptation
(defun chromatic-adaptation (xyz-list matrix)
  (multiply-matrix-and-list matrix xyz-list))


;; special function for Bradford transformation between default standard illuminants
(defun bradford (xyz-list from-illuminant to-illuminant)
  (if (or (< (illuminant-index from-illuminant) 0)
	  (< (illuminant-index to-illuminant) 0))
      (error "The function BRADFORD cannot take a user-defined standard illuminant.
Use CALCULATE-CA-MATRIX and CHROMATIC-ADAPTATION.")
      (chromatic-adaptation
       xyz-list
       (get-bradford-transformation-matrix from-illuminant to-illuminant))))

(defun xyz-to-xyy (x y z &optional (illuminant d65))
  (if (= x y z 0)
      (list (illuminant-x illuminant) (illuminant-y illuminant) y)
      (list (/ x (+ x y z)) (/ y (+ x y z)) y)))



;;; RGB Color Space
(defparameter identity-matrix
  (make-array '(3 3) :element-type 'double-float
	      :initial-contents '((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 1d0))))

(defstruct rgbspace
  (xr 0d0 :type double-float) (yr 0d0 :type double-float)
  (xg 0d0 :type double-float) (yg 0d0 :type double-float)
  (xb 0d0 :type double-float) (yb 0d0 :type double-float)
  (illuminant d65)
  (linearizer #'identity)
  (delinearizer #'identity)
  (to-xyz-matrix identity-matrix :type (simple-array double-float (3 3)))
  (from-xyz-matrix identity-matrix :type (simple-array double-float (3 3))))

(defun new-rgbspace (xr yr xg yg xb yb &key (illuminant d65) (linearizer #'identity) (delinearizer #'identity))
  (let ((coordinates
	 (make-array '(3 3)
		     :element-type 'double-float
		     :initial-contents (list (list xr xg xb)
					     (list yr yg yb)
					     (list (- 1d0 xr yr) (- 1d0 xg yg) (- 1d0 xb yb))))))
    (destructuring-bind (sr sg sb)
	(multiply-matrix-and-list (invert-matrix33 coordinates)
				  (list (illuminant-largex illuminant)
					(illuminant-largey illuminant)
					(illuminant-largez illuminant)))
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
		       
(defparameter srgb
  (clcl::new-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		      :linearizer #'(lambda (x)
					 (if (<= x 0.04045d0)
					     (/ x 12.92d0)
					     (expt (/ (+ x 0.055d0) 1.055d0) 2.4d0)))
		      :delinearizer #'(lambda (x)
					(if (<= x 0.0031308d0)
					    (* x 12.92d0)
					    (- (* 1.055d0 (expt x 0.41666666666d0)) 0.055d0)))))
(defparameter srgbd65 srgb)
(defparameter adobe
  (clcl::new-rgbspace 0.64d0 0.33d0  0.21d0 0.71d0 0.15d0 0.06d0
		      :linearizer #'(lambda (x)
				      (if (<= x 0.0556d0)
					  (* x 0.03125d0)
					  (expt x 2.2d0)))
					  
		      :delinearizer #'(lambda (x)
					(if (<= x 0.00174d0)
					    (* x 32d0)
					    (expt x 0.45454545454d0)))))



;; (defun delinearize (x &optional (rgbspace srgb))
;;   (if (<= x 0.0031308d0)
;;       (* x 12.92d0)
;;       (- (* 1.055d0 (expt x 0.41666666666d0)) 0.055d0)))
;; (defun linearize (x)
;;   (if (<= x 0.04045d0)
;;       (/ x 12.92d0)
;;       (expt (/ (+ x 0.055d0) 1.055d0) 2.4d0)))

;; the nominal range of x is [0, 1]
(defun linearize (x &optional (rgbspace srgbd65))
  (funcall (rgbspace-linearizer rgbspace) x))

(defun delinearize (x &optional (rgbspace srgbd65))
  (funcall (rgbspace-delinearizer rgbspace) x))

(defun genlinearizer (gamma)
  #'(lambda (x) (expt x (coerce gamma 'double-float))))

(defun gendelinearizer (gamma)
  #'(lambda (x) (expt x (/ 1d0 (coerce gamma 'double-float)))))


;; (defun xyz-to-lrgb (x y z &optional (rgbspace srgb))
;;   (list (+ (* 3.2404542d0 x) (* -1.5371385d0 y) (* -0.4985314d0 z))
;; 	(+ (* -0.9692660d0 x) (* 1.8760108d0 y) (* 0.0415560d0 z))
;; 	(+ (* 0.0556434d0 x) (* -0.2040259d0 y) (* 1.0572252d0 z))))

(defun xyz-to-lrgb (x y z &optional (rgbspace srgb))
  (multiply-matrix-and-list (rgbspace-from-xyz-matrix rgbspace)
			    (list x y z)))

(defun lrgb-to-xyz (lr lg lb &optional (rgbspace srgb))
  (multiply-matrix-and-list (rgbspace-to-xyz-matrix rgbspace)
			    (list lr lg lb)))

;; (defun xyy-to-lrgb (x y largey)
;;   (apply #'xyz-to-lrgb
;; 	 (apply #'xyzc-to-xyzd65
;; 		(xyy-to-xyz x y largey))))
		       

;; convert XYZ d65 to sRGB d65 in [0, 255]
;; return multiple values: (r g b) and out-of-gamut-p
(defun xyz-to-srgb (x y z &key (rgbspace srgbd65) (threshold 0))
  (destructuring-bind (r g b) (xyz-to-lrgb x y z rgbspace)
    (let ((out-of-gamut (not (and (nearly<= threshold 0 r 1)
				  (nearly<= threshold 0 g 1)
				  (nearly<= threshold 0 b 1)))))
      (setf r (round (* (delinearize (bound r 0 1)) 255)))
      (setf g (round (* (delinearize (bound g 0 1)) 255)))
      (setf b (round (* (delinearize (bound b 0 1)) 255)))
      (values (list r g b) out-of-gamut))))

;; (defun xyy-to-srgb (x y largey)
;;   (apply #'xyz-to-srgb (apply #'xyzc-to-xyzd65 (xyy-to-xyz x y largey))))

(defun srgb-to-hex (r g b)
  (+ (ash r 16) (ash g 8) b))

(defun hex-to-srgb (hex)
  (list (logand (ash hex -16) #xff)
	(logand (ash hex -8) #xff)
	(logand hex #xff)))

;; convert sRGB d65 ([0, 255]) to XYZ d65 ([0, 1])
(defun srgb-to-xyz (r g b)
  (let ((lr (linearize (/ r 255.0d0)))
	(lg (linearize (/ g 255.0d0)))
	(lb (linearize (/ b 255.0d0))))
    (list (+ (* 0.4124564d0 lr) (* 0.3575761d0 lg) (* 0.1804375d0 lb))
	  (+ (* 0.2126729d0 lr) (* 0.7151522d0 lg) (* 0.0721750d0 lb))
	  (+ (* 0.0193339d0 lr) (* 0.1191920d0 lg) (* 0.9503041d0 lb)))))

(defmacro rgb1+ (x)
  `(bound (1+ ,x) 0 255))

(defmacro rgb1- (x)
  `(bound (1- ,x) 0 255))



;;; L*a*b*, L*u*v*, LCH, Delta E
(defun function-f (x)
  (if (> x 0.00885645167d0)
      (expt x 0.3333333333d0)
      (+ (* 7.78703703704d0 x) 0.13793103448d0)))

(defun xyz-to-lab (x y z &optional (illuminant d65))
  (let ((fx (function-f (/ x (illuminant-largex illuminant))))
	(fy (function-f y))
	(fz (function-f (/ z (illuminant-largez illuminant)))))
    (list (- (* 116 fy) 16)
	  (* 500 (- fx fy))
	  (* 200 (- fy fz)))))

(defun xyy-to-lab (x y largey &optional (illuminant d65))
  (destructuring-bind (x y z) (xyy-to-xyz x y largey)
    (xyz-to-lab x y z illuminant)))

(defun lab-to-lchab (l a b)
  (list l
	(sqrt (+ (* a a) (* b b)))
	(mod (atan b a) TWO-PI)))

(defun lchab-to-lab (l c h)
  (list l (* c (cos h)) (* c (sin h))))

(defun xyz-to-lchab (x y z &optional (illuminant d65))
  (apply #'lab-to-lchab (xyz-to-lab x y z illuminant)))

(defun xyy-to-lchab (x y largey &optional (illuminant d65))
  (apply #'lab-to-lchab (xyy-to-lab x y largey illuminant)))


(defun srgb-to-lab (r g b)
  (destructuring-bind (x y z) (srgb-to-xyz r g b)
    (xyz-to-lab x y z)))

(defun deltae (l1 a1 b1 l2 a2 b2)
  (let ((deltal (- l1 l2))
	(deltaa (- a1 a2))
	(deltab (- b1 b2)))
    (sqrt (+ (* deltal deltal)
	     (* deltaa deltaa)
	     (* deltab deltab)))))

(defun xyz-deltae (x1 y1 z1 x2 y2 z2 &optional (illuminant d65))
  (destructuring-bind (l1 a1 b1) (xyz-to-lab x1 y1 z1 illuminant)
    (destructuring-bind (l2 a2 b2) (xyz-to-lab x2 y2 z2 illuminant)
      (deltae l1 a1 b1 l2 a2 b2))))

(defun srgb-deltae (r1 g1 b1 r2 g2 b2)
  (destructuring-bind (x1 y1 z1) (srgb-to-xyz r1 g1 b1)
    (destructuring-bind (x2 y2 z2) (srgb-to-xyz r2 g2 b2)
      (xyz-deltae x1 y1 z1 x2 y2 z2))))

;; obsolete
;; (defun xyzc-to-xyzd65 (x y z)
;;   (list (+ (* 0.9904476095076271d0 x) (* -0.007168269199019821d0 y) (* -0.011615568838811846d0 z))
;; 	(+ (* -0.012371160443260617d0 x) (* 1.0155949953067143d0 y) (* -0.002928228748209215d0 z))
;; 	(+ (* -0.0035635466770583962d0 x) (* 0.006769691557536081d0 y) (* 0.9181568621105303d0 z))))

;; obsolete
;; (defun xyzd65-to-xyzc (x y z)
;;   (list (+ (* 1.009778518523861d0 x) (* 0.007041913032199525d0 y) (* 0.012797129456767808d0 z))
;; 	(+ (* 0.012311347023773754d0 x) (* 0.9847093981360044d0 y) (* 0.0032962316048580284d0 z))
;; 	(+ (* 0.003828375092859271d0 x) (* -0.0072330611330787d0 y) (* 1.0891638781614845d0 z))))

(defun subtract-with-mod (x y &optional (divisor TWO-PI))
  (mod (- x y) divisor))

;; counterclockwise linear interpolation from theta1 to theta2 in a circle group
(defun interpolate-in-circle-group (theta1 theta2 ratio &optional (perimeter TWO-PI))
  (let ((dtheta (subtract-with-mod theta2 theta1 perimeter)))
    (mod (+ theta1 (* dtheta ratio)) perimeter)))

(defun polar-mean-of-xy (x1 y1 x2 y2)
  (destructuring-bind (r1 theta1) (xy-to-polar x1 y1)
    (destructuring-bind (r2 theta2) (xy-to-polar x2 y2)
      (polar-to-xy (* 0.5d0 (+ r1 r2))
		   (interpolate-in-circle-group theta1 theta2 0.5d0)))))

(defun xy-to-polar (x y)
  (let ((dx (- x 0.31006d0))
	(dy (- y 0.31616d0)))
    (list (sqrt (+ (* dx dx) (* dy dy)))
	  (mod (atan dy dx) TWO-PI))))

(defun polar-to-xy (r theta)
  (let ((dx (* r (cos theta)))
	(dy (* r (sin theta))))
    (list (+ dx 0.31006d0) (+ dy 0.31616d0))))
