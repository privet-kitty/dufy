(in-package :cl-user)

(defpackage :clcl
  (:use :common-lisp)
  (:shadowing-import-from :alexandria :rcurry :clamp)
  (:export :xyy-to-xyz
	   :xyz-to-xyy
	   :illuminant
	   :illuminant-x
	   :illuminant-y
	   :illuminant-largex
	   :illuminant-largey
	   :illuminant-largez
	   :new-illuminant
	   :defilluminant
	   :c :d50 :d65
	   :calc-ca-matrix
	   :get-ca-converter
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
	   :rgbspace-illuminant
	   :rgbspace-xr
	   :rgbspace-yr
	   :rgbspace-xg
	   :rgbspace-yg
	   :rgbspace-xb
	   :rgbspace-yb
	   :rgbspace-to-xyz-matrix
	   :rgbspace-from-xyz-matrix
	   :genlinearizer
	   :gendelinearizer

	   :xyz-to-lab
	   :lab-to-xyz
	   :rgb255-to-lab
	   :lab-to-lchab
	   :lchab-to-lab
	   :xyy-to-lab
	   :lab-to-xyy
	   :xyz-to-lchab
	   :xyy-to-lchab
	   :lchab-to-xyz
	   :lchab-to-xyy
	   :deltae
	   :xyz-deltae
	   :rgb255-deltae

	   :delinearize
	   :linearize
	   :nearly=
	   :nearly<=
	   :xyz-to-lrgb
	   :lrgb-to-xyz
	   :xyz-to-rgb
	   :rgb-to-xyz
	   :xyz-to-rgb255
	   :rgb255-to-xyz
	   :rgb255-to-hex
	   :hex-to-rgb255
	   :two-pi
	   :subtract-with-mod
	   :interpolate-in-circle-group
	   :polar-mean-of-xy
	   :xy-to-polar
	   :polar-to-xy
	   :rgb1+
	   :rgb1-

	   :hsv-to-rgb
	   :rgb-to-hsv
	   :hsv-to-rgb255
	   :rgb255-to-hsv
	   :hsv-to-xyz
	   :xyz-to-hsv

	   :mrd-filename
	   :mrd-pathname
	   :munsell-value-to-y
	   :y-to-munsell-value
	   :rgb255-to-munsell-value
	   :munsell-hvc-to-xyy
	   :munsell-hvc-to-xyz
	   :munsell-hvc-to-lrgb
	   :munsell-hvc-to-rgb255
	   :munsellspec-to-xyy
	   :munsellspec-to-hvc
	   :munsellspec-to-rgb255
	   :max-chroma
	   :interpolatedp
	   :make-munsell-inversion-data
	   :interpolate-munsell-inversion-data
	   :load-munsell-inversion-data
	   :save-munsell-inversion-data
	   :rgb255-to-munsell-hvc
))

(in-package :clcl)

(defconstant TWO-PI (+ PI PI))

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


;; (defun rcurry (fn &rest args) 
;;   #'(lambda  (&rest args2) 
;;     (apply fn (append args2 args))))


;;; Standard Illuminant, XYZ, xyY
;;; The nominal range of X, Y, Z, x, y is always [0, 1].

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
    
(defparameter bradford-dictionary
  (make-array (list *number-of-illuminants* *number-of-illuminants*)
	      :element-type '(simple-array double-float (3 3))))
	      
(defun reconstruct-bradford-dictionary ()
  (dotimes (from-index *number-of-illuminants*)
    (dotimes (to-index *number-of-illuminants*)
      (setf (aref bradford-dictionary from-index to-index)
	    (calc-ca-matrix
	     (aref illuminant-array from-index)
	     (aref illuminant-array to-index))))))

(reconstruct-bradford-dictionary)

;; get a transformation matrix by the above constructed dictionary
(defun get-bradford-transformation-matrix (from-illuminant to-illuminant)
  (aref bradford-dictionary
	(illuminant-index from-illuminant)
	(illuminant-index to-illuminant)))

;; get a function for chromatic adaptation
(defun get-ca-converter (from-illuminant to-illuminant)
  (let ((mat (calc-ca-matrix from-illuminant to-illuminant)))
    #'(lambda (x y z)
	(multiply-matrix-and-vec mat x y z))))



;; special function for Bradford transformation between default standard illuminants
(defun bradford (x y z from-illuminant to-illuminant)
  (if (or (< (illuminant-index from-illuminant) 0)
	  (< (illuminant-index to-illuminant) 0))
      (error "The function BRADFORD cannot take a user-defined standard illuminant.
Use CALC-CA-MATRIX and CHROMATIC-ADAPTATION instead.")
      (multiply-matrix-and-vec
       (get-bradford-transformation-matrix from-illuminant to-illuminant)
       x y z)))

(defun xyz-to-xyy (x y z &optional (illuminant d65))
  (if (= x y z 0)
      (list (illuminant-x illuminant) (illuminant-y illuminant) y)
      (list (/ x (+ x y z)) (/ y (+ x y z)) y)))



;;; RGB Color Space
(defparameter identity-matrix
  (make-array '(3 3) :element-type 'double-float
	      :initial-contents '((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 1d0))))

(defun genlinearizer (gamma)
  #'(lambda (x) (expt x (coerce gamma 'double-float))))

(defun gendelinearizer (gamma)
  #'(lambda (x) (expt x (/ 1d0 (coerce gamma 'double-float)))))

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


   
(defparameter srgb
  (new-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
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
  (new-rgbspace 0.64d0 0.33d0 0.21d0 0.71d0 0.15d0 0.06d0
		:linearizer #'(lambda (x)
				(if (<= x 0.0556d0)
				    (* x 0.03125d0)
				    (expt x 2.2d0)))		  
		:delinearizer #'(lambda (x)
				  (if (<= x 0.00174d0)
				      (* x 32d0)
				      (expt x 0.45454545454d0)))))


(defparameter ntsc1953
  (new-rgbspace 0.67d0 0.33d0 0.21d0 0.71d0 0.14d0 0.08d0
		:illuminant clcl:c
		:linearizer (genlinearizer 2.2d0)
		:delinearizer (gendelinearizer 2.2d0)))

(defparameter pal/secam
  (new-rgbspace 0.64d0 0.33d0 0.29d0 0.60d0 0.15d0 0.06d0
		:linearizer (genlinearizer 2.8d0)
		:delinearizer (gendelinearizer 2.8d0)))

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

;; (defun xyz-to-lrgb (x y z &optional (rgbspace srgb))
;;   (list (+ (* 3.2404542d0 x) (* -1.5371385d0 y) (* -0.4985314d0 z))
;; 	(+ (* -0.9692660d0 x) (* 1.8760108d0 y) (* 0.0415560d0 z))
;; 	(+ (* 0.0556434d0 x) (* -0.2040259d0 y) (* 1.0572252d0 z))))


;; convert XYZ to linear RGB in [0, 1]
;; return multiple values: (lr lg lb) and out-of-gamut flag
(defun xyz-to-lrgb (x y z &key (rgbspace srgbd65) (threshold 0))
  (destructuring-bind (lr lg lb)
      (multiply-matrix-and-vec (rgbspace-from-xyz-matrix rgbspace)
				x y z)
    (let ((out-of-gamut (not (and  (nearly<= threshold 0 lr 1)
				   (nearly<= threshold 0 lg 1)
				   (nearly<= threshold 0 lb 1)))))
      (values (list lr lg lb) out-of-gamut))))

(defun lrgb-to-xyz (lr lg lb &optional (rgbspace srgbd65))
  (multiply-matrix-and-vec (rgbspace-to-xyz-matrix rgbspace)
			    lr lg lb))		       

;; convert XYZ to non-linear (i.e. gamma corrected) RGB in [0, 1]
;; return multiple values: (lr lg lb) and out-of-gamut flag
(defun xyz-to-rgb (x y z &key (rgbspace srgbd65) (threshold 0))
  (multiple-value-bind (lrgb out-of-gamut)
      (xyz-to-lrgb x y z :rgbspace rgbspace :threshold threshold)
    (values (mapcar (rgbspace-delinearizer rgbspace) lrgb)
	    out-of-gamut)))

(defun rgb-to-xyz (r g b &optional (rgbspace srgb))
  (let ((lr (linearize r rgbspace))
	(lg (linearize g rgbspace))
	(lb (linearize b rgbspace)))
    (lrgb-to-xyz lr lg lb rgbspace)))


;; convert XYZ to RGB in {0, 1, ..., 255}
;; return multiple values: (r g b) and out-of-gamut-p
(defun xyz-to-rgb255 (x y z &key (rgbspace srgbd65) (threshold 0))
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (mapcar #'(lambda (x) (round (* (clamp x 0 1) 255d0))) rgb)
	    out-of-gamut)))

;; convert RGB ({0, 1, ..., 255}) to XYZ ([0, 1])
(defun rgb255-to-xyz (r g b &optional (rgbspace srgbd65))
  (rgb-to-xyz (/ r 255d0) (/ g 255d0) (/ b 255d0) rgbspace))

(defun rgb255-to-hex (r g b)
  (+ (ash r 16) (ash g 8) b))

(defun hex-to-rgb255 (hex)
  (list (logand (ash hex -16) #xff)
	(logand (ash hex -8) #xff)
	(logand hex #xff)))

(defmacro rgb1+ (x)
  `(clamp (1+ ,x) 0 255))

(defmacro rgb1- (x)
  `(clamp (1- ,x) 0 255))



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

(defun lab-to-xyz (l a b &optional (illuminant d65))
  (let* ((fy (* (+ l 16) 0.008620689655172414d0))
	 (fx (+ fy (* a 0.002d0)))
	 (fz (- fy (* b 0.005d0))))
    (list (if (> fx 0.20689655172413793d0)
	      (* (illuminant-largex illuminant) fx fx fx)
	      (* (- fx 0.13793103448275862d0) 0.12841854934601665d0 (illuminant-largex illuminant)))
	  (if (> fy 0.20689655172413793d0)
	      (* (illuminant-largey illuminant) fy fy fy)
	      (* (- fy 0.13793103448275862d0) 0.12841854934601665d0 (illuminant-largey illuminant)))
	  (if (> fz 0.20689655172413793d0)
	      (* (illuminant-largez illuminant) fz fz fz)
	      (* (- fz 0.13793103448275862d0) 0.12841854934601665d0 (illuminant-largez illuminant))))))

(defun lab-to-xyy (l a b &optional (illuminant d65))
  (destructuring-bind (x y z) (lab-to-xyz l a b illuminant)
    (xyz-to-xyy x y z illuminant)))

(defparameter CONST-TWO-PI/360 (/ TWO-PI 360))
(defparameter CONST-360/TWO-PI (/ 360 TWO-PI))

(defun lab-to-lchab (l a b)
  (list l
	(sqrt (+ (* a a) (* b b)))
	(mod (* (atan b a) CONST-360/TWO-PI) 360d0)))

(defun lchab-to-lab (l c h)
  (let ((hue-two-pi (* h CONST-TWO-PI/360)))
    (list l (* c (cos hue-two-pi)) (* c (sin hue-two-pi)))))

(defun xyz-to-lchab (x y z &optional (illuminant d65))
  (apply #'lab-to-lchab (xyz-to-lab x y z illuminant)))

(defun xyy-to-lchab (x y largey &optional (illuminant d65))
  (apply #'lab-to-lchab (xyy-to-lab x y largey illuminant)))

(defun lchab-to-xyz (l c h &optional (illuminant d65))
  (destructuring-bind (l a b) (lchab-to-lab l c h)
    (lab-to-xyz l a b illuminant)))

(defun lchab-to-xyy (l c h &optional (illuminant d65))
  (destructuring-bind (x y z) (lchab-to-xyz l c h illuminant)
    (xyz-to-xyy x y z illuminant)))

(defun rgb255-to-lab (r g b &optional (rgbspace srgbd65))
  (destructuring-bind (x y z) (rgb255-to-xyz r g b rgbspace)
    (xyz-to-lab x y z)))

;; CIE76
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

(defun rgb255-deltae (r1 g1 b1 r2 g2 b2 &optional (rgbspace srgbd65))
  (destructuring-bind (x1 y1 z1) (rgb255-to-xyz r1 g1 b1 rgbspace)
    (destructuring-bind (x2 y2 z2) (rgb255-to-xyz r2 g2 b2 rgbspace)
      (xyz-deltae x1 y1 z1 x2 y2 z2 (rgbspace-illuminant rgbspace)))))

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
(defun interpolate-in-circle-group (theta1 theta2 coef &optional (perimeter TWO-PI))
  (let ((dtheta (subtract-with-mod theta2 theta1 perimeter)))
    (mod (+ theta1 (* dtheta coef)) perimeter)))

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


;;; HSV/HSL

;; H is in R/360. S and V are in [0, 1].
(defun hsv-to-rgb (h s v)
  (let* ((c (* v s))
	 (h-prime (/ (mod h 360d0) 60d0))
	 (h-prime-int (floor h-prime))
	 (x (* c (- 1d0 (abs (- (mod h-prime 2d0) 1d0)))))
	 (base (- v c)))
    (cond ((= s 0d0) (list base base base))
	  ((= 0 h-prime-int) (list (+ base c) (+ base x) base))
	  ((= 1 h-prime-int) (list (+ base x) (+ base c) base))
	  ((= 2 h-prime-int) (list base (+ base c) (+ base x)))
	  ((= 3 h-prime-int) (list base (+ base x) (+ base c)))
	  ((= 4 h-prime-int) (list (+ base x) base (+ base c)))
	  ((= 5 h-prime-int) (list (+ base c) base (+ base x))))))
	 
(defun hsv-to-rgb255 (h s v)
  (mapcar #'(lambda (x) (round (* x 255d0)))
	  (hsv-to-rgb h s v)))

;; The received HSV color is regarded as converted from a non-linear (i.e. gamma-corrected) RGB color.
(defun hsv-to-xyz (h s v &optional (rgbspace srgbd65))
  (destructuring-bind (r g b) (hsv-to-rgb h s v)
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
	 
(defun rgb255-to-hsv (r g b)
  (rgb-to-hsv (* r 0.00392156862745098d0)
	      (* g 0.00392156862745098d0)
	      (* b 0.00392156862745098d0)))

(defun xyz-to-hsv (x y z &key (rgbspace srgbd65) (threshold 0))
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-hsv
		   (mapcar #'(lambda (i) (clamp i 0d0 1d0)) rgb))
	    out-of-gamut)))
  
