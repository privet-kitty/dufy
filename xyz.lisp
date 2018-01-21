;;; Spectrum, Illuminant, XYZ, xyY

(in-package :dufy)

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


(defun gen-spectrum (spectrum-array &optional (wl-begin 360) (wl-end 830))
  "Returns a spectral power distribution function,
#'(lambda (wavelength) ...) : [WL-BEGIN, WL-END] -> R,
by interpolating SPECTRUM-ARRAY linearly which can have arbitrary size."
  (let* ((size (- (length spectrum-array) 1)))
    (if (= size (- wl-end wl-begin))
	;; if SPECTRUM-ARRAY is defined for each integer, the SPECTRUM function is simple:
	#'(lambda (wavelength)
	    (multiple-value-bind (quot rem)
		(floor (- (clamp wavelength wl-begin wl-end) wl-begin))
	      (lerp rem
		    (aref spectrum-array quot)
		    (aref spectrum-array (min (1+ quot) size)))))
	(let* ((band (float (/ (- wl-end wl-begin) size) 1d0))
	       (/band (/ band)))
	  #'(lambda (wavelength)
	      (let* ((wl$ (- (clamp wavelength wl-begin wl-end) wl-begin))
		     (frac (mod wl$ band))
		     (coef (* frac /band))
		     (idx (round (* (- wl$ frac) /band))))
		(lerp coef
		      (aref spectrum-array idx)
		      (aref spectrum-array (min (+ idx 1) size)))))))))

;; s0, s1, s2
;; http://www.rit.edu/cos/colorscience/rc_useful_data.php
(defparameter s0-arr
  #.(make-array 54 :element-type 'double-float
	      :initial-contents '(0.04d0 6d0 29.6d0 55.3d0 57.3d0 61.8d0 61.5d0 68.8d0 63.4d0 65.8d0 94.8d0 104.8d0 105.9d0 96.8d0 113.9d0 125.6d0 125.5d0 121.3d0 121.3d0 113.5d0 113.1d0 110.8d0 106.5d0 108.8d0 105.3d0 104.4d0 100d0 96d0 95.1d0 89.1d0 90.5d0 90.3d0 88.4d0 84d0 85.1d0 81.9d0 82.6d0 84.9d0 81.3d0 71.9d0 74.3d0 76.4d0 63.3d0 71.7d0 77d0 65.2d0 47.7d0 68.6d0 65d0 66d0 61d0 53.3d0 58.9d0 61.9d0)))

(defparameter s1-arr
  #.(make-array 54 :element-type 'double-float
	      :initial-contents '(0.02d0 4.5d0 22.4d0 42d0 40.6d0 41.6d0 38d0 42.4d0 38.5d0 35d0 43.4d0 46.3d0 43.9d0 37.1d0 36.7d0 35.9d0 32.6d0 27.9d0 24.3d0 20.1d0 16.2d0 13.2d0 8.6d0 6.1d0 4.2d0 1.9d0 0d0 -1.6d0 -3.5d0 -3.5d0 -5.8d0 -7.2d0 -8.6d0 -9.5d0 -10.9d0 -10.7d0 -12d0 -14d0 -13.6d0 -12d0 -13.3d0 -12.9d0 -10.6d0 -11.6d0 -12.2d0 -10.2d0 -7.8d0 -11.2d0 -10.4d0 -10.6d0 -9.7d0 -8.3d0 -9.3d0 -9.8d0 )))

(defparameter s2-arr
  #.(make-array 54 :element-type 'double-float
	      :initial-contents '(0d0 2d0 4d0 8.5d0 7.8d0 6.7d0 5.3d0 6.1d0 2d0 1.2d0 -1.1d0 -0.5d0 -0.7d0 -1.2d0 -2.6d0 -2.9d0 -2.8d0 -2.6d0 -2.6d0 -1.8d0 -1.5d0 -1.3d0 -1.2d0 -1d0 -0.5d0 -0.3d0 0d0 0.2d0 0.5d0 2.1d0 3.2d0 4.1d0 4.7d0 5.1d0 6.7d0 7.3d0 8.6d0 9.8d0 10.2d0 8.3d0 9.6d0 8.5d0 7d0 7.6d0 8d0 6.7d0 5.2d0 7.4d0 6.8d0 7d0 6.4d0 5.5d0 6.1d0 6.5d0)))


(defparameter s0-func (gen-spectrum s0-arr 300 830))
(defparameter s1-func (gen-spectrum s1-arr 300 830))
(defparameter s2-func (gen-spectrum s2-arr 300 830))

(defun gen-illum-d-spectrum-array (temperature &optional (wl-begin 300) (wl-end 830))
  (labels ((calc-xd (temp)
	     (let ((/temp (/ temp)))
	       (if (<= temp 7000d0)
		   (+ 0.244063d0 (* /temp (+ 0.09911d3 (* /temp (+ 2.9678d6 (* /temp -4.607d9))))))
		   (+ 0.234040d0 (* /temp (+ 0.24748d3 (* /temp (+ 1.9018d6 (* /temp -2.0064d9)))))))))
	   (calc-yd (xd)
	     (+ -0.275d0 (* xd (+ 2.870d0 (* xd -3d0))))))
    (let* ((xd (calc-xd (float temperature 1d0)))
	   (yd (calc-yd xd))
	   (m (+ 0.0241d0 (* xd 0.2562d0) (* yd -0.7341d0)))
	   (m1 (/ (+ -1.3515d0 (* xd -1.7703d0) (* yd 5.9114d0)) m))
	   (m2 (/ (+ 0.03d0 (* xd -31.4424d0) (* yd 30.0717d0)) m))
	   (arr (make-array (1+ (- wl-end wl-begin))
			    :element-type 'double-float
			    :initial-element 0d0)))
      (loop for wl from wl-begin to wl-end do
	   (setf (aref arr (- wl wl-begin))
		 (+ (funcall s0-func wl)
		    (* m1 (funcall s1-func wl))
		    (* m2 (funcall s2-func wl)))))
      arr)))

(defun gen-illum-d-spectrum (temperature &optional (wl-begin 300) (wl-end 830))
  (gen-spectrum (gen-illum-d-spectrum-array temperature wl-begin wl-end)
	   wl-begin wl-end))
	   

				 
(defun spectrum-sum (spectrum &key (wl-begin 360) (wl-end 780) (band 1))
  (loop for wl from wl-begin to wl-end by band
     sum (funcall spectrum wl)))


;; the spectrum of a black body
(defun bb-spectrum (wavelength &optional (temperature 5000))
  (let ((wlm (* wavelength 1d-9)))
    (/ (* 3.74183d-16 (expt wlm -5d0))
       (- (exp (/ 1.4388d-2 (* wlm temperature))) 1d0))))

(defun optimal-spectrum (wavelength &optional (wl1 300) (wl2 830))
  (if (<= wl1 wl2)
      (if (<= wl1 wavelength wl2) 1 0)
      (if (or (<= wavelength wl2) (<= wl1 wavelength)) 1 0)))

(defun flat-spectrum (wavelength)
  (declare (ignore wavelength))
  1d0)

(defun scale-xyz (x y z &optional (scale-to 1))
  "X, Y, Z are scaled so as to satisfy X+Y+Z = SCALE-TO."
  (let ((factor (* scale-to (/ (+ x y z)))))
    (list (* x factor)
	  (* y factor)
	  (* z factor))))

;; (defun scale-lrgb-until-saturated (lr lg lb)
;;   "LR, LG, LB are scaled so that one of them is saturated."
;;   (let ((max (max lr lg lb)))
;;     (if (<= max 0)
;; 	(list lr lg lb)
;; 	(let ((factor (/ max)))
;; 	  (list (* lr factor)
;; 		(* lg factor)
;; 		(* lb factor))))))


;; (let ((e-to-d65 (gen-ca-converter illum-e illum-d65)))
;;   (defun temperature-test (temp)
;;     (apply #'rgb-to-rgb255
;; 	   (apply #'lrgb-to-rgb
;; 		  (apply #'scale-lrgb-until-saturated
;; 			 (mapcar (rcurry #'clamp 0 1)
;; 				 (apply #'xyz-to-lrgb
;; 					(apply e-to-d65
;; 					       (spectrum-to-xyz (compose (rcurry #'* (/ (spectrum-sum (rcurry #'bb-spectrum temp))))
;; 									 (rcurry #'bb-spectrum temp)))))))))))
  


;;; Standard Illuminant, XYZ, xyY
;;; The nominal range of X, Y, Z, x, y is always [0, 1].

(defstruct illuminant
  "The support of SPECTRUM is not completed."
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (largex 0.0 :type double-float)
  (largey 0.0 :type double-float)
  (largez 0.0 :type double-float)
  (spectrum #'flat-spectrum :type function))

(defvar illum-e) ;; avoid a WARNING
(defun spectrum-to-xyz (spectrum &key (illuminant illum-e) (observer :1931))
  "The function SPECTRUM must be defined at least in [360, 780].
The return values are not normalized."
  (let ((const 0.009358316379939337d0) ; (/ (dufy::spectrum-sum #'dufy:color-matching-y))
	(x 0) (y 0) (z 0))
    (loop for wl from 360 to 780 do
	 (let ((p (funcall (illuminant-spectrum illuminant) wl))
	       (reflec (funcall spectrum wl))
	       (idx (- wl 360)))
	   (if (eq observer :1931)
	       (progn
		 (incf x (* (aref color-matching-arr-1931 idx 0) p reflec))
		 (incf y (* (aref color-matching-arr-1931 idx 1) p reflec))
		 (incf z (* (aref color-matching-arr-1931 idx 2) p reflec)))
	       (progn
		 (incf x (* (aref color-matching-arr-1964 idx 0) p reflec))
		 (incf y (* (aref color-matching-arr-1964 idx 1) p reflec))
		 (incf z (* (aref color-matching-arr-1964 idx 2) p reflec))))))
    (list (* x const) (* y const) (* z const))))

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


(defun new-illuminant (x y &optional (spectrum #'flat-spectrum))
  (destructuring-bind (largex largey largez) (xyy-to-xyz x y 1d0)
    (make-illuminant :x (float x 1d0)
		     :y (float y 1d0)
		     :largex (float largex 1d0)
		     :largey (float largey 1d0)
		     :largez (float largez 1d0)
		     :spectrum spectrum)))

(defparameter illum-a (new-illuminant 0.44757d0 0.40745d0))
(defparameter illum-c (new-illuminant 0.31006d0 0.31616d0))
(defparameter illum-d50 (new-illuminant 0.34567d0 0.35850d0 (gen-illum-d-spectrum 5003)))
(defparameter illum-d65 (new-illuminant 0.31271d0 0.32902d0 (gen-illum-d-spectrum 6504)))
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

(defun calc-ca-matrix  (from-illuminant to-illuminant &optional (tmatrix bradford))
  "Returns a 3*3 chromatic adaptation matrix."
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


(declaim (ftype (function * function) gen-ca-converter))
(defun gen-ca-converter (from-illuminant to-illuminant &optional (tmatrix bradford))
  "Returns a chromatic adaptation function of XYZ color: #'(lambda (X Y Z) ...)"
  (let ((mat (calc-ca-matrix from-illuminant to-illuminant tmatrix)))
    #'(lambda (x y z)
	(multiply-matrix-and-vec mat x y z))))

(declaim (ftype (function * function) gen-ca-converter-xyy))
(defun gen-ca-converter-xyy (from-illuminant to-illuminant &optional (tmatrix bradford))
  "Returns a chromatic adaptation function of xyY color: #'(lambda (X Y LARGEY) ...)"
  (let ((ca-func (gen-ca-converter from-illuminant to-illuminant tmatrix)))
    #'(lambda (x y largey)
	(apply #'xyz-to-xyy
	       (apply ca-func
		      (xyy-to-xyz x y largey))))))
