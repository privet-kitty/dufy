;;;
;;; Spectrum, Illuminant, XYZ, xyY
;;;

(in-package :dufy)
    
(defun xyy-to-xyz (small-x small-y y)
  (if (zerop small-y)
      (values 0d0 y 0d0)
      (values (/ (* small-x y) small-y) 
	      y
	      (/ (* (- 1d0 small-x small-y) y) small-y))))


(defun xyz-to-xyy (x y z)
  (let ((sum (+ x y z)))
    (if (= sum 0)
	(values 0d0 0d0 y)
	(values (/ x sum) (/ y sum) y))))
  
(defun gen-spectrum (spectrum-array &optional (wl-begin 360) (wl-end 830))
  "Note: SPECTRUM-ARRAY must be (simple-array double-float *).

Returns a spectral power distribution
function, #'(lambda (wavelength-nm) ...), by linearly interpolating
SPECTRUM-ARRAY which can have arbitrary length.
"
  (declare (optimize (speed 3) (safety 1))
	   ((simple-array double-float) spectrum-array))
  (let* ((size (- (length spectrum-array) 1))
	 (wl-begin-f (float wl-begin 1d0))
	 (wl-end-f (float wl-end 1d0)))
    (if (= size (- wl-end wl-begin))
	;; If SPECTRUM-ARRAY is defined just for each integer,
	;; the spectrum function is simple:
	#'(lambda (wl-nm)
	    (multiple-value-bind (quot rem)
		(floor (- (clamp (float wl-nm 1d0) wl-begin-f wl-end-f)
			  wl-begin-f))
	      (lerp rem
		    (aref spectrum-array quot)
		    (aref spectrum-array (min (1+ quot) size)))))
	(let* ((band (/ (- wl-end-f wl-begin-f) size))
	       (/band (/ band)))
	  #'(lambda (wl-nm)
	      (let* ((wl$ (- (clamp (float wl-nm 1d0) wl-begin-f wl-end-f)
			     wl-begin-f))
		     (frac (mod wl$ band))
		     (coef (* frac /band))
		     (idx (round (* (- wl$ frac) /band))))
		(lerp coef
		      (aref spectrum-array idx)
		      (aref spectrum-array (min (+ idx 1) size)))))))))


;;;
;;; Observer
;;;

(defstruct (observer (:constructor $make-observer))
  "Structure of color matching functions."
  (begin-wl 360 :type (integer 0))
  (end-wl 830 :type (integer 0))
  (cmf-arr (make-array '(471 3) :element-type 'double-float)
	   :type (simple-array double-float))
  ;; Functions based on cmf-arr
  (cmf-x #'empty-function :type function)
  (cmf-y #'empty-function :type function)
  (cmf-z #'empty-function :type function)
  (cmf #'empty-function :type function))


(defun make-observer (cmf-arr &optional (begin-wl 360) (end-wl 830))
  "Defines an observer based on CMF arrays, which must
be (SIMPLE-ARRAY DOUBLE-FLOAT (* 3))."
  (declare (optimize (speed 3) (safety 1)))
  (labels ((gen-cmf-1 (arr num &optional (wl-begin 360) (wl-end 830))
	     ;; verbose, almost equivalent to GEN-SPECTRUM
	     (declare ((simple-array double-float (* 3)) arr))
	     (let* ((size (- (array-dimension arr 0) 1))
		    (wl-begin-f (float wl-begin 1d0))
		    (wl-end-f (float wl-end 1d0)))
	       (if (= size (- wl-end wl-begin))
		   #'(lambda (wl)
		       (multiple-value-bind (quot rem)
			   (floor (- (clamp (float wl 1d0) wl-begin-f wl-end-f) wl-begin-f))
			 (lerp rem
			       (aref arr quot num)
			       (aref arr (min (1+ quot) size) num))))
		   (let* ((band (/ (- wl-end-f wl-begin-f) size))
			  (/band (/ band)))
		     #'(lambda (wl)
			 (let* ((wl$ (- (clamp (float wl 1d0) wl-begin-f wl-end-f) wl-begin-f))
				(frac (mod wl$ band))
				(coef (* frac /band))
				(idx (round (* (- wl$ frac) /band))))
			   (lerp coef
				 (aref arr idx num)
				 (aref arr (min (+ idx 1) size) num))))))))
	   (gen-cmf-3 (arr &optional (wl-begin 360) (wl-end 830))
	     (declare ((simple-array double-float (* 3)) arr))
	     (let* ((size (- (array-dimension arr 0) 1))
		    (wl-begin-f (float wl-begin 1d0))
		    (wl-end-f (float wl-end 1d0)))
	       (if (= size (- wl-end wl-begin))
		   #'(lambda (wl)
		       (multiple-value-bind (quot rem)
			   (floor (- (clamp (float wl 1d0) wl-begin-f wl-end-f) wl-begin-f))
			 (values (lerp rem
				       (aref arr quot 0)
				       (aref arr (min (1+ quot) size) 0))
				 (lerp rem
				       (aref arr quot 1)
				       (aref arr (min (1+ quot) size) 1))
				 (lerp rem
				       (aref arr quot 2)
				       (aref arr (min (1+ quot) size) 2)))))
		   (let* ((band (/ (- wl-end-f wl-begin-f) size))
			  (/band (/ band)))
		     #'(lambda (wl)
			 (let* ((wl$ (- (clamp (float wl 1d0) wl-begin-f wl-end-f) wl-begin-f))
				(frac (mod wl$ band))
				(coef (* frac /band))
				(idx (round (* (- wl$ frac) /band))))
			   (values (lerp coef
					 (aref arr idx 0)
					 (aref arr (min (+ idx 1) size) 0))
				   (lerp coef
					 (aref arr idx 1)
					 (aref arr (min (+ idx 1) size) 1))
				   (lerp coef
					 (aref arr idx 2)
					 (aref arr (min (+ idx 1) size) 2))))))))))
    ($make-observer
     :begin-wl begin-wl
     :end-wl end-wl
     :cmf-arr cmf-arr
     :cmf-x (gen-cmf-1 cmf-arr 0 begin-wl end-wl)
     :cmf-y (gen-cmf-1 cmf-arr 1 begin-wl end-wl)
     :cmf-z (gen-cmf-1 cmf-arr 2 begin-wl end-wl)
     :cmf (gen-cmf-3 cmf-arr
		     begin-wl end-wl))))

(defparameter +obs-cie1931+ (make-observer cmf-arr-cie1931))
(defparameter +obs-cie1964+ (make-observer cmf-arr-cie1964))



;; s0, s1, s2
;; http://www.rit.edu/cos/colorscience/rc_useful_data.php
(defparameter +s0-arr+
  #.(make-array 54 :element-type 'double-float
		:initial-contents '(0.04d0 6d0 29.6d0 55.3d0 57.3d0 61.8d0 61.5d0 68.8d0 63.4d0 65.8d0 94.8d0 104.8d0 105.9d0 96.8d0 113.9d0 125.6d0 125.5d0 121.3d0 121.3d0 113.5d0 113.1d0 110.8d0 106.5d0 108.8d0 105.3d0 104.4d0 100d0 96d0 95.1d0 89.1d0 90.5d0 90.3d0 88.4d0 84d0 85.1d0 81.9d0 82.6d0 84.9d0 81.3d0 71.9d0 74.3d0 76.4d0 63.3d0 71.7d0 77d0 65.2d0 47.7d0 68.6d0 65d0 66d0 61d0 53.3d0 58.9d0 61.9d0)))
(defparameter +s1-arr+
  #.(make-array 54 :element-type 'double-float
		:initial-contents '(0.02d0 4.5d0 22.4d0 42d0 40.6d0 41.6d0 38d0 42.4d0 38.5d0 35d0 43.4d0 46.3d0 43.9d0 37.1d0 36.7d0 35.9d0 32.6d0 27.9d0 24.3d0 20.1d0 16.2d0 13.2d0 8.6d0 6.1d0 4.2d0 1.9d0 0d0 -1.6d0 -3.5d0 -3.5d0 -5.8d0 -7.2d0 -8.6d0 -9.5d0 -10.9d0 -10.7d0 -12d0 -14d0 -13.6d0 -12d0 -13.3d0 -12.9d0 -10.6d0 -11.6d0 -12.2d0 -10.2d0 -7.8d0 -11.2d0 -10.4d0 -10.6d0 -9.7d0 -8.3d0 -9.3d0 -9.8d0)))
(defparameter +s2-arr+
       #.(make-array 54 :element-type 'double-float
		     :initial-contents '(0d0 2d0 4d0 8.5d0 7.8d0 6.7d0 5.3d0 6.1d0 2d0 1.2d0 -1.1d0 -0.5d0 -0.7d0 -1.2d0 -2.6d0 -2.9d0 -2.8d0 -2.6d0 -2.6d0 -1.8d0 -1.5d0 -1.3d0 -1.2d0 -1d0 -0.5d0 -0.3d0 0d0 0.2d0 0.5d0 2.1d0 3.2d0 4.1d0 4.7d0 5.1d0 6.7d0 7.3d0 8.6d0 9.8d0 10.2d0 8.3d0 9.6d0 8.5d0 7d0 7.6d0 8d0 6.7d0 5.2d0 7.4d0 6.8d0 7d0 6.4d0 5.5d0 6.1d0 6.5d0)))

(declaim (type (function * double-float) +s0-func+ +s1-func+ +s2-func+))
(defparameter +s0-func+ (gen-spectrum +s0-arr+ 300 830))
(defparameter +s1-func+ (gen-spectrum +s1-arr+ 300 830))
(defparameter +s2-func+ (gen-spectrum +s2-arr+ 300 830))

(defun gen-illum-d-spectrum-array (temperature &optional (wl-begin 300) (wl-end 830))
  (declare (optimize (speed 3) (safety 1))
	   (fixnum wl-begin wl-end))
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
		 (+ (funcall +s0-func+ wl)
		    (* m1 (funcall +s1-func+ wl))
		    (* m2 (funcall +s2-func+ wl)))))
      arr)))

(defun gen-illum-d-spectrum (temperature)
  "Generates the spectrum of the illuminant series D for a given
temperature."
  (gen-spectrum (gen-illum-d-spectrum-array temperature 300 830)
		300 830))

				 
(defun spectrum-sum (spectrum &key (wl-begin 300) (wl-end 830) (band 1))
  (loop for wl from wl-begin to wl-end by band
     sum (funcall spectrum wl)))


(defun bb-spectrum (wavelength-nm &optional (temperature 5000))
  "Spectrum function of a blackbody. It is not normalized."
  (let ((wlm (* wavelength-nm 1d-9)))
    (/ (* 3.74183d-16 (expt wlm -5d0))
       (- (exp (/ 1.4388d-2 (* wlm temperature))) 1d0))))

(defun optimal-spectrum (wavelength-nm &optional (wl1 300) (wl2 830))
  "Spectrum function of optimal colors:
In the case wl1 <= wl2:
f(x) = 1d0 if wl1 <= x <= wl2,
f(x) = 0d0 otherwise.
In the case wl1 > wl2:
f(x) = 1d0 if x <=wl2 or wl1 <= x,
f(x) = 0d0 otherwise.
"
  (if (<= wl1 wl2)
      (if (<= wl1 wavelength-nm wl2) 1d0 0d0)
      (if (or (<= wavelength-nm wl2) (<= wl1 wavelength-nm)) 1d0 0d0)))

(defun flat-spectrum (wavelength-nm)
  "(constantly 1d0)"
  (declare (ignore wavelength-nm))
  1d0)
    
(defun scale-xyz (x y z &optional (scale-to 1))
  "X, Y, Z are scaled so as to satisfy X+Y+Z = SCALE-TO."
  (let ((factor (* scale-to (/ (+ x y z)))))
    (values (* x factor)
	    (* y factor)
	    (* z factor))))


;;; Standard Illuminant, XYZ, xyY
;;; The nominal range of Y is always [0, 1].

(defstruct (illuminant (:constructor $make-illuminant)
		       (:copier nil))
  (small-x 0.0 :type double-float)
  (small-y 0.0 :type double-float)
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (z 0.0 :type double-float)
  (spectrum #'empty-function :type function)
  (observer +obs-cie1931+ :type observer)
  ;; used for xyz-to-spectrum conversion
  (to-spectrum-matrix +empty-matrix+ :type (simple-array double-float (3 3))))

(defvar +illum-e+) ;; to avoid WARNING
(defun spectrum-to-xyz (spectrum &optional (illuminant +illum-e+))
  "Computes XYZ values from SPECTRUM in reflective and transmissive
case. The function SPECTRUM must be defined at least in [360, 830].
The return values are not normalized."
  (when (eq #'empty-function (illuminant-spectrum illuminant))
    (let ((*print-array* nil))
      (error (make-condition 'no-spd-error :illuminant illuminant))))
  (spectrum-to-xyz-raw spectrum
		       (illuminant-spectrum illuminant)
		       (illuminant-observer illuminant)))

(defun spectrum-to-xyz-raw (spectrum illum-spectrum observer)
  "Another version of spectrum-to-xyz: only the return value
of (illuminant-spectrum illuminant) and observer are necessary in
spectrum-to-xyz."
  (declare (optimize (speed 3) (safety 1))
	   ((function * double-float) spectrum illum-spectrum))
  (let ((x 0d0) (y 0d0) (z 0d0) (max-y 0d0)
	(arr (observer-cmf-arr observer)))
    (declare (double-float x y z max-y))
    (loop for wl from 360 to 830 do
	 (let ((p (funcall illum-spectrum wl))
	       (reflec (funcall spectrum wl))
	       (idx (- wl 360)))
	   (progn
	     (incf x (* (aref arr idx 0) p reflec))
	     (incf y (* (aref arr idx 1) p reflec))
	     (incf z (* (aref arr idx 2) p reflec))
	     (incf max-y (* (aref arr idx 1) p)))))
    (let ((factor (/ max-y)))
      (values (* x factor) (* y factor) (* z factor)))))

;; (defun spectrum-to-lchab (spectrum &key (illuminant +illum-e+) (observer +obs-cie1931+))
;;   (apply (rcurry #'xyz-to-lchab illuminant)
;; 	 (spectrum-to-xyz spectrum :illuminant illuminant :observer observer)))


(let ((mat (make-array '(3 3)
		       :element-type 'double-float
		       :initial-element 0d0)))
  (defun calc-to-spectrum-matrix (illum-spectrum observer)
    (declare (optimize (speed 3) (safety 1)))
    (multiple-value-bind (a00 a10 a20)
	(spectrum-to-xyz-raw (observer-cmf-x observer) illum-spectrum observer)
      (multiple-value-bind (a01 a11 a21)
	  (spectrum-to-xyz-raw (observer-cmf-y observer) illum-spectrum observer)
	(multiple-value-bind (a02 a12 a22)
	    (spectrum-to-xyz-raw (observer-cmf-z observer) illum-spectrum observer)
	  (setf (aref mat 0 0) a00
		(aref mat 0 1) a01
		(aref mat 0 2) a02
		(aref mat 1 0) a10
		(aref mat 1 1) a11
		(aref mat 1 2) a12
		(aref mat 2 0) a20
		(aref mat 2 1) a21
		(aref mat 2 2) a22))))
    (invert-matrix33 mat)))

(defun xyz-to-spectrum (x y z &optional (illuminant +illum-e+))
  "Converts XYZ to spectrum, which is, of course, a spectrum among
many."
  (let ((mat (illuminant-to-spectrum-matrix illuminant)))
    (if (eq mat +empty-matrix+)
	(let ((*print-array* nil))
	  (error (make-condition 'no-spd-error :illuminant illuminant)))
	(let ((observer (illuminant-observer illuminant)))
	  (multiple-value-bind (fac-x fac-y fac-z)
	      (multiply-mat-vec (illuminant-to-spectrum-matrix illuminant) x y z)
	    #'(lambda (wl)
		(+ (* fac-x (funcall (observer-cmf-x observer) wl))
		   (* fac-y (funcall (observer-cmf-y observer) wl))
		   (* fac-z (funcall (observer-cmf-z observer) wl)))))))))
    
	    
(defun make-illuminant (small-x small-y &optional (spectrum nil) (observer +obs-cie1931+))
  "Defines an illuminant based on a white point. No error occurs, even
if the given (small-x, small-y) and SPD contradicts to each other."
  (multiple-value-bind (x y z) (xyy-to-xyz small-x small-y 1d0)
    ($make-illuminant :small-x (float small-x 1d0)
		      :small-y (float small-y 1d0)
		      :x (float x 1d0)
		      :y (float y 1d0)
		      :z (float z 1d0)
		      :spectrum (or spectrum #'empty-function)
		      :observer observer
		      :to-spectrum-matrix (if spectrum
					      (calc-to-spectrum-matrix spectrum observer)
					      +empty-matrix+))))

(defun make-illuminant-by-spd (spectrum &optional (observer +obs-cie1931+))
  "Defines an illuminant based on a spectral power distribution. The
proper white point is automatically calculated."
  (multiple-value-bind (x y z)
      (spectrum-to-xyz-raw #'flat-spectrum spectrum observer)
    (multiple-value-bind (small-x small-y disused)
	(xyz-to-xyy x y z)
      (declare (ignore disused))
      ($make-illuminant :small-x (float small-x 1d0)
			:small-y (float small-y 1d0)
			:x (float x 1d0)
			:y (float y 1d0)
			:z (float z 1d0)
			:spectrum (or spectrum #'flat-spectrum)
			:observer observer
			:to-spectrum-matrix (calc-to-spectrum-matrix spectrum observer)))))

(define-condition no-spd-error (simple-error)
  ((illuminant :initarg :illuminant
	       :initform nil
	       :accessor cond-illuminant))
  (:report (lambda (condition stream)
	     (format stream "The illuminant has no spectrum: ~A"
		     (cond-illuminant condition)))))

(defparameter +illum-a+
  (make-illuminant 0.44757d0 0.40745d0
		   #'(lambda (wl)
		       (declare (optimize (speed 3) (safety 1)))
		       (let ((wl (float wl 1d0)))
			 (* 100d0
			    (expt (/ 560d0 wl) 5)
			    (/ #.(- (exp (/ 1.435d7 (* 2848 560))) 1d0)
			       (- (exp (/ 1.435d7 (* 2848d0 wl))) 1d0)))))))
			  
(defparameter +illum-b+ (make-illuminant 0.34842d0 0.35161d0)) ; no spd

(defparameter +illum-c-arr+ (make-array 107 :element-type 'double-float
					:initial-contents '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.20d0 0.40d0 1.55d0 2.70d0 4.85d0 7.00d0 9.95d0 12.90d0 17.20d0 21.40d0 27.5d0 33.00d0 39.92d0 47.40d0 55.17d0 63.30d0 71.81d0 80.60d0 89.53d0 98.10d0 105.80d0 112.40d0 117.75d0 121.50d0 123.45d0 124.00d0 123.60d0 123.10d0 123.30d0 123.80d0 124.09d0 123.90d0 122.92d0 120.70d0 116.90d0 112.10d0 106.98d0 102.30d0 98.81d0 96.90d0 96.78d0 98.00d0 99.94d0 102.10d0 103.95d0 105.20d0 105.67d0 105.30d0 104.11d0 102.30d0 100.15d0 97.80d0 95.43d0 93.20d0 91.22d0 89.70d0 88.83d0 88.40d0 88.19d0 88.10d0 88.06d0 88.00d0 87.86d0 87.80d0 87.99d0 88.20d0 88.20d0 87.90d0 87.22d0 86.30d0 85.30d0 84.00d0 82.21d0 80.20d0 78.24d0 76.30d0 74.36d0 72.40d0 70.40d0 68.30d0 66.30d0 64.40d0 62.80d0 61.50d0 60.20d0 59.20d0 58.50d0 58.10d0 58.00d0 58.20d0 58.50d0 59.10d0 78.91d0 79.55d0 76.48d0 73.40d0 68.66d0 63.92d0 67.35d0 70.78d0 72.61d0 74.44d0)))
(defparameter +illum-c+ (make-illuminant 0.31006d0 0.31616d0
					 (gen-spectrum +illum-c-arr+ 300 830)))

(defparameter +illum-d50+ (make-illuminant 0.34567d0 0.35850d0
					   (gen-illum-d-spectrum #.(* 5000 (/ 1.4388d0 1.438)))))
(defparameter +illum-d65+ (make-illuminant 0.31271d0 0.32902d0
					   (gen-illum-d-spectrum #.(* 6500 (/ 1.43880d0 1.438)))))
(defparameter +illum-e+ (make-illuminant #.(float 1/3 1d0)
					 #.(float 1/3 1d0)
					 #'flat-spectrum))




;;; LMS, chromatic adaptation
(defstruct (cat (:constructor $make-cat))
  "Model of chromatic adaptation transformation. Currently only linear
models are available."
  (matrix +empty-matrix+ :type (simple-array double-float (3 3)))
  (inv-matrix +empty-matrix+ :type (simple-array double-float (3 3))))

(defun make-cat (mat)
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


(defun xyz-to-lms (x y z &key (illuminant nil) (cat +bradford+))
  "Note: The default illuminant is **not** D65; if ILLUMINANT is NIL,
the transform is virtually equivalent to that of illuminant E. "
  (declare (optimize (speed 3) (safety 1)))
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
      (multiply-mat-vec (cat-matrix cat) x y z)))
	    

(defun lms-to-xyz (l m s &key (illuminant nil) (cat +bradford+))
  "Note: The default illuminant is **not** D65; if ILLUMINANT is NIL,
the transform is virtually equivalent to that of illuminant E. "
  (declare (optimize (speed 3) (safety 1)))
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
      (multiply-mat-vec (cat-inv-matrix cat) l m s)))
  

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
	(tmatrix (cat-matrix cat))
	(inv-tmatrix (cat-inv-matrix cat)))
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
	(let ((matrix2 (make-array '(3 3) :element-type 'double-float)))
	  (dotimes (i 3)
	    (dotimes (j 3)
	      (do ((sum 0d0 (+ sum (* (aref inv-tmatrix i k)
				      (aref matrix1 k j))))
		   (k 0 (1+ k)))
		  ((= k 3) 
		   (setf (aref matrix2 i j) sum)))))
	  matrix2)))))


(declaim (ftype (function * function) gen-cat-function))
(defun gen-cat-function (from-illuminant to-illuminant &optional (tmatrix +bradford+))
  "Returns a chromatic adaptation function on XYZ space:
> (funcall (gen-cat-function +illum-d65+ +illum-e+) 0.9504d0 1.0d0 1.0889d0)
=> (0.9999700272441295d0 0.999998887365445d0 0.9999997282885571d0)
"
  (let ((mat (calc-cat-matrix from-illuminant to-illuminant tmatrix)))
    #'(lambda (x y z)
	(multiply-mat-vec mat x y z))))

;; (declaim (ftype (function * function) gen-cat-function-xyy))
;; (defun gen-cat-function-xyy (from-illuminant to-illuminant &optional (tmatrix +bradford+))
;;   "Returns a chromatic adaptation function of xyY values: #'(lambda (SMALL-X SMALL-Y Y) ...)"
;;   (let ((ca-func (gen-cat-function from-illuminant to-illuminant tmatrix)))
;;     #'(lambda (small-x small-y y)
;; 	(apply #'xyz-to-xyy
;; 	       (apply ca-func
;; 		      (xyy-to-xyz small-x small-y y))))))

(defun normalize-xyz (x y z)
  "Normalizes Y to 1."
  (let ((/y (/ y)))
    (list (* /y x)
	  1d0
	  (* /y z))))
