;;;
;;; Spectrum, Illuminant, XYZ, xyY
;;;

(in-package :dufy)


(declaim (inline xyy-to-xyz))
(defun xyy-to-xyz (small-x small-y y)
  "xyY to XYZ. The nominal range of Y is [0, 1], though all real
values are accepted."
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (small-x small-y y)
    (if (zerop small-y)
	(values 0d0 y 0d0)
	(values (/ (* small-x y) small-y) 
		y
		(/ (* (- 1d0 small-x small-y) y) small-y)))))

(declaim (inline xyz-to-xyy))
(defun xyz-to-xyy (x y z)
  "XYZ to xyY. The nominal range of Y is [0, 1], though all real
values are accepted."
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (x y z)
    (let ((sum (+ x y z)))
      (if (= sum 0)
	  (values 0d0 0d0 y)
	  (values (/ x sum) (/ y sum) y)))))


(defun gen-spectrum (spectrum-seq &optional (begin-wl 360) (end-wl 830))
  "A spectrum is just a function which takes a real number as
wavelength (nm) and returns a double-float.

GEN-SPECTRUM returns a spectral power distribution
function, #'(lambda (wavelength-nm) ...), which interpolates
SPECTRUM-SEQ linearly.

Note: SPECTRUM-SEQ must be a sequence of double-float.
If the type of SPECTRUM-SEQ is (simple-array double-float (*)), it is
not copied but referenced, otherwise it is copied by (coerce
spectrum-seq '(simple-array double-float (*)))."
  (check-type spectrum-seq sequence)
  (let* ((spectrum-arr (if (typep spectrum-seq '(simple-array double-float (*)))
			   spectrum-seq
			   (coerce spectrum-seq '(simple-array double-float (*)))))
	 (size (- (length spectrum-arr) 1))
	 (begin-wl-f (float begin-wl 1d0))
	 (end-wl-f (float end-wl 1d0)))
    (if (= size (- end-wl begin-wl))
	;; If SPECTRUM-SEQ is defined just for each integer,
	;; the spectrum function is simpler:
	#'(lambda (wl-nm)
	    (declare (optimize (speed 3) (safety 1)))
	    (multiple-value-bind (quot rem)
		(floor (- (clamp (float wl-nm 1d0) begin-wl-f end-wl-f)
			  begin-wl-f))
	      (lerp rem
		    (aref spectrum-arr quot)
		    (aref spectrum-arr (min (1+ quot) size)))))
	(let* ((band (/ (- end-wl-f begin-wl-f) size))
	       (/band (/ band)))
	  #'(lambda (wl-nm)
	      (declare (optimize (speed 3) (safety 1)))
	      (let* ((wl$ (- (clamp (float wl-nm 1d0) begin-wl-f end-wl-f)
			     begin-wl-f))
		     (frac (mod wl$ band))
		     (coef (* frac /band))
		     (idx (round (* (- wl$ frac) /band))))
		(lerp coef
		      (aref spectrum-arr idx)
		      (aref spectrum-arr (min (+ idx 1) size)))))))))


(defun approximate-spectrum (spectrum &optional (begin-wl 360d0) (end-wl 830d0) (band 1d0))
  "Generates an approximate spectrum of SPECTRUM by pieacewise
linearization. It is used to lighten a \"heavy\" spectrum function."
  (declare (optimize (speed 3) (safety 1))
	   (spectrum-function spectrum))
  (with-double-float (begin-wl end-wl band)
    (let* ((partitions (max 2 (round (/ (- end-wl begin-wl) band))))
	   (partitions-f (float partitions 1d0)))
      (declare (fixnum partitions))
      (gen-spectrum
       (let ((arr (make-array (1+ partitions) :element-type 'double-float)))
	 (loop for i from 0 to partitions
	    for wl = (lerp (/ i partitions-f) begin-wl end-wl)
	    do (setf (aref arr i) (funcall spectrum wl))
	    finally (return arr)))
       begin-wl
       end-wl))))


;;;
;;; Observer
;;;

(defstruct (observer (:constructor %make-observer))
  "OBSERVER is a structure of color matching functions."
  (begin-wl 360 :type (integer 0))
  (end-wl 830 :type (integer 0))
  (cmf-arr nil :type (simple-array double-float (* 3)))
  ;; (linear) interpolation functions of cmf-arr
  (cmf-x nil :type spectrum-function)
  (cmf-y nil :type spectrum-function)
  (cmf-z nil :type spectrum-function)
  (cmf nil :type (function * (values double-float double-float double-float &optional))))


(defun make-observer (cmf-arr &optional (begin-wl 360) (end-wl 830))
  "Generates an observer based on CMF arrays, which must
be (SIMPLE-ARRAY DOUBLE-FLOAT (* 3))."
  (labels ((gen-cmf-1 (arr num &optional (begin-wl 360) (end-wl 830))
	     ;; verbose, almost equivalent to GEN-SPECTRUM
	     (declare ((simple-array double-float (* 3)) arr))
	     (let* ((size (- (array-dimension arr 0) 1))
		    (begin-wl-f (float begin-wl 1d0))
		    (end-wl-f (float end-wl 1d0)))
	       (if (= size (- end-wl begin-wl))
		   #'(lambda (wl)
		       (declare (optimize (speed 3) (safety 1)))
		       (multiple-value-bind (quot rem)
			   (floor (- (clamp (float wl 1d0) begin-wl-f end-wl-f) begin-wl-f))
			 (lerp rem
			       (aref arr quot num)
			       (aref arr (min (1+ quot) size) num))))
		   (let* ((band (/ (- end-wl-f begin-wl-f) size))
			  (/band (/ band)))
		     #'(lambda (wl)
			 (declare (optimize (speed 3) (safety 1)))
			 (let* ((wl$ (- (clamp (float wl 1d0) begin-wl-f end-wl-f) begin-wl-f))
				(frac (mod wl$ band))
				(coef (* frac /band))
				(idx (round (* (- wl$ frac) /band))))
			   (lerp coef
				 (aref arr idx num)
				 (aref arr (min (+ idx 1) size) num))))))))
	   (gen-cmf-3 (arr &optional (begin-wl 360) (end-wl 830))
	     (declare ((simple-array double-float (* 3)) arr))
	     (let* ((size (- (array-dimension arr 0) 1))
		    (begin-wl-f (float begin-wl 1d0))
		    (end-wl-f (float end-wl 1d0)))
	       (if (= size (- end-wl begin-wl))
		   #'(lambda (wl)
		       (declare (optimize (speed 3) (safety 1)))
		       (multiple-value-bind (quot rem)
			   (floor (- (clamp (float wl 1d0) begin-wl-f end-wl-f) begin-wl-f))
			 (values (lerp rem
				       (aref arr quot 0)
				       (aref arr (min (1+ quot) size) 0))
				 (lerp rem
				       (aref arr quot 1)
				       (aref arr (min (1+ quot) size) 1))
				 (lerp rem
				       (aref arr quot 2)
				       (aref arr (min (1+ quot) size) 2)))))
		   (let* ((band (/ (- end-wl-f begin-wl-f) size))
			  (/band (/ band)))
		     #'(lambda (wl)
			 (declare (optimize (speed 3) (safety 1)))
			 (let* ((wl$ (- (clamp (float wl 1d0) begin-wl-f end-wl-f) begin-wl-f))
				(frac (mod wl$ band))
				(coef (* frac /band))
				(idx (round (* (- wl$ frac) /band)))
                                (idx+1 (min (1+ idx) size)))
			   (values (lerp coef
					 (aref arr idx 0)
					 (aref arr idx+1 0))
				   (lerp coef
					 (aref arr idx 1)
					 (aref arr idx+1 1))
				   (lerp coef
					 (aref arr idx 2)
					 (aref arr idx+1 2))))))))))
    (%make-observer
     :begin-wl begin-wl
     :end-wl end-wl
     :cmf-arr cmf-arr
     :cmf-x (gen-cmf-1 cmf-arr 0 begin-wl end-wl)
     :cmf-y (gen-cmf-1 cmf-arr 1 begin-wl end-wl)
     :cmf-z (gen-cmf-1 cmf-arr 2 begin-wl end-wl)
     :cmf (gen-cmf-3 cmf-arr begin-wl end-wl))))

(defparameter +obs-cie1931+ (make-observer cmf-arr-cie1931)
  "CIE 1931 Standard Colorimetric Observer (2-degree).")
(defparameter +obs-cie1964+ (make-observer cmf-arr-cie1964)
  "CIE 1964 Standard Colorimetric Observer (10-degree).")



;; s0, s1, s2 for illuminant series D
;; http://www.rit.edu/cos/colorscience/rc_useful_data.php
(defparameter +s0-arr+
  #.(make-array 54
		:element-type 'double-float
		:initial-contents '(0.04d0 6d0 29.6d0 55.3d0 57.3d0 61.8d0 61.5d0 68.8d0 63.4d0 65.8d0 94.8d0 104.8d0 105.9d0 96.8d0 113.9d0 125.6d0 125.5d0 121.3d0 121.3d0 113.5d0 113.1d0 110.8d0 106.5d0 108.8d0 105.3d0 104.4d0 100d0 96d0 95.1d0 89.1d0 90.5d0 90.3d0 88.4d0 84d0 85.1d0 81.9d0 82.6d0 84.9d0 81.3d0 71.9d0 74.3d0 76.4d0 63.3d0 71.7d0 77d0 65.2d0 47.7d0 68.6d0 65d0 66d0 61d0 53.3d0 58.9d0 61.9d0)))

(defparameter +s1-arr+
  #.(make-array 54
		:element-type 'double-float
		:initial-contents '(0.02d0 4.5d0 22.4d0 42d0 40.6d0 41.6d0 38d0 42.4d0 38.5d0 35d0 43.4d0 46.3d0 43.9d0 37.1d0 36.7d0 35.9d0 32.6d0 27.9d0 24.3d0 20.1d0 16.2d0 13.2d0 8.6d0 6.1d0 4.2d0 1.9d0 0d0 -1.6d0 -3.5d0 -3.5d0 -5.8d0 -7.2d0 -8.6d0 -9.5d0 -10.9d0 -10.7d0 -12d0 -14d0 -13.6d0 -12d0 -13.3d0 -12.9d0 -10.6d0 -11.6d0 -12.2d0 -10.2d0 -7.8d0 -11.2d0 -10.4d0 -10.6d0 -9.7d0 -8.3d0 -9.3d0 -9.8d0)))

(defparameter +s2-arr+
  #.(make-array 54
		:element-type 'double-float
		:initial-contents '(0d0 2d0 4d0 8.5d0 7.8d0 6.7d0 5.3d0 6.1d0 2d0 1.2d0 -1.1d0 -0.5d0 -0.7d0 -1.2d0 -2.6d0 -2.9d0 -2.8d0 -2.6d0 -2.6d0 -1.8d0 -1.5d0 -1.3d0 -1.2d0 -1d0 -0.5d0 -0.3d0 0d0 0.2d0 0.5d0 2.1d0 3.2d0 4.1d0 4.7d0 5.1d0 6.7d0 7.3d0 8.6d0 9.8d0 10.2d0 8.3d0 9.6d0 8.5d0 7d0 7.6d0 8d0 6.7d0 5.2d0 7.4d0 6.8d0 7d0 6.4d0 5.5d0 6.1d0 6.5d0)))

(declaim (type spectrum-function +s0-func+ +s1-func+ +s2-func+))
(defparameter +s0-func+ (gen-spectrum +s0-arr+ 300 830))
(defparameter +s1-func+ (gen-spectrum +s1-arr+ 300 830))
(defparameter +s2-func+ (gen-spectrum +s2-arr+ 300 830))

(defun make-illum-d-spectrum-array (temperature &optional (begin-wl 300) (end-wl 830))
  (declare (optimize (speed 3) (safety 1)))
  (check-type begin-wl fixnum)
  (check-type end-wl fixnum)
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
	   (arr (make-array (1+ (- end-wl begin-wl))
			    :element-type 'double-float
			    :initial-element 0d0)))
      (loop for wl from begin-wl to end-wl do
	   (setf (aref arr (- wl begin-wl))
		 (+ (funcall +s0-func+ wl)
		    (* m1 (funcall +s1-func+ wl))
		    (* m2 (funcall +s2-func+ wl)))))
      arr)))

(defun gen-illum-d-spectrum (temperature)
  "Generates the spectrum of the illuminant series D for a given
temperature."
  (gen-spectrum (make-illum-d-spectrum-array temperature 300 830)
		300 830))


(defun spectrum-sum (spectrum &optional (begin-wl 300) (end-wl 830) (band 1))
  (loop for wl from begin-wl to end-wl by band
     sum (funcall spectrum wl)))

(declaim (inline bb-spectrum))
(defun bb-spectrum (wavelength-nm &optional (temperature 5000d0))
  "Spectrum function of a blackbody, which is not normalized."
  (declare (optimize (speed 3) (safety 1)))
  (let ((wlm (* (float wavelength-nm 1d0) 1d-9)))
    (check-type wlm (double-float 0d0))
    (/ (* 3.74183d-16 (expt wlm -5d0))
       (- (exp (/ 1.4388d-2 (* wlm (float temperature 1d0)))) 1d0))))

(declaim (inline optimal-spectrum))
(defun optimal-spectrum (wavelength-nm &optional (wl1 300d0) (wl2 830d0))
  "Spectrum function of optimal colors:
In the case wl1 <= wl2:
f(x) = 1d0 if wl1 <= x <= wl2,
f(x) = 0d0 otherwise.
In the case wl1 > wl2:
f(x) = 1d0 if x <=wl2 or wl1 <= x,
f(x) = 0d0 otherwise.
"
  (declare (optimize (speed 3) (safety 1)))
  (let ((wl (float wavelength-nm 1d0))
	(wl1 (float wl1 1d0))
	(wl2 (float wl2 1d0)))
    (if (<= wl1 wl2)
	(if (<= wl1 wl wl2) 1d0 0d0)
	(if (or (<= wl wl2) (<= wl1 wl)) 1d0 0d0))))

(defun flat-spectrum (wavelength-nm)
  "(constantly 1d0)"
  (declare (optimize (speed 3) (safety 1)))
  (declare (ignore wavelength-nm))
  1d0)





;;; Standard Illuminant, XYZ, xyY
;;; The nominal range of Y is always [0, 1].

(defstruct (illuminant (:constructor %make-illuminant)
		       (:copier nil))
  (small-x 0.0 :type double-float)
  (small-y 0.0 :type double-float)
  (x 0.0 :type double-float)
  (y 0.0 :type double-float)
  (z 0.0 :type double-float)
  (spectrum #'empty-function :type spectrum-function)
  (observer +obs-cie1931+ :type observer)
  ;; used for xyz-to-spectrum conversion
  (to-spectrum-matrix +empty-matrix+ :type (simple-array double-float (3 3))))

(declaim (inline illuminant-no-spd-p))
(defun illuminant-no-spd-p (illuminant)
  (eq #'empty-function (illuminant-spectrum illuminant)))

(define-condition no-spd-error (simple-error)
  ((illuminant :initarg :illuminant
	       :initform nil
	       :accessor cond-illuminant))
  (:report (lambda (condition stream)
             (let ((*print-array* nil))
               (format stream "The illuminant has no spectrum: ~A"
                       (cond-illuminant condition))))))

(defvar +illum-e+) ; defined later
(defvar +illum-c+)

(defun spectrum-to-xyz (spectrum &optional (illuminant +illum-e+) (begin-wl 360) (end-wl 830) (band 1))
  (declare (optimize (speed 3) (safety 1)))
  "Computes XYZ values from SPECTRUM in reflective and transmissive
case. The function SPECTRUM must be defined at least in [BEGIN-WL, END-WL]; the SPECTRUM is called for BEGIN-WL, BEGIN-WL + BAND, BEGIN-WL + 2*BAND, ..., END-WL."
  (if (illuminant-no-spd-p illuminant)
      (error (make-condition 'no-spd-error :illuminant illuminant))
      (%spectrum-to-xyz spectrum
                        (illuminant-spectrum illuminant)
                        (illuminant-observer illuminant)
                        begin-wl
                        end-wl
                        band)))

(defun %spectrum-to-xyz (spectrum illum-spectrum observer &optional (begin-wl 360) (end-wl 830) (band 1))
  (declare (optimize (speed 3) (safety 1))
	   (spectrum-function spectrum illum-spectrum))
  (let ((x 0d0) (y 0d0) (z 0d0) (max-y 0d0)
	(cmf (observer-cmf observer)))
    (declare (double-float x y z max-y))
    (loop for wl from begin-wl to end-wl by band do
	 (let ((p (funcall illum-spectrum wl))
	       (reflec (funcall spectrum wl)))
	   (multiple-value-bind (x-fac y-fac z-fac) (funcall cmf wl)
	     (incf x (* x-fac p reflec))
	     (incf y (* y-fac p reflec))
	     (incf z (* z-fac p reflec))
	     (incf max-y (* y-fac p)))))
    (let ((factor (/ max-y)))
      (values (* x factor) (* y factor) (* z factor)))))


(defun bench-spectrum (&optional (num 50000) (illuminant +illum-c+))
  (declare (optimize (speed 3) (safety 1))
	   (fixnum num))
  (time-after-gc
    (let ((spctrm (gen-illum-d-spectrum 4000)))
      (dotimes (idx num)
        (spectrum-to-xyz spctrm illuminant)))))


(let ((mat (make-array '(3 3)
		       :element-type 'double-float
		       :initial-element 0d0)))
  (defun calc-to-spectrum-matrix (illum-spectrum observer)
    "Used for XYZ-to-spectrum conversion."
    (declare (optimize (speed 3) (safety 1)))
    (multiple-value-bind (a00 a10 a20)
	(%spectrum-to-xyz (observer-cmf-x observer) illum-spectrum observer)
      (multiple-value-bind (a01 a11 a21)
	  (%spectrum-to-xyz (observer-cmf-y observer) illum-spectrum observer)
	(multiple-value-bind (a02 a12 a22)
	    (%spectrum-to-xyz (observer-cmf-z observer) illum-spectrum observer)
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
  (if (illuminant-no-spd-p illuminant)
      (error (make-condition 'no-spd-error :illuminant illuminant))
      (let ((observer (illuminant-observer illuminant)))
	(multiple-value-bind (fac-x fac-y fac-z)
	    (multiply-mat-vec (illuminant-to-spectrum-matrix illuminant) x y z)
	  #'(lambda (wl)
	      (+ (* fac-x (funcall (observer-cmf-x observer) wl))
		 (* fac-y (funcall (observer-cmf-y observer) wl))
		 (* fac-z (funcall (observer-cmf-z observer) wl))))))))
    
	    
(defun make-illuminant (small-x small-y &optional (spectrum nil) (observer +obs-cie1931+))
  "Generates an illuminant based on a white point. No error occurs,
even if the given white point, (small-x, small-y), and SPD contradicts
to each other."
  (multiple-value-bind (x y z) (xyy-to-xyz small-x small-y 1d0)
    (%make-illuminant :small-x (float small-x 1d0)
		      :small-y (float small-y 1d0)
		      :x (float x 1d0)
		      :y (float y 1d0)
		      :z (float z 1d0)
		      :spectrum (or spectrum #'empty-function)
		      :observer observer
		      :to-spectrum-matrix (if spectrum
					      (calc-to-spectrum-matrix spectrum
                                                                       observer)
					      +empty-matrix+))))

(defun make-illuminant-by-spd (spectrum &optional (observer +obs-cie1931+))
  "Generates an illuminant based on a spectral power distribution. The
white point is automatically calculated."
  (multiple-value-bind (x y z)
      (%spectrum-to-xyz #'flat-spectrum spectrum observer)
    (multiple-value-bind (small-x small-y disused)
	(xyz-to-xyy x y z)
      (declare (ignore disused))
      (%make-illuminant :small-x (float small-x 1d0)
			:small-y (float small-y 1d0)
			:x (float x 1d0)
			:y (float y 1d0)
			:z (float z 1d0)
			:spectrum (or spectrum #'flat-spectrum)
			:observer observer
			:to-spectrum-matrix (calc-to-spectrum-matrix spectrum observer)))))

(defparameter +illum-a+
  (make-illuminant 0.44757d0 0.40745d0
		   #'(lambda (wl)
		       (declare (optimize (speed 3) (safety 1)))
		       (let ((wl (float wl 1d0)))
			 (check-type wl (double-float 0d0))
			 (* 100d0
			    (expt (/ 560d0 wl) 5)
			    (/ #.(- (exp (/ 1.435d7 (* 2848 560))) 1d0)
			       (- (exp (/ 1.435d7 (* 2848d0 wl))) 1d0)))))))
			  
(defparameter +illum-b+ (make-illuminant 0.34842d0 0.35161d0)) ; no spd

(defparameter +illum-c-arr+
  #.(make-array 107
		:element-type 'double-float
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

