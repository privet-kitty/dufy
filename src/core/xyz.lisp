;;;
;;; Spectrum, Illuminant, XYZ, xyY
;;;

(in-package :dufy-core)


(deftype spectrum-function () '(function * (values double-float &optional)))

(define-colorspace xyz (x y z)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Y is normalized: i.e. the nominal range of Y is [0, 1]")
(define-colorspace xyy (small-x small-y y)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Y is normalized: i.e. the nominal range of Y is [0, 1]")
(define-colorspace spectrum (spectrum)
  :arg-types (spectrum-function)
  :return-types (spectrum-function)
  :documentation "A spectrum is just a function which receives a
wavelength (nm) as a real number and returns a double-float: (function
* (values double-float &optional))")

(define-primary-converter (xyy xyz) (small-x small-y y)
  (declare (optimize (speed 3) (safety 1)))
  "xyY to XYZ. The nominal range of Y is [0, 1], though all real
values are accepted."
  (with-ensuring-type double-float (small-x small-y y)
    (if (zerop small-y)
	(values 0d0 y 0d0)
	(values (/ (* small-x y) small-y) 
		y
		(/ (* (- 1d0 small-x small-y) y) small-y)))))

(define-primary-converter (xyz xyy) (x y z)
  (declare (optimize (speed 3) (safety 1)))
  "XYZ to xyY. The nominal range of Y is [0, 1], though all real
values are accepted."
  (with-ensuring-type double-float (x y z)
    (let ((sum (+ x y z)))
      (if (= sum 0)
	  (values 0d0 0d0 y)
	  (values (/ x sum) (/ y sum) y)))))

(defun gen-spectrum (spectrum-seq &optional (begin-wl 360) (end-wl 830))
  "GEN-SPECTRUM returns a spectral power distribution
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
	;; the spectrum function is simpler and more efficient:
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


(defun approximate-spectrum (spectrum &key (begin-wl 360d0) (end-wl 830d0) (band 1d0))
  "Generates an approximate spectrum of SPECTRUM by pieacewise
linearization. It is used to lighten a \"heavy\" spectrum function."
  (declare (optimize (speed 3) (safety 1))
	   (spectrum-function spectrum))
  (with-ensuring-type double-float (begin-wl end-wl band)
    (let* ((partitions (max 2 (round (/ (- end-wl begin-wl) band))))
	   (partitions-f (float partitions 1d0))
           (points (make-array (1+ partitions) :element-type 'double-float)))
      (declare (fixnum partitions))
      (gen-spectrum (loop for i from 0 to partitions
                          for wl = (lerp (/ i partitions-f)
                                         begin-wl
                                         end-wl)
                          do (setf (aref points i) (funcall spectrum wl))
                          finally (return points))
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

(defmethod print-object ((obs observer) stream)
  (let ((*print-array* nil))
    (call-next-method)))

(defun make-observer (cmf-arr &optional (begin-wl 360) (end-wl 830))
  "Generates an observer object based on CMF arrays, which must
be (SIMPLE-ARRAY DOUBLE-FLOAT (* 3)). The response out of the interval
[begin-wl, end-wl] is regarded as 0."
  (let ((begin-wl-f (float begin-wl 1d0))
        (end-wl-f (float end-wl 1d0)))
    (labels ((gen-cmf-1 (arr num &optional (begin-wl 360) (end-wl 830))
               ;; fix me
               ;; verbose, almost equivalent to GEN-SPECTRUM
               (declare ((simple-array double-float (* 3)) arr))
               (let ((size (- (array-dimension arr 0) 1)))
                 (if (= size (- end-wl begin-wl))
                     #'(lambda (wl)
                         (declare (optimize (speed 3) (safety 1)))
                         (let ((wl (float wl 1d0)))
                           (if (or (< wl begin-wl-f) (< end-wl-f wl))
                               0d0
                               (multiple-value-bind (quot rem)
                                   (floor (- wl begin-wl-f))
                                 (lerp rem
                                       (aref arr quot num)
                                       (aref arr (min (1+ quot) size) num))))))
                     (let* ((band (/ (- end-wl-f begin-wl-f) size))
                            (/band (/ band)))
                       #'(lambda (wl)
                           (declare (optimize (speed 3) (safety 1)))
                           (let ((wl (float wl 1d0)))
                             (if (or (< wl begin-wl-f) (< end-wl-f wl))
                                 0d0
                                 (let* ((wl$ (- wl begin-wl-f))
                                        (frac (mod wl$ band))
                                        (coef (* frac /band))
                                        (idx (round (* (- wl$ frac) /band))))
                                   (lerp coef
                                         (aref arr idx num)
                                         (aref arr (min (+ idx 1) size) num))))))))))
             (gen-cmf-3 (arr &optional (begin-wl 360) (end-wl 830))
               (declare ((simple-array double-float (* 3)) arr))
               (let ((size (- (array-dimension arr 0) 1)))
                 (if (= size (- end-wl begin-wl))
                     #'(lambda (wl)
                         (declare (optimize (speed 3) (safety 1)))
                         (let ((wl (float wl 1d0)))
                           (if (or (< wl begin-wl-f) (< end-wl-f wl))
                               (values 0d0 0d0 0d0)
                               (multiple-value-bind (quot rem)
                                   (floor (- wl begin-wl-f))
                                 (values (lerp rem
                                               (aref arr quot 0)
                                               (aref arr (min (1+ quot) size) 0))
                                         (lerp rem
                                               (aref arr quot 1)
                                               (aref arr (min (1+ quot) size) 1))
                                         (lerp rem
                                               (aref arr quot 2)
                                               (aref arr (min (1+ quot) size) 2)))))))
                     (let* ((band (/ (- end-wl-f begin-wl-f) size))
                            (/band (/ band)))
                       #'(lambda (wl)
                           (declare (optimize (speed 3) (safety 1)))
                           (let ((wl (float wl 1d0)))
                             (if (or (< wl begin-wl-f) (< end-wl-f wl))
                                 (values 0d0 0d0 0d0)
                                 (let* ((wl$ (- wl begin-wl-f))
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
                                                 (aref arr idx+1 2))))))))))))
      (%make-observer
       :begin-wl begin-wl
       :end-wl end-wl
       :cmf-arr cmf-arr
       :cmf-x (gen-cmf-1 cmf-arr 0 begin-wl end-wl)
       :cmf-y (gen-cmf-1 cmf-arr 1 begin-wl end-wl)
       :cmf-z (gen-cmf-1 cmf-arr 2 begin-wl end-wl)
       :cmf (gen-cmf-3 cmf-arr begin-wl end-wl)))))

(defparameter +obs-cie1931+ (make-observer cmf-arr-cie1931)
  "CIE 1931 Standard Colorimetric Observer (2-degree).")
(defparameter +obs-cie1964+ (make-observer cmf-arr-cie1964)
  "CIE 1964 Standard Colorimetric Observer (10-degree).")



;; s0, s1, s2 for illuminant series D
;; http://www.rit.edu/cos/colorscience/rc_useful_data.php
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +s0-arr+
    (make-array 54
                :element-type 'double-float
                :initial-contents '(0.04d0 6d0 29.6d0 55.3d0 57.3d0 61.8d0 61.5d0 68.8d0 63.4d0 65.8d0 94.8d0 104.8d0 105.9d0 96.8d0 113.9d0 125.6d0 125.5d0 121.3d0 121.3d0 113.5d0 113.1d0 110.8d0 106.5d0 108.8d0 105.3d0 104.4d0 100d0 96d0 95.1d0 89.1d0 90.5d0 90.3d0 88.4d0 84d0 85.1d0 81.9d0 82.6d0 84.9d0 81.3d0 71.9d0 74.3d0 76.4d0 63.3d0 71.7d0 77d0 65.2d0 47.7d0 68.6d0 65d0 66d0 61d0 53.3d0 58.9d0 61.9d0)))

  (defparameter +s1-arr+
    (make-array 54
                :element-type 'double-float
                :initial-contents '(0.02d0 4.5d0 22.4d0 42d0 40.6d0 41.6d0 38d0 42.4d0 38.5d0 35d0 43.4d0 46.3d0 43.9d0 37.1d0 36.7d0 35.9d0 32.6d0 27.9d0 24.3d0 20.1d0 16.2d0 13.2d0 8.6d0 6.1d0 4.2d0 1.9d0 0d0 -1.6d0 -3.5d0 -3.5d0 -5.8d0 -7.2d0 -8.6d0 -9.5d0 -10.9d0 -10.7d0 -12d0 -14d0 -13.6d0 -12d0 -13.3d0 -12.9d0 -10.6d0 -11.6d0 -12.2d0 -10.2d0 -7.8d0 -11.2d0 -10.4d0 -10.6d0 -9.7d0 -8.3d0 -9.3d0 -9.8d0)))

  (defparameter +s2-arr+
    (make-array 54
                :element-type 'double-float
                :initial-contents '(0d0 2d0 4d0 8.5d0 7.8d0 6.7d0 5.3d0 6.1d0 2d0 1.2d0 -1.1d0 -0.5d0 -0.7d0 -1.2d0 -2.6d0 -2.9d0 -2.8d0 -2.6d0 -2.6d0 -1.8d0 -1.5d0 -1.3d0 -1.2d0 -1d0 -0.5d0 -0.3d0 0d0 0.2d0 0.5d0 2.1d0 3.2d0 4.1d0 4.7d0 5.1d0 6.7d0 7.3d0 8.6d0 9.8d0 10.2d0 8.3d0 9.6d0 8.5d0 7d0 7.6d0 8d0 6.7d0 5.2d0 7.4d0 6.8d0 7d0 6.4d0 5.5d0 6.1d0 6.5d0))))


(defun make-illum-d-spectrum-array (temperature &optional (begin-wl 300) (end-wl 830))
  (declare (optimize (speed 3) (safety 1)))
  (check-type begin-wl fixnum)
  (check-type end-wl fixnum)
  (let ((s0-func (load-time-value (gen-spectrum +s0-arr+ 300 830) t))
        (s1-func (load-time-value (gen-spectrum +s1-arr+ 300 830) t))
        (s2-func (load-time-value (gen-spectrum +s2-arr+ 300 830) t)))
    (declare (type spectrum-function s0-func s1-func s2-func))
    (labels ((calc-xd (temp)
               (let ((/temp (/ temp)))
                 (if (<= temp 7000d0)
                     (+ 0.244063d0 (* /temp (+ 0.09911d3 (* /temp (+ 2.9678d6 (* /temp -4.607d9))))))
                     (+ 0.237040d0 (* /temp (+ 0.24748d3 (* /temp (+ 1.9018d6 (* /temp -2.0064d9)))))))))
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
        (loop for wl from begin-wl to end-wl
              do (setf (aref arr (- wl begin-wl))
                       (+ (funcall s0-func wl)
                          (* m1 (funcall s1-func wl))
                          (* m2 (funcall s2-func wl)))))
        arr))))

(defun gen-illum-d-spectrum (temperature &key rectify)
  "Generates the spectrum of the illuminant series D for a given
temperature (from 300 nm to 830 nm with band-width 1 nm). If RECTIFY
is t, the temperature multiplied by (/ 1.43880 1.438) is used
instead. (roughly 6504K for 6500K, etc.)"
  (let ((temp (if rectify
                  (* temperature #.(/ 1.43880d0 1.438d0))
                  temperature)))
    (gen-spectrum (make-illum-d-spectrum-array temp 300 830)
                  300 830)))


(declaim (inline bb-spectrum))
(defun bb-spectrum (wavelength-nm &optional (temperature 5000d0))
  "Spectrum function of a blackbody, which is not normalized."
  (declare (optimize (speed 3) (safety 1)))
  (let ((wlm (* (float wavelength-nm 1d0) 1d-9)))
    (check-type wlm (double-float 0d0))
    (/ (* 3.74183d-16 (expt wlm -5d0))
       (- (exp (/ 1.4388d-2 (* wlm (float temperature 1d0)))) 1d0))))

(declaim (inline optimal-spectrum1 optimal-spectrum2))
(defun optimal-spectrum1 (wavelength-nm &optional (wl1 300d0) (wl2 830d0))
  "Spectrum function of optimal colors:
f(x) = 1d0 if wl1 <= x <= wl2,
f(x) = 0d0 otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (if (<= wl1 wavelength-nm wl2) 1d0 0d0))

(defun optimal-spectrum2 (wavelength-nm &optional (wl1 300d0) (wl2 830d0))
  "Spectrum function of optimal colors:
f(x) = 1d0 if x <=wl2 or wl1 <= x,
f(x) = 0d0 otherwise."
  (declare (optimize (speed 3) (safety 1)))
  (if (or (<= wavelength-nm wl1)
          (<= wl2 wavelength-nm))
      1d0 0d0))

(defun flat-spectrum (wavelength-nm)
  "(constantly 1d0)"
  (declare (optimize (speed 3) (safety 0)))
  (declare (ignore wavelength-nm))
  1d0)

(defun empty-spectrum ()
  "Used internally instead of NIL."
  0d0)



;;;
;;; Illuminant, White Point
;;;

(defstruct (illuminant (:constructor %make-illuminant)
                       (:copier nil))
  (x 1d0 :type double-float)
  (z 1d0 :type double-float)
  (spectrum #'empty-spectrum :type spectrum-function)
  (observer +obs-cie1931+ :type observer)
  ;; used in xyz-to-spectrum conversion
  (to-spectrum-matrix +empty-matrix+ :type (simple-array double-float (3 3))))

(defun illuminant-xy (illuminant)
  "Returns the xy chromacity coordinates of a given illuminant."
  (declare (optimize (speed 3) (safety 1))
           (type illuminant illuminant))
  (let* ((x (illuminant-x illuminant))
         (z (illuminant-z illuminant))
         (sum (+ x 1d0 z)))
      (if (= sum 0)
          (values 0d0 0d0)
          (values (/ x sum) (/ sum)))))

(declaim (inline illuminant-no-spd-p))
(defun illuminant-no-spd-p (illuminant)
  (eq #'empty-spectrum (illuminant-spectrum illuminant)))

(define-condition no-spd-error (simple-error)
  ((illuminant :initarg :illuminant
	       :initform nil
	       :accessor cond-illuminant))
  (:report (lambda (condition stream)
             (let ((*print-array* nil))
               (format stream "The illuminant has no spectrum: ~A"
                       (cond-illuminant condition))))))

(defvar +illum-d65+) ; defined later
(defvar +illum-c+)

(defun spectrum-to-xyz-primitive (spectrum illuminant-spd observer &optional (begin-wl 360) (end-wl 830) (band 1))
  "SPECTRUM: spectral reflectance (or transmittance)
ILLUMINANT-SPD: SPD of illuminant"
  (declare (optimize (speed 3) (safety 1))
	   (spectrum-function spectrum illuminant-spd))
  (let ((x 0d0) (y 0d0) (z 0d0) (max-y 0d0)
	(cmf (observer-cmf observer)))
    (declare (double-float x y z max-y))
    (loop for wl from begin-wl to end-wl by band
          do (let* ((p (funcall illuminant-spd wl))
                    (reflec (funcall spectrum wl))
                    (factor (* p reflec)))
               (multiple-value-bind (x-match y-match z-match) (funcall cmf wl)
                 (incf x (* x-match factor))
                 (incf y (* y-match factor))
                 (incf z (* z-match factor))
                 (incf max-y (* y-match p)))))
    (let ((normalizing-factor (/ max-y)))
      (values (* x normalizing-factor)
              (* y normalizing-factor)
              (* z normalizing-factor)))))

(define-primary-converter (spectrum xyz) (spectrum &key (illuminant +illum-d65+) (begin-wl 360) (end-wl 830) (band 1))
  (declare (optimize (speed 3) (safety 1)))
  "Computes the XYZ values of SPECTRUM in reflective or transmissive
case. The function SPECTRUM, a spectral reflectance, must be defined
at least in [BEGIN-WL, END-WL]; the SPECTRUM is called for BEGIN-WL,
BEGIN-WL + BAND, BEGIN-WL + 2*BAND, ..., BEGIN-WL + n*BAND (<= END-WL)."
  (if (illuminant-no-spd-p illuminant)
      (error (make-condition 'no-spd-error :illuminant illuminant))
      (spectrum-to-xyz-primitive spectrum
                                 (illuminant-spectrum illuminant)
                                 (illuminant-observer illuminant)
                                 begin-wl
                                 end-wl
                                 band)))



(defun bench-spectrum (&optional (num 50000) (illuminant +illum-c+))
  "For devel."
  (declare (optimize (speed 3) (safety 1))
	   (fixnum num))
  (time-after-gc
    (let ((spctrm (gen-illum-d-spectrum 4000)))
      (dotimes (idx num)
        (spectrum-to-xyz spctrm :illuminant illuminant)))))


(defun calc-to-spectrum-matrix (illuminant-spd observer &optional (begin-wl 360) (end-wl 830) (band 1))
  "Used for XYZ-to-spectrum conversion."
  (declare (optimize (speed 3) (safety 1)))
  (let ((mat (load-time-value
              (make-array '(3 3) :element-type 'double-float
                                 :initial-element 0d0))))
    (multiple-value-bind (a00 a10 a20)
        (spectrum-to-xyz-primitive (observer-cmf-x observer) illuminant-spd observer begin-wl end-wl band)
      (multiple-value-bind (a01 a11 a21)
          (spectrum-to-xyz-primitive (observer-cmf-y observer) illuminant-spd observer begin-wl end-wl band)
        (multiple-value-bind (a02 a12 a22)
            (spectrum-to-xyz-primitive (observer-cmf-z observer) illuminant-spd observer begin-wl end-wl band)
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

(define-primary-converter (xyz spectrum) (x y z &key (illuminant +illum-d65+))
  "Converts XYZ to spectrum, which is, of course, a spectrum among
many and may contain a negative spectral density."
  (if (illuminant-no-spd-p illuminant)
      (error (make-condition 'no-spd-error :illuminant illuminant))
      (let ((observer (illuminant-observer illuminant)))
        (multiple-value-bind (fac-x fac-y fac-z)
            (multiply-mat-vec (illuminant-to-spectrum-matrix illuminant) x y z)
          #'(lambda (wl)
              (declare (optimize (speed 3) (safety 1)))
              (+ (* fac-x (funcall (observer-cmf-x observer) wl))
                 (* fac-y (funcall (observer-cmf-y observer) wl))
                 (* fac-z (funcall (observer-cmf-z observer) wl))))))))
    
	    
(defun make-illuminant (&key x z spectrum (observer +obs-cie1931+) (compile-time nil) (begin-wl 360) (end-wl 830) (band 1))
  "Generates an illuminant. If the SPECTRUM is nil, the returned
illuminant contains only a white point. Although the white point (X,
1d0, Z) is automatically calculated if X and Z are nil, you can
designate X and Z explicitly. Note that no error occurs, even if the
given white point and SPD contradicts to each other.

 (make-illuminant :x 1.0 :z 1.0)
;; => illuminant without SPD

 (make-illuminant :x 1.0 :z 1.0 :spectrum #'flat-spectrum)
 (make-illuminant :spectrum #'flat-spectrum)
;; => illuminant with SPD

 (make-illuminant :x 0.9 :z 1.1 :spectrum #'flat-spectrum)
;; => illuminant with SPD (valid but meaningless)

If X and Y are NIL and COMPILE-TIME is T, the white point is
calculated at compile time. (Avoid side effects in this case.)"
  (declare (ignore compile-time))
  (macrolet
      ((make (x z)
         `(%make-illuminant
           :x (float ,x 1d0)
           :z (float ,z 1d0)
           :spectrum (or spectrum #'empty-spectrum)
           :observer observer
           :to-spectrum-matrix (if spectrum
                                   (calc-to-spectrum-matrix spectrum observer begin-wl end-wl band)
                                   +empty-matrix+))))
    (if (and (null x) (null z))
        (multiple-value-bind (x y z)
            (spectrum-to-xyz-primitive #'flat-spectrum spectrum observer begin-wl end-wl band)
          (declare (ignore y))
          (make x z))
        (make x z))))

(define-compiler-macro make-illuminant (&whole form &key x z spectrum (observer '+obs-cie1931+) (begin-wl 360) (end-wl 830) (band 1) (compile-time nil))
  (if (and compile-time (null x) (null z))
      (let ((spctrm (eval spectrum))
            (obs (eval observer))
            (bwl (eval begin-wl))
            (ewl (eval end-wl))
            (bnd (eval band)))
        (multiple-value-bind (x y z)
            (spectrum-to-xyz-primitive #'flat-spectrum spctrm obs bwl ewl bnd)
          (declare (ignore y))
          `(%make-illuminant
            :x (float ,x 1d0)
            :z (float ,z 1d0)
            :spectrum ,spectrum
            :observer ,observer
            :to-spectrum-matrix ,(calc-to-spectrum-matrix spctrm obs bwl ewl bnd))))
      form))

