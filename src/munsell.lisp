(in-package :dufy)

;; The bradford transformations between D65 and C are frequently used here.
(declaim (type (function * (values double-float double-float double-float)) d65-to-c c-to-d65))
(defparameter d65-to-c
  (load-time-value (gen-cat-function +illum-d65+ +illum-c+) t))
(defparameter c-to-d65
  (load-time-value (gen-cat-function +illum-c+ +illum-d65+) t))

(eval-when (:compile-toplevel)
  (defparameter *bit-positive-fixnum* #.(floor (log most-positive-fixnum 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (double-float *maximum-chroma*))
  (defparameter *maximum-chroma*
    #+(and sbcl 64-bit) #.(float (expt 2 (- *bit-positive-fixnum* 10)) 1d0)
    #-(or sbcl 64-bit)  most-positive-double-float
    "The largest chroma which the converters accepts. It is less than
    MOST-POSITIVE-DOUBLE-FLOAT because of efficiency: e.g. in
    SBCL (64-bit) it is desirable that a float F
    fulfills (typep (round F) '(SIGNED-BYTE 64)"))


(declaim (ftype (function * (integer 0 50)) max-chroma))
(defun max-chroma (hue40 value &key (use-dark t))
  "Returns the largest chroma in the Munsell renotation data."
  (declare (optimize (speed 3) (safety 1)))
  (let ((hue40 (float hue40 1d0))
	(value (float value 1d0)))
    (let* ((hue (mod hue40 40d0))
	   (hue1 (floor hue))
	   (hue2 (mod (ceiling hue) 40)))
      (if (or (>= value 1)
	      (not use-dark))
	  (let ((val1 (floor value))
		(val2 (ceiling value)))
	    (min (aref max-chroma-arr hue1 val1)
		 (aref max-chroma-arr hue1 val2)
		 (aref max-chroma-arr hue2 val1)
		 (aref max-chroma-arr hue2 val2)))
	  (let* ((dark-value (* value 5d0))
		 (dark-val1 (floor dark-value))
		 (dark-val2 (ceiling dark-value)))
	    (min (aref max-chroma-arr-dark hue1 dark-val1)
		 (aref max-chroma-arr-dark hue1 dark-val2)
		 (aref max-chroma-arr-dark hue2 dark-val1)
		 (aref max-chroma-arr-dark hue2 dark-val2)))))))

(declaim (inline munsel-value-to-y
		 munsell-value-to-lstar))
(declaim (ftype (function * double-float) munsell-value-to-y))
(defun munsell-value-to-y (v)
  "Converts Munsell value to Y, whose nominal range is [0, 1]. The
formula is based on ASTM D1535-08e1:"
  (declare (optimize (speed 3) (safety 1)))
  (let ((v (float v 1d0)))
    (* v (+ 1.1914d0 (* v (+ -0.22533d0 (* v (+ 0.23352d0 (* v (+ -0.020484d0 (* v 0.00081939d0)))))))) 0.01d0)))

(defun munsell-value-to-lstar (v)
  "Converts Munsell value to L*, whose nominal range is [0, 100]."
  (- (* 116d0 (function-f (munsell-value-to-y v))) 16d0))

  
;; (defun munsell-value-to-achromatic-qrgb (v)
;;   "Returns the gray corresponding to given Munsell value."
;;   (let ((x (round (* (delinearize (munsell-value-to-y v)) 255))))
;;     (values x x x)))

(defun munsell-value-to-achromatic-xyy (v)
  "Illuminant C."
  (values 0.31006d0 0.31616d0 (munsell-value-to-y v)))

;; (defun munsell-value-to-achromatic-lchab (v)
;;   "Illuminant C"
;;   (apply #'lab-to-lchab
;; 	 (apply (rcurry #'xyz-to-lab +illum-c+)
;; 		(xyy-to-xyz 0.31006d0
;; 			    0.31616d0
;; 			    (munsell-value-to-y v)))))


(defun munsell-value-to-achromatic-xyy-from-mrd (v)
  "For devel. Another version of munsell-value-to-achromatic-xyy based
on the Munsell renotation data. V must be integer. It is nearly equal
to munsell-value-to-achromatic-xyy"
  (values 0.31006d0 0.31616d0
	  (clamp (* (aref (vector 0d0 0.0121d0 0.03126d0 0.0655d0 0.120d0 0.1977d0 0.3003d0 0.4306d0 0.591d0 0.7866d0 1.0257d0) v)
		    0.975d0)
		 0d0 1d0)))

(declaim (inline y-to-munsell-value))
(defun y-to-munsell-value (y)
  "Interpolates the inversion table of MUNSELL-VALUE-TO-Y linearly,
whose band width is 10^-3. The
error, (abs (- (y (munsell-value-to-y (y-to-munsell-value y))))), is
smaller than 10^-5."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((y1000 (* (clamp (float y 1d0) 0d0 1d0) 1000))
	 (y1 (floor y1000))
	 (y2 (ceiling y1000)))
    (if (= y1 y2)
	(aref y-to-munsell-value-arr y1)
	(let ((r (- y1000 y1)))
	  (+ (* (- 1 r) (aref y-to-munsell-value-arr y1))
	     (* r (aref y-to-munsell-value-arr y2)))))))

(defun test-value-to-y (&optional (num 100000000))
  "Checks error of y-to-munsell-value"
  (declare (optimize (speed 3) (safety 1))
	   (fixnum num))
  (let ((max-error 0d0)
	(worst-y 0d0))
    (dotimes (n num (values max-error worst-y))
      (let* ((y (random 1d0))
	     (delta (abs (- (munsell-value-to-y (y-to-munsell-value y)) y))))
	(when (>= delta max-error)
	  ;;(format t "y=~A delta=~A~%" y delta)
	  (setf worst-y y
		max-error delta))))))

(defun qrgb-to-munsell-value (r g b &optional (rgbspace +srgb+))
  (y-to-munsell-value (nth-value 1 (qrgb-to-xyz r g b rgbspace))))


(declaim (ftype (function * (values (double-float 0d0 360d0) double-float double-float))
		mhvc-to-lchab-simplest-case
		mhvc-to-lchab-value-chroma-integer-case
		mhvc-to-lchab-value-integer-case
		mhvc-to-lchab-general-case
		mhvc-to-lchab))

(defun mhvc-to-lchab-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
  "There are no type checks: e.g. HUE40 must be in {0, ...., 39}."
  (declare (optimize (speed 3) (safety 0))
	   (fixnum hue40 tmp-value half-chroma))
  (let ((arr (if dark
		 mrd-array-lchab-dark
		 mrd-array-lchab)))
    (if (<= half-chroma 25)
	(values (aref arr hue40 tmp-value half-chroma 0)
		(aref arr hue40 tmp-value half-chroma 1)
		(aref arr hue40 tmp-value half-chroma 2))
	;; in the case chroma > 50
	(let ((cstarab (aref arr hue40 tmp-value 25 1))
	      (factor (* half-chroma #.(float 1/25 1d0))))
	  (values (aref arr hue40 tmp-value 25 0)
		  (* cstarab factor)
		  (aref arr hue40 tmp-value 25 2))))))

(defun mhvc-to-lchab-value-chroma-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
	   ((double-float 0d0 40d0) hue40)
	   (fixnum tmp-value half-chroma))
  (let* ((hue1 (floor hue40))
	 (hue2 (mod (ceiling hue40) 40)))
    (if (= hue1 hue2)
	(mhvc-to-lchab-simplest-case (round hue40) tmp-value half-chroma dark)
	(multiple-value-bind (lstar cstarab1 hab1)
	    (mhvc-to-lchab-simplest-case hue1 tmp-value half-chroma dark)
	  (multiple-value-bind (disused cstarab2 hab2)
	      (mhvc-to-lchab-simplest-case hue2 tmp-value half-chroma dark)
	    (declare (ignore disused)
		     ((double-float 0d0 360d0) hab1 hab2))
	    (if (= hab1 hab2)
		(values lstar cstarab1 hab1)
		(let* ((hab (the (double-float 0d0 360d0)
				 (circular-lerp (- hue40 hue1) hab1 hab2 360d0)))
		       (cstarab (+ (* cstarab1 (/ (subtract-with-mod hab2 hab 360d0)
						  (subtract-with-mod hab2 hab1 360d0)))
				   (* cstarab2 (/ (subtract-with-mod hab hab1 360d0)
						  (subtract-with-mod hab2 hab1 360d0))))))
		  (values lstar cstarab hab))))))))


(defun mhvc-to-lchab-value-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
	   ((double-float 0d0 40d0) hue40)
	   ((double-float 0d0 #.*maximum-chroma*) half-chroma)
	   (fixnum tmp-value))
  (let ((hchroma1 (floor half-chroma))
	(hchroma2 (ceiling half-chroma)))
    (if (= hchroma1 hchroma2)
	(mhvc-to-lchab-value-chroma-integer-case hue40 tmp-value (round half-chroma) dark)
	(multiple-value-bind (lstar astar1 bstar1)
	    (multiple-value-call #'lchab-to-lab
	      (mhvc-to-lchab-value-chroma-integer-case hue40 tmp-value hchroma1 dark))
	  (multiple-value-bind (disused astar2 bstar2)
	      (multiple-value-call #'lchab-to-lab
		(mhvc-to-lchab-value-chroma-integer-case hue40 tmp-value hchroma2 dark))
	    (declare (ignore disused)
		     (double-float lstar astar1 bstar1 astar2 bstar2))
	    (let* ((astar (+ (* astar1 (- hchroma2 half-chroma))
			     (* astar2 (- half-chroma hchroma1))))
		   (bstar (+ (* bstar1 (- hchroma2 half-chroma))
			     (* bstar2 (- half-chroma hchroma1)))))
	      (lab-to-lchab lstar astar bstar)))))))


(defun mhvc-to-lchab-general-case (hue40 tmp-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
  	   ((double-float 0d0 40d0) hue40 tmp-value)
  	   ((double-float 0d0 #.*maximum-chroma*) half-chroma))
  (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
    (let  ((tmp-val1 (floor tmp-value))
	   (tmp-val2 (ceiling tmp-value))
	   (lstar (munsell-value-to-lstar true-value)))
      (if (= tmp-val1 tmp-val2)
	  (mhvc-to-lchab-value-integer-case hue40 tmp-val1 half-chroma dark)
	  ;; If the given color is so dark that it is out of MRD, we
	  ;; use the fact that the chroma and hue of LCh(ab)
	  ;; corresponds roughly to that of Munsell.
	  (if (zerop tmp-val1)
	      (multiple-value-bind (disused cstarab hab)
		  (mhvc-to-lchab-value-integer-case hue40 1 half-chroma dark)
		(declare (ignore disused))
		(values lstar cstarab hab))
	      (multiple-value-bind (lstar1 astar1 bstar1)
		  (multiple-value-call #'lchab-to-lab
		    (mhvc-to-lchab-value-integer-case hue40 tmp-val1 half-chroma dark))
		(multiple-value-bind (lstar2 astar2 bstar2)
		    (multiple-value-call #'lchab-to-lab
		      (mhvc-to-lchab-value-integer-case hue40 tmp-val2 half-chroma dark))
		  (declare (double-float lstar lstar1 astar1 bstar1 lstar2 astar2 bstar2))
		  (let ((astar (+ (* astar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
				  (* astar2 (/ (- lstar lstar1) (- lstar2 lstar1)))))
			(bstar (+ (* bstar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
				  (* bstar2 (/ (- lstar lstar1) (- lstar2 lstar1))))))
		    (lab-to-lchab lstar astar bstar)))))))))


(define-condition invalid-mhvc-error (simple-error)
  ((value :initarg :value
	  :initform 0d0
	  :accessor cond-value)
   (chroma :initarg :chroma
	   :initform 0d0
	   :accessor cond-chroma))
  (:report (lambda (condition stream)
	     (format stream "Value and chroma must be within [0, 10] and [0, ~A), respectively: (V C) = (~A ~A)"
		     *maximum-chroma*
		     (cond-value condition)
		     (cond-chroma condition)))))

      
(defun mhvc-out-of-mrd-p (hue40 value chroma)
  "Judge if MHVC is out of the Munsell renotation data."
  (or (< value 0) (> value 10)
      (< chroma 0)
      (> chroma (max-chroma hue40 value))))


(defun mhvc-invalid-p (hue40 value chroma)
  "Judge if MHVC is invalid."
  (declare (ignore hue40))
  (or (< value 0) (> value 10)
      (< chroma 0) (> chroma *maximum-chroma*)))

(declaim (inline mhvc-to-lchab))
(defun mhvc-to-lchab (hue40 value chroma)
  "Note: The Standard Illuminant is C."
  (declare (optimize (speed 3) (safety 1)))
  (let ((d-hue (mod (float hue40 1d0) 40d0))
	(d-value (clamp (float value 1d0) 0d0 10d0))
	(d-chroma (clamp (float chroma 1d0) 0d0 *maximum-chroma*)))
    ;; (format t "~A ~A ~A" d-hue (* d-value 5) (/ d-chroma 2))
    (if (>= d-value 1d0)
	(mhvc-to-lchab-general-case d-hue d-value (* d-chroma 0.5d0) nil)
	(mhvc-to-lchab-general-case d-hue (* d-value 5d0) (* d-chroma 0.5d0) t))))

(declaim (inline mhvc-to-xyz-illum-c))
(defun mhvc-to-xyz-illum-c (hue40 value chroma)
  "Illuminant C. (Munsell Renotation Data is measured under the
Illuminant C.)"
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'lchab-to-xyz
    (mhvc-to-lchab (float hue40 1d0) (float value 1d0) (float chroma 1d0))
    +illum-c+))

(declaim (inline mhvc-to-xyz))
(defun mhvc-to-xyz (hue40 value chroma)
  "Illuminant D65. It causes an error by Bradford transformation,
since the Munsell Renotation Data is measured under the Illuminant C."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call c-to-d65
    (mhvc-to-xyz-illum-c (float hue40 1d0) (float value 1d0) (float chroma 1d0))))

(defun mhvc-to-xyy (hue40 value chroma)
  "Illuminant D65."
  (multiple-value-call #'xyz-to-xyy
    (mhvc-to-xyz hue40 value chroma)))

(defun mhvc-to-lrgb (hue40 value chroma &optional (rgbspace +srgb+))
  "The standard illuminant is D65: that of RGBSPACE must also be D65."
  (multiple-value-call #'xyz-to-lrgb
    (mhvc-to-xyz hue40 value chroma)
    rgbspace))

(declaim (inline mhvc-to-qrgb))
(defun mhvc-to-qrgb (hue40 value chroma &key (rgbspace +srgb+) (clamp nil))
  "The standard illuminant is D65: that of RGBSPACE must also be D65."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'xyz-to-qrgb
    (mhvc-to-xyz hue40 value chroma)
    :rgbspace rgbspace
    :clamp clamp))

(defun bench-mhvc-to-qrgb (&optional (num 300000))
  (time (dotimes (x num)
	  (mhvc-to-qrgb (random 40d0) (random 10d0) (random 50d0)))))

(define-condition invalid-munsell-spec-error (simple-error)
  ((spec :initarg :spec
	 :initform nil
	 :accessor cond-spec))
  (:report (lambda (condition stream)
	     (format stream "Invalid Munsell spec.: ~A"
		     (cond-spec condition)))))
      
(defun munsell-to-mhvc (munsellspec)
  "Usage Example:
CL-USER> (dufy:munsell-to-mhvc \"0.02RP 0.9/3.5\")
=> (36.00799999982119d0 0.8999999761581421d0 3.5d0)
Many other notations are acceptable as number designations; an ugly
specification as follows are also available:
CL-USER> (dufy:munsell-to-mhvc \"2d-2RP .9/ #x0ffffff\")
=> (36.008d0 0.8999999761581421d0 1.6777215d7)
but the capital letters and  '/' are reserved:
CL-USER> (dufy:munsell-to-mhvc \"2D-2RP 9/10 / #x0FFFFFF\")
=> ERROR,
"
  (let ((lst (let ((*read-default-float-format* 'double-float))
	       (mapcar (compose (rcurry #'coerce 'double-float)
				#'read-from-string)
		       (remove "" (cl-ppcre:split "[^0-9.a-z#\-]+" munsellspec)
			       :test #'string=)))))
    (let* ((hue-name (cl-ppcre:scan-to-strings "[A-Z]+" munsellspec))
	   (hue-number
	    (switch (hue-name :test #'string=)
	      ("R" 0) ("YR" 1) ("Y" 2) ("GY" 3) ("G" 4)
	      ("BG" 5) ("B" 6) ("PB" 7) ("P" 8) ("RP" 9) ("N" -1)
	      (t (error (make-condition 'invalid-munsell-spec-error
					:spec (format nil "Invalid hue designator: ~A" hue-name)))))))
      (if (< hue-number 0)
	  (values 0d0 (car lst) 0d0)
	  (if (/= (length lst) 3)
	      (error (make-condition 'invalid-munsell-spec-error
				     :spec lst))
	      (progn
		(setf (car lst) (+ (* hue-number 4) (/ (* (car lst) 2) 5)))
		(values-list lst)))))))

(defun mhvc-to-munsell (hue40 value chroma &optional (digits 2))
  (let ((unit (concatenate 'string "~," (write-to-string digits) "F")))
    (if (< chroma (* 0.5d0 (expt 0.1d0 digits))) ; if achromatic
	(format nil (concatenate 'string "N " unit) value)
	(let* ((hue40$ (mod hue40 40d0))
	       (hue-number (floor (/ hue40$ 4)))
	       (hue-prefix (* (mod hue40$ 4) 2.5d0))
	       (hue-name (aref #("R" "YR" "Y" "GY" "G"
				 "BG" "B" "PB" "P" "RP")
			       hue-number)))
	  (format nil (concatenate 'string unit "~A " unit "/" unit)
		  hue-prefix hue-name value chroma)))))



(defun munsell-out-of-mrd-p (munsellspec)
  (multiple-value-call #'mhvc-out-of-mrd-p (munsell-to-mhvc munsellspec)))

(defun munsell-to-lchab (munsellspec)
  "Illuminant C."
  (multiple-value-bind (hue40 value chroma) (munsell-to-mhvc munsellspec)
    (if (mhvc-invalid-p hue40 value chroma)
	(error (make-condition 'invalid-mhvc-error :value value :chroma chroma))
	(mhvc-to-lchab hue40 value chroma))))

(defun munsell-to-xyz (munsellspec)
  "Illuminant D65."
  (multiple-value-bind (hue40 value chroma) (munsell-to-mhvc munsellspec)
    (if (mhvc-invalid-p hue40 value chroma)
	(error (make-condition 'invalid-mhvc-error :value value :chroma chroma))
	(mhvc-to-xyz hue40 value chroma))))


(defun munsell-to-xyy (munsellspec)
  "Illuminant D65."
  (multiple-value-call #'xyz-to-xyy (munsell-to-xyz munsellspec)))


(defun munsell-to-qrgb (munsellspec &key (rgbspace +srgb+) (clamp nil))
  "Illuminant D65; the standard illuminant of RGBSPACE must also be D65."
  (multiple-value-call #'xyz-to-qrgb
    (munsell-to-xyz munsellspec)
    :rgbspace rgbspace
    :clamp clamp))


(defun max-chroma-lchab (hue40 value &key (use-dark t))
  "Returns the LCh(ab) value of the color on the max-chroma boundary in MRD."
  (mhvc-to-lchab hue40
		 value
		 (max-chroma hue40 value :use-dark use-dark)))

; avoid that x slightly exceeds an integer 
(defun modify-float-error (x epsilon)
  (if (<= (- x (floor x)) epsilon)
      (floor x)
      x))

;; (defun rough-munsell-hue-to-hab (h)
;;   (mod (* h 9) 360))

;; (defun rough-munsell-chroma-to-cstarab (c)
;;   (* c 5))


;; used in INVERT-LCHAB-TO-MHVC
(defun rough-lchab-to-mhvc (lstar cstarab hab)
  (declare (optimize (speed 3) (safety 0))
	   (double-float lstar cstarab hab))
  (values (* hab #.(float 40/360 1d0))
	  (lstar-to-munsell-value lstar)
	  (* cstarab #.(/ 5.5d0))))

(defun lstar-to-munsell-value (lstar)
  (y-to-munsell-value (lstar-to-y lstar)))


;; used in INVERT-LCHAB-TO-MHVC
(declaim (ftype (function * double-float) circular-delta))
(defun circular-delta (theta1 theta2)
  (declare (optimize (speed 3) (safety 0))
	   (double-float theta1 theta2))
  (let ((z (mod (- theta1 theta2) 360d0)))
    (if (<= z 180)
	z
	(- z 360d0))))


;; used in INVERT-LCHAB-TO-MHVC
(declaim (inline invert-mhvc-to-lchab-with-init))
(defun invert-mhvc-to-lchab-with-init (lstar cstarab hab init-hue40 init-chroma &key (max-iteration 200) (factor 0.5d0) (threshold 1d-6))
  "Illuminant C."
  (declare (optimize (speed 3) (safety 1))
	   (double-float lstar cstarab hab))
  (let ((factor (float factor 1d0))
	(threshold (float threshold 1d0))
	(tmp-hue40 (float init-hue40 1d0))
	(v (lstar-to-munsell-value lstar))
	(tmp-c (float init-chroma 1d0)))
    (declare (double-float tmp-hue40))
    (dotimes (i max-iteration (values (mod tmp-hue40 40) v tmp-c max-iteration))
      (multiple-value-bind (disused tmp-cstarab tmp-hab)
	  (mhvc-to-lchab tmp-hue40 v tmp-c)
	(declare (ignore disused))
	(let* ((delta-cstarab (- cstarab tmp-cstarab))
	       (delta-hab (circular-delta hab tmp-hab))
	       (delta-hue40 (* delta-hab #.(float 40/360 1d0)))
	       (delta-c (* delta-cstarab #.(/ 5.5d0))))
	  (if (and (<= (abs delta-hue40) threshold)
		   (<= (abs delta-c) threshold))
	      (return (values (mod tmp-hue40 40d0) v tmp-c i))
	      (setf tmp-hue40 (+ tmp-hue40 (* factor delta-hue40))
		    tmp-c (max (+ tmp-c (* factor delta-c)) 0d0))))))))


(defun lchab-to-mhvc (lstar cstarab hab &key (max-iteration 200) (factor 0.5d0) (threshold 1d-6))
  "Illuminant C.
An inverter of MHVC-TO-LCHAB with a simple iteration algorithm:
V := LSTAR-TO-MUNSELL-VALUE(LSTAR);
H_(n+1) := H_n + factor * delta(H_n);
C_(n+1) :=  C_n + factor * delta(C_n),
where delta(H_n) and delta(C_n) is internally calculated at every
step. H and C could diverge, if FACTOR is too large. The return
values are as follows:
1. If max(delta(H_n), delta(C_n)) falls below THRESHOLD:
=> H V C NUMBER-OF-ITERATION.
2. If the number of iteration exceeds MAX-ITERATION:
=> H V C MAX-ITERATION.
In other words: The inversion has failed, if the second value is
equal to MAX-ITERATION.
"
  (declare (optimize (speed 3) (safety 1))
	   (fixnum max-iteration))
  (let ((lstar (float lstar 1d0))
	(cstarab (float cstarab 1d0))
	(hab (float hab 1d0)))
    (multiple-value-bind (init-h disused init-c)
	(rough-lchab-to-mhvc lstar cstarab hab)
      (declare (ignore disused))
      (invert-mhvc-to-lchab-with-init lstar cstarab hab
				      init-h init-c
				      :max-iteration max-iteration
				      :factor factor
				      :threshold threshold))))

(defun lchab-to-munsell (lstar cstarab hab &key (max-iteration 200) (factor 0.5d0) (threshold 1d-6) (digits 2))
  "Illuminant C."
  (multiple-value-bind (h v c ite)
      (lchab-to-mhvc lstar cstarab hab
		     :max-iteration max-iteration
		     :factor factor
		     :threshold threshold)
    (values (mhvc-to-munsell h v c digits)
	    ite)))

(defun xyz-to-mhvc (x y z &key (max-iteration 200) (factor 0.5d0) (threshold 1d-6))
  "Illuminant D65. including Bradford transformation."
  (multiple-value-call #'lchab-to-mhvc
    (multiple-value-call #'xyz-to-lchab
      (funcall d65-to-c x y z)
      +illum-c+)
    :max-iteration max-iteration
    :factor factor
    :threshold threshold))


(defun xyz-to-munsell (x y z &key (max-iteration 200) (factor 0.5d0) (threshold 1d-6) (digits 2))
  "Illuminant D65. including Bradford transformation."
  (multiple-value-bind (m h v ite)
      (xyz-to-mhvc x y z
		   :max-iteration max-iteration
		   :factor factor
		   :threshold threshold)
    (values (mhvc-to-munsell m h v digits) ite)))

  
(defun test-inverter ()
  "For devel."
  (let ((max-iteration 300))
    (do ((lstar 0 (+ lstar 10)))
	((> lstar 100) 'done)
      (do ((hab 0 (+ hab 9)))
	  ((= hab 360))
	(do ((cstarab 0 (+ cstarab 10)))
	    ((= cstarab 200) nil)
	  (multiple-value-bind (lst number)
	      (lchab-to-mhvc lstar cstarab hab
			     :threshold 1d-6 :max-iteration max-iteration)
	    (declare (ignore lst))
	    (when (= number max-iteration)
	      (format t "failed at L*=~A C*ab=~A Hab=~A.~%" lstar cstarab hab))))))))

(defun test-inverter2 (&optional (num-loop 10000) (profile nil) (rgbspace +srgb+))
  "For devel."
  #+sbcl(when profile (sb-profile:profile "DUFY"))
  #-sbcl(declare (ignore profile))
  (let ((qmax+1 (1+ (rgbspace-qmax rgbspace)))
	(cat-func (gen-cat-function (rgbspace-illuminant rgbspace) +illum-c+))
	(sum 0)
	(max-ite 300))
    (dotimes (x num-loop (prog1 (float (/ sum num-loop) 1d0)
			   #+sbcl(when profile
				   (sb-profile:report :print-no-call-list nil)
				   (sb-profile:unprofile "DUFY"))))
      (let ((qr (random qmax+1)) (qg (random qmax+1)) (qb (random qmax+1)))
	(multiple-value-bind (lstar cstarab hab)
	    (multiple-value-call #'xyz-to-lchab
	      (multiple-value-call cat-func
		(qrgb-to-xyz qr qg qb rgbspace))
	      +illum-c+)
	  (multiple-value-bind (h v c ite)
	      (lchab-to-mhvc lstar cstarab hab :max-iteration max-ite :factor 0.5d0)
	    (declare (ignore h v c))
	    (when (= ite max-ite)
	      (incf sum)
	      (format t "~A ~A ~A, (~a ~a ~a) ~A~%" lstar cstarab hab qr qg qb ite))))))))

;; doesn't converge:
;; LCH = 90.25015693115249d0 194.95626408656423d0 115.6958104971207d0
;; (38754 63266 343) in ProPhoto, 16-bit

(defun test-inverter3 (&optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((max-ite 200)
	(sum 0))
    (dotimes (qr 256)
      (print qr)
      (dotimes (qg 256)
	(dotimes (qb 256)
	  (let ((ite (nth-value 3 (multiple-value-call #'xyz-to-mhvc 
				    (qrgb-to-xyz qr qg qb rgbspace)
				    :threshold 1.0d-3))))
	    (declare (fixnum ite))
	    (when (= ite max-ite)
	      (incf sum)
	      (format t "(~a ~a ~a) ~A~%" qr qg qb ite))))))
    (float (/ sum (* 256 256 256)) 1d0)))
