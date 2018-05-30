(in-package :dufy-core)

;;;
;;; RGB Color Space
;;;

(define-colorspace lrgb ((lr double-float) (lg double-float) (lb double-float)))
(define-colorspace rgb ((r double-float) (g double-float) (b double-float)))
(define-colorspace qrgb ((qr fixnum) (qg fixnum) (qb fixnum))
  :clamp :clampable)
(define-colorspace rgbpack ((int integer))
  :clamp :always-clamped)

(define-colorspace rgba ((r double-float) (g double-float) (b double-float) (alpha double-float)))
(define-colorspace qrgba ((qr fixnum) (qg fixnum) (qb fixnum) (qalpha fixnum))
  :clamp :clampable)
(define-colorspace rgbapack ((int integer))
  :clamp :always-clamped)

(defun gen-linearizer (gamma)
  "Returns a linearization function for a given gamma value. You
shouldn't call the returned function on your own, as it is not safe."
  (let ((gamma (float gamma 1d0)))
    #'(lambda (x)
	(declare (optimize (speed 3) (safety 0))
		 (double-float x))
	(if (plusp x)
	    (expt x gamma)
	    (- (expt (- x) gamma))))))

(defun gen-delinearizer (gamma)
  "Returns a gamma-correction function for a given gamma value. You
shouldn't call the returned function on your own, as it is not safe."
  (let ((/gamma (/ (float gamma 1d0))))
    #'(lambda (x)
	(declare (optimize (speed 3) (safety 0))
		 (double-float x))
	(if (plusp x)
	    (expt x /gamma)
	    (- (expt (- x) /gamma))))))

(defstruct (rgbspace (:constructor %make-rgbspace)
		     (:copier nil))
  "Structure of RGB space, including encoding characteristics"
  ;; primary coordinates in xyY space.
  (xr 0d0 :type double-float) (yr 0d0 :type double-float)
  (xg 0d0 :type double-float) (yg 0d0 :type double-float)
  (xb 0d0 :type double-float) (yb 0d0 :type double-float)
  
  (illuminant +illum-d65+ :type illuminant)
  (to-xyz-matrix +identity-matrix+ :type matrix33)
  (from-xyz-matrix +identity-matrix+ :type matrix33)

  ;; nominal range of linear values
  (lmin 0d0 :type double-float)
  (lmax 1d0 :type double-float)
  
  (linearizer (rcurry #'float 1d0) :type (function * (values double-float &optional)))
  (delinearizer (rcurry #'float 1d0) :type (function * (values double-float &optional)))

  ;; nominal range of gamma-corrected values
  (min 0d0 :type double-float)
  (max 1d0 :type double-float)
  (length 1d0 :type double-float) ; length of the interval [min, max]
  (/length 1d0 :type double-float) ; reciprocal
  (normal t :type boolean) ; t, if min = 0d0 and max = 1d0

  ;; quantization
  (bit-per-channel 8 :type (integer 1 #.(floor (log most-positive-fixnum 2))))
  (qmax 255 :type (integer 1 #.most-positive-fixnum)) ; maximum of quantized values
  (qmax-float 255d0 :type double-float)
  (length/qmax-float (float 1/255 1d0) :type double-float)
  (qmax-float/length 255d0 :type double-float))


(defun make-rgbspace (xr yr xg yg xb yb &key (illuminant +illum-d65+) (lmin 0d0) (lmax 1d0) (linearizer (rcurry #'float 1d0)) (delinearizer (rcurry #'float 1d0)) (bit-per-channel 8) (force-normal nil))
  "xr, yr, xg, yg, xb, yb := primary coordinates in the xy plane.
[lmin, lmax] := range of linear values ([0, 1] typically).

LINEARIZER and DELINEARIZER must be (FUNCTION * (VALUES DOUBLE-FLOAT
&OPTIONAL)).  If FORCE-NORMAL is T, the nominal range of
gamma-corrected value is forcibly set to [0, 1]."
  (declare (optimize (speed 3) (safety 1))
	   ((function * double-float) linearizer delinearizer))
  (with-double-float (xr yr xg yg xb yb)
    (let ((coordinates
            (make-array '(3 3)
                        :element-type 'double-float
                        :initial-contents
                        (list (list xr xg xb)
                              (list yr yg yb)
                              (list (- 1d0 xr yr) (- 1d0 xg yg) (- 1d0 xb yb))))))
      (multiple-value-bind (sr sg sb)
	  (multiply-mat-vec (invert-matrix33 coordinates)
			    (illuminant-x illuminant)
			    1d0
			    (illuminant-z illuminant))
	(let* ((mat
                 (make-array '(3 3)
                             :element-type 'double-float
                             :initial-contents
                             (list (list (* sr (aref coordinates 0 0))
                                         (* sg (aref coordinates 0 1))
                                         (* sb (aref coordinates 0 2)))
                                   (list (* sr (aref coordinates 1 0))
                                         (* sg (aref coordinates 1 1))
                                         (* sb (aref coordinates 1 2)))
                                   (list (* sr (aref coordinates 2 0))
                                         (* sg (aref coordinates 2 1))
                                         (* sb (aref coordinates 2 2))))))
	       (min (if force-normal 0d0 (funcall delinearizer lmin)))
	       (max (if force-normal 1d0 (funcall delinearizer lmax)))
	       (normal (and (= min 0d0) (= max 1d0)))
	       (qmax (- (expt 2 bit-per-channel) 1))
	       (qmax-float (float qmax 1d0))
	       (len (- max min)))
	  (%make-rgbspace :xr xr :yr yr :xg xg :yg yg :xb xb :yb yb
			  :illuminant illuminant
			  :linearizer linearizer
			  :delinearizer delinearizer
			  :to-xyz-matrix mat
			  :from-xyz-matrix (invert-matrix33 mat)
			  :lmin lmin
			  :lmax lmax
			  :min min
			  :max max
			  :length len
                          :/length (/ len)
			  :normal normal
			  :bit-per-channel bit-per-channel
			  :qmax qmax
			  :qmax-float qmax-float
			  :qmax-float/length (/ qmax-float len)
			  :length/qmax-float (/ len qmax-float)))))))

(defvar +srgb+) ; later defined

(define-primary-converter (xyz lrgb) (x y z &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiply-mat-vec (rgbspace-from-xyz-matrix rgbspace)
		    (float x 1d0)
		    (float y 1d0)
		    (float z 1d0)))

(define-primary-converter (lrgb xyz) (lr lg lb &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiply-mat-vec (rgbspace-to-xyz-matrix rgbspace)
		    (float lr 1d0)
		    (float lg 1d0)
		    (float lb 1d0)))


;;;
;;; Linear RGB, gamma-corrected RGB and quantized RGB
;;;

(defun lrgb-out-of-gamut-p (lr lg lb &key (rgbspace +srgb+) (threshold 1d-4))
  (declare (optimize (speed 3) (safety 1)))
  "Returns true, if at least one of LR, LG and LB is outside the
interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX + THRESHOLD]"
  (with-double-float (lr lg lb threshold)
    (let ((inf (- (rgbspace-lmin rgbspace) threshold))
          (sup (+ (rgbspace-lmax rgbspace) threshold)))
      (not (and  (<= inf lr sup)
                 (<= inf lg sup)
                 (<= inf lb sup))))))

(declaim (inline linearize))
(defun linearize (x &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (funcall (rgbspace-linearizer rgbspace) (float x 1d0)))

(declaim (inline delinearize))
(defun delinearize (x &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (funcall (rgbspace-delinearizer rgbspace) (float x 1d0)))

(define-primary-converter (lrgb rgb) (lr lg lb &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((delin (rgbspace-delinearizer rgbspace)))
    (values (funcall delin (float lr 1d0))
	    (funcall delin (float lg 1d0))
	    (funcall delin (float lb 1d0)))))

(define-primary-converter (rgb lrgb) (r g b &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((lin (rgbspace-linearizer rgbspace)))
    (values (funcall lin (float r 1d0))
	    (funcall lin (float g 1d0))
	    (funcall lin (float b 1d0)))))

(defun rgb-out-of-gamut-p (r g b &key (rgbspace +srgb+) (threshold 1d-4))
  "Returns true, if at least one of R, G and B is outside the interval
[RGBSPACE-MIN - THRESHOLD, RGBSPACE-MAX + THRESHOLD]"
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (r g b threshold)
    (let ((inf (- (rgbspace-min rgbspace) threshold))
	  (sup (+ (rgbspace-max rgbspace) threshold)))
      (not (and (<= inf r sup)
		(<= inf g sup)
		(<= inf b sup))))))

(defconverter xyz rgb)
(defconverter rgb xyz)

(defun bench-xyz-to-rgb (&optional (num 5000000))
  "For devel."
  (time-median 10
    (dotimes (i num)
      (xyz-to-rgb 0.1 0.2 0.3))))


(declaim (inline qrgb-out-of-gamut-p))
(defun qrgb-out-of-gamut-p (qr qg qb &key (rgbspace +srgb+) (threshold 0))
  (declare (optimize (speed 3) (safety 1))
	   (fixnum qr qg qb threshold))
  (let ((inf (- threshold))
	(sup (+ (rgbspace-qmax rgbspace) threshold)))
    (not (and (<= inf qr sup)
	      (<= inf qg sup)
	      (<= inf qb sup)))))

(declaim (inline quantize))
(defun quantize (x &key (rgbspace +srgb+) (clamp t))
  "RGB value to QRGB value"
  (declare (optimize (speed 3) (safety 1)))
  (if clamp
      (clamp (round (* (- (float x 1d0) (rgbspace-min rgbspace))
                       (rgbspace-qmax-float/length rgbspace)))
             0 (rgbspace-qmax rgbspace))
      (round (* (- (float x 1d0) (rgbspace-min rgbspace))
                (rgbspace-qmax-float/length rgbspace)))))

(declaim (inline dequantize))
(defun dequantize (n &key (rgbspace +srgb+))
  "QRGB value to RGB value"
  (declare (optimize (speed 3) (safety 1))
           (fixnum n))
  (+ (rgbspace-min rgbspace)
     (* n (rgbspace-length/qmax-float rgbspace))))

(define-primary-converter (rgb qrgb) (r g b &key (rgbspace +srgb+) (clamp t))
  "Quantizes RGB values from [RGBSPACE-MIN, RGBSPACE-MAX] ([0, 1],
typically) to {0, 1, ..., RGBSPACE-QMAX} ({0, 1, ..., 255},
typically), though it accepts all the real values."
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (r g b)
    (let ((min (rgbspace-min rgbspace))
	  (qmax-float/length (rgbspace-qmax-float/length rgbspace))
	  (qmax (rgbspace-qmax rgbspace)))
      (if clamp
          (values (clamp (round (* (- r min) qmax-float/length)) 0 qmax)
                  (clamp (round (* (- g min) qmax-float/length)) 0 qmax)
                  (clamp (round (* (- b min) qmax-float/length)) 0 qmax))
          (values (round (* (- r min) qmax-float/length))
                  (round (* (- g min) qmax-float/length))
                  (round (* (- b min) qmax-float/length)))))))

(define-primary-converter (qrgb rgb) (qr qg qb &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (fixnum qr qg qb))
  (let ((min (rgbspace-min rgbspace))
	(length/qmax-float (rgbspace-length/qmax-float rgbspace)))
    (values (+ min (* qr length/qmax-float))
	    (+ min (* qg length/qmax-float))
	    (+ min (* qb length/qmax-float)))))

(defconverter lrgb qrgb)
(defconverter qrgb lrgb)


(defun bench-qrgb-to-lrgb (&optional (num 8000000))
  (time-median 10
    (dotimes (i num)
      (qrgb-to-lrgb 100 200 50))))


(defconverter xyz qrgb)
(defconverter qrgb xyz)


(define-primary-converter (qrgb rgbpack) (qr qg qb &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (fixnum qr qg qb))
  (let ((bpc (rgbspace-bit-per-channel rgbspace))
	(qmax (rgbspace-qmax rgbspace)))
    (+ (ash (clamp qr 0 qmax) (+ bpc bpc))
       (ash (clamp qg 0 qmax) bpc)
       (clamp qb 0 qmax))))

(define-primary-converter (rgbpack qrgb) (int &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer int))
  (let ((minus-bpc (- (rgbspace-bit-per-channel rgbspace)))
	(qmax (rgbspace-qmax rgbspace)))
    (values (logand (ash int (+ minus-bpc minus-bpc)) qmax)
	    (logand (ash int minus-bpc) qmax)
	    (logand int qmax))))

(define-primary-converter (qrgba rgbapack) (qr qg qb qalpha &key (rgbspace +srgb+) (order :argb))
  (declare (optimize (speed 3) (safety 1))
	   (fixnum qr qg qb qalpha))
  "The order can be :ARGB or :RGBA. Note that it is different from the
  'physical' byte order in a machine, which depends on the endianess."
  (let* ((bpc (rgbspace-bit-per-channel rgbspace))
	 (2bpc (+ bpc bpc))
	 (qmax (rgbspace-qmax rgbspace)))
    (ecase order
      (:argb (+ (clamp qb 0 qmax)
		(ash (clamp qg 0 qmax) bpc)
		(ash (clamp qr 0 qmax) 2bpc)
		(ash (clamp qalpha 0 qmax) (+ 2bpc bpc))))
      (:rgba (+ (clamp qalpha 0 qmax)
		(ash (clamp qb 0 qmax) bpc)
		(ash (clamp qg 0 qmax) 2bpc)
		(ash (clamp qr 0 qmax) (+ 2bpc bpc)))))))

(define-primary-converter (rgbapack qrgba) (int &key (rgbspace +srgb+) (order :argb))
  (declare (optimize (speed 3) (safety 1))
	   (integer int))
  "The order can be :ARGB or :RGBA. Note that it is different from the
  'physical' byte order in a machine, which depends on the endianess."
  (let* ((-bpc (- (rgbspace-bit-per-channel rgbspace)))
	 (-2bpc (+ -bpc -bpc))
	 (qmax (rgbspace-qmax rgbspace)))
    (ecase order
      (:argb (values (logand (ash int -2bpc) qmax)
		     (logand (ash int -bpc) qmax)
		     (logand int qmax)
		     (logand (ash int (+ -2bpc -bpc)) qmax)))
      (:rgba (values (logand (ash int (+ -2bpc -bpc)) qmax)
		     (logand (ash int -2bpc) qmax)
		     (logand (ash int -bpc) qmax)
		     (logand int qmax))))))

(defconverter rgbpack rgb)
(defconverter rgb rgbpack)

(defconverter rgbpack lrgb)
(defconverter lrgb rgbpack)

(defconverter rgbpack xyz)
(defconverter xyz rgbpack)



;;;
;;; HSV/HSL
;;;

(define-colorspace hsv ((hue double-float) (sat double-float) (val double-float)))
(define-colorspace hsl ((hue double-float) (sat double-float) (lum double-float)))

(defmacro macrolet-applied-only-when (test definitions &body body)
  `(if ,test
       (macrolet ,definitions
         ,@body)
       (macrolet ,(loop for def in definitions
                        collect `(,(car def) (arg) arg))
         ,@body)))

(define-primary-converter (hsv rgb) (hue sat val &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  "HUE is in the circle group R/360. The nominal range of SAT and VAL
is [0, 1]. All the real values outside the interval are also
acceptable."
  (let ((hue (the (double-float 0d0 360d0) (mod (float hue 1d0) 360d0)))
        (sat (float sat 1d0))
        (val (float val 1d0)))
    (let* ((c (* val sat))
           (h-prime (* hue #.(float 1/60 1d0)))
           (h-prime-int (floor h-prime))
           (x (* c (- 1d0 (abs (- (mod h-prime 2d0) 1d0)))))
           (base (- val c)))
      (macrolet-applied-only-when (not (rgbspace-normal rgbspace))
          ((%lerp (x) `(+ (rgbspace-min rgbspace)
                          (* ,x (rgbspace-length rgbspace)))))
        (cond ((= sat 0d0) (values (%lerp base) (%lerp base) (%lerp base)))
              ((= 0 h-prime-int) (values (%lerp val)
                                         (%lerp (+ base x))
                                         (%lerp base)))
              ((= 1 h-prime-int) (values (%lerp (+ base x))
                                         (%lerp val)
                                         (%lerp base)))
              ((= 2 h-prime-int) (values (%lerp base)
                                         (%lerp val)
                                         (%lerp (+ base x))))
              ((= 3 h-prime-int) (values (%lerp base)
                                         (%lerp (+ base x))
                                         (%lerp val)))
              ((= 4 h-prime-int) (values (%lerp (+ base x))
                                         (%lerp base)
                                         (%lerp val)))
              ((= 5 h-prime-int) (values (%lerp val)
                                         (%lerp base)
                                         (%lerp (+ base x))))
              (t (values 0d0 0d0 0d0) ; unreachable. just for avoiding warnings
                 ))))))

(defconverter hsv qrgb)
(defconverter hsv xyz)

(define-primary-converter (rgb hsv) (r g b &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (macrolet-applied-only-when (not (rgbspace-normal rgbspace))
      ((%lerp (x) ; scale to the range [0, 1]
              `(* (- ,x (rgbspace-min rgbspace))
                  (rgbspace-/length rgbspace))))
    (let ((r (%lerp (float r 1d0)))
          (g (%lerp (float g 1d0)))
          (b (%lerp (float b 1d0))))
      (let* ((maxrgb (max r g b))
             (minrgb (min r g b))
             (s (if (= maxrgb 0d0)
                    0d0
                    (/ (- maxrgb minrgb) maxrgb)))
             (h (cond ((= minrgb maxrgb) 0d0)
                      ((= minrgb b) (+ (* 60d0 (/ (- g r) (- maxrgb minrgb))) 60d0))
                      ((= minrgb r) (+ (* 60d0 (/ (- b g) (- maxrgb minrgb))) 180d0))
                      ((= minrgb g) (+ (* 60d0 (/ (- r b) (- maxrgb minrgb))) 300d0)))))
        (values h s maxrgb)))))

(defconverter qrgb hsv)
(defconverter xyz hsv)


(define-primary-converter (hsl rgb) (hue sat lum &key (rgbspace +srgb+))
  "HUE is in the circle group R/360. The nominal range of SAT and LUM is [0,
1]; all the real values outside the interval are also acceptable."
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (hue sat lum)
    (let* ((tmp (* 0.5d0 sat (- 1d0 (abs (+ lum lum -1d0)))))
	   (max (+ lum tmp))
	   (min (- lum tmp))
	   (delta (- max min))
	   (h-prime (floor (the (double-float 0d0 6d0)
				(* (mod hue 360d0) 1/60)))))
      (macrolet-applied-only-when (not (rgbspace-normal rgbspace))
          ((%lerp (x)
                  `(+ (rgbspace-min rgbspace)
                      (* ,x (rgbspace-length rgbspace)))))
        (cond ((= sat 0d0) (values (%lerp max) (%lerp max) (%lerp max)))
              ((= 0 h-prime) (values (%lerp max)
                                     (%lerp (+ min (* delta hue 1/60)))
                                     (%lerp min)))
              ((= 1 h-prime) (values (%lerp (+ min (* delta (- 120d0 hue) 1/60)))
                                     (%lerp max)
                                     (%lerp min)))
              ((= 2 h-prime) (values (%lerp min)
                                     (%lerp max)
                                     (%lerp (+ min (* delta (- hue 120d0) 1/60)))))
              ((= 3 h-prime) (values (%lerp min)
                                     (%lerp (+ min (* delta (- 240d0 hue) 1/60)))
                                     (%lerp max)))
              ((= 4 h-prime) (values (%lerp (+ min (* delta (- hue 240d0) 1/60)))
                                     (%lerp min)
                                     (%lerp max)))
              ((= 5 h-prime) (values (%lerp max)
                                     (%lerp min)
                                     (%lerp (+ min (* delta (- 360d0 hue) 1/60)))))
              (t (values 0d0 0d0 0d0) ; unreachable. just for avoiding warnings
                 ))))))
 
(defconverter hsl qrgb)
(defconverter hsl xyz)

(define-primary-converter (rgb hsl) (r g b &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (macrolet-applied-only-when (not (rgbspace-normal rgbspace))
      ((%lerp (x) ; scale to the range [0, 1]
              `(* (- ,x (rgbspace-min rgbspace))
                  (rgbspace-/length rgbspace))))
    (let ((r (%lerp (float r 1d0)))
          (g (%lerp (float g 1d0)))
          (b (%lerp (float b 1d0))))
      (let ((minrgb (min r g b))
            (maxrgb (max r g b)))
        (values (cond ((= minrgb maxrgb) 0d0)
                      ((= minrgb b) (+ 60d0 (* 60d0 (/ (- g r) (- maxrgb minrgb)))))
                      ((= minrgb r) (+ 180d0 (* 60d0 (/ (- b g) (- maxrgb minrgb)))))
                      ((= minrgb g) (+ 300d0 (* 60d0 (/ (- r b) (- maxrgb minrgb))))))
                (let ((denom (- 1d0 (abs (+ maxrgb minrgb -1d0)))))
                  (if (zerop denom)
                      0d0
                      (/ (- maxrgb minrgb) denom)))
                (* 0.5d0 (+ maxrgb minrgb)))))))

(defconverter qrgb hsl)
(defconverter xyz hsl)
