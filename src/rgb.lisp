(in-package :dufy)

;;;
;;; RGB Color Space
;;;

(defun gen-linearizer (gamma)
  "Returns a linearization function for a given gamma value."
  (let ((gamma (float gamma 1d0)))
    #'(lambda (x)
	(declare (optimize (speed 3) (safety 1))
		 (double-float x))
	(if (plusp x)
	    (expt x gamma)
	    (- (expt (- x) gamma))))))

(defun gen-delinearizer (gamma)
  "Returns a gamma-correction function for a given gamma value."
  (let ((/gamma (/ (float gamma 1d0))))
    #'(lambda (x)
	(declare (optimize (speed 3) (safety 1))
		 (double-float x))
	(if (plusp x)
	    (expt x /gamma)
	    (- (expt (- x) /gamma))))))

(defstruct (rgbspace (:constructor $make-rgbspace)
		     (:copier nil))
  "Structure of RGB space, including encoding characteristics"
  ;; primary coordinates in xyY space.
  (xr 0d0 :type double-float) (yr 0d0 :type double-float)
  (xg 0d0 :type double-float) (yg 0d0 :type double-float)
  (xb 0d0 :type double-float) (yb 0d0 :type double-float)
  
  (illuminant +illum-d65+ :type illuminant)
  (to-xyz-matrix +identity-matrix+ :type (simple-array double-float (3 3)))
  (from-xyz-matrix +identity-matrix+ :type (simple-array double-float (3 3)))

  ;; nominal range of linear values
  (lmin 0d0 :type double-float)
  (lmax 1d0 :type double-float)
  
  (linearizer (rcurry #'float 1d0) :type (function * double-float))
  (delinearizer (rcurry #'float 1d0) :type (function * double-float))

  ;; nominal range of gamma-corrected values
  (min 0d0 :type double-float)
  (max 1d0 :type double-float)
  (len 1d0 :type double-float) ; length of the interval [min, max]
  (normal t :type boolean) ; t, if min = 0d0 and max = 1d0

  ;; quantization
  (bit-per-channel 8 :type (integer 1 #.(floor (log most-positive-fixnum 2))))
  (qmax 255 :type (integer 1 #.most-positive-fixnum) :read-only t) ; max. of quantized values
  (qmax-float 255d0 :type double-float)
  (len/qmax-float (float 1/255 1d0) :type double-float)
  (qmax-float/len 255d0 :type double-float))



(defun make-rgbspace (xr yr xg yg xb yb &key (illuminant +illum-d65+) (lmin 0d0) (lmax 1d0) (linearizer (rcurry #'float 1d0)) (delinearizer (rcurry #'float 1d0)) (bit-per-channel 8) (force-normal nil))
  "LINEARIZER and DELINEARIZER must be (FUNCTION * DOUBLE-FLOAT).
If FORCE-NORMAL is T, the nominal range of gamma-corrected value is
forcibly set to [0, 1]."
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
			    (illuminant-y illuminant)
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
	       (normal (if (and (= min 0d0) (= max 1d0))
			   t nil))
	       (qmax (- (expt 2 bit-per-channel) 1))
	       (qmax-float (float qmax 1d0))
	       (len (- max min)))
	  ($make-rgbspace :xr xr :yr yr :xg xg :yg yg :xb xb :yb yb
			  :illuminant illuminant
			  :linearizer linearizer
			  :delinearizer delinearizer
			  :to-xyz-matrix mat
			  :from-xyz-matrix (invert-matrix33 mat)
			  :lmin lmin
			  :lmax lmax
			  :min min
			  :max max
			  :len len
			  :normal normal
			  :bit-per-channel bit-per-channel
			  :qmax qmax
			  :qmax-float qmax-float
			  :qmax-float/len (/ qmax-float len)
			  :len/qmax-float (/ len qmax-float)))))))

(defvar +srgb+) ; later defined

(declaim (inline xyz-to-lrgb))
(defun xyz-to-lrgb (x y z &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiply-mat-vec (rgbspace-from-xyz-matrix rgbspace)
		    (float x 1d0)
		    (float y 1d0)
		    (float z 1d0)))

(declaim (inline lrgb-to-xyz))
(defun lrgb-to-xyz (lr lg lb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiply-mat-vec (rgbspace-to-xyz-matrix rgbspace)
		    (float lr 1d0)
		    (float lg 1d0)
		    (float lb 1d0)))


(defun copy-rgbspace (rgbspace &key (illuminant nil) (bit-per-channel nil))
  "Returns a new RGBSPACE with different standard illuminant and/or
bit-per-channel. All the parameters are properly recalculated. If both
are nil, it is just a copier."
  (destructuring-bind (new-xr new-yr new-xg new-yg new-xb new-yb)
      (if illuminant
	  (let ((ca-func (gen-cat-function (rgbspace-illuminant rgbspace) illuminant)))
	    (labels ((get-new-xy (r g b)
		       (multiple-value-bind (small-x small-y y)
			   (multiple-value-call #'xyz-to-xyy
			     (multiple-value-call ca-func
			       (lrgb-to-xyz r g b rgbspace)))
			 (declare (ignore y))
			 (list small-x small-y))))
	      (append (get-new-xy 1 0 0)
		      (get-new-xy 0 1 0)
		      (get-new-xy 0 0 1))))
	  (list (rgbspace-xr rgbspace) (rgbspace-yr rgbspace)
		(rgbspace-xg rgbspace) (rgbspace-yg rgbspace)
		(rgbspace-xb rgbspace) (rgbspace-yb rgbspace)))
    (make-rgbspace new-xr new-yr new-xg new-yg new-xb new-yb
		   :illuminant (or illuminant (rgbspace-illuminant rgbspace))
		   :linearizer (rgbspace-linearizer rgbspace)
		   :delinearizer (rgbspace-delinearizer rgbspace)
		   :lmin (rgbspace-lmin rgbspace)
		   :lmax (rgbspace-lmax rgbspace)
		   :bit-per-channel (or bit-per-channel (rgbspace-bit-per-channel rgbspace))
		   :force-normal (rgbspace-normal rgbspace))))


;;;
;;; Predefined RGB spaces
;;;

(defun linearize-srgb (x)
  "linearizer of sRGB (actually the same as bg-sRGB)"
  (declare (optimize (speed 3) (safety 1))
	   (double-float x))
  (cond ((> x #.(* 0.0031308d0 12.92d0))
	 (expt (* (+ 0.055d0 x) #.(/ 1.055d0)) 2.4d0))
	((< x #.(* -0.0031308d0 12.92d0))
	 (- (expt (* (- 0.055d0 x) #.(/ 1.055d0)) 2.4d0)))
	(t (* x #.(/ 12.92d0)))))

(defun delinearize-srgb (x)
  "delinealizer of sRGB (actually the same as bg-sRGB)"
  (declare (optimize (speed 3) (safety 1))
	   (double-float x))
  (cond ((> x 0.0031308d0)
	 (+ (* 1.055d0 (expt x #.(/ 2.4d0))) -0.055d0))
	((< x -0.0031308d0)
	 (+ (* -1.055d0 (expt (- x) #.(/ 2.4d0))) 0.055d0))
	(t (* x 12.92d0))))

(defun linearize-scrgb-nl (x)
  (declare (optimize (speed 3) (safety 1))
	   (double-float x))
  (cond ((> x #.(* 4.5d0 0.018d0))
	 (expt (* (+ 0.099d0 x) #.(/ 1.099d0)) #.(/ 0.45d0)))
	((< x (* 4.5d0 -0.018d0))
	 (- (expt (* (- 0.099d0 x) #.(/ 1.099d0)) #.(/ 0.45d0))))
	(t (* x #.(/ 4.5d0)))))

(defun delinearize-scrgb-nl (x)
  (declare (optimize (speed 3) (safety 1))
	   (double-float x))
  (cond ((> x 0.018d0)
	 (+ (* 1.099d0 (expt x 0.45d0)) -0.099d0))
	((< x -0.018d0)
	 (+ (* -1.099d0 (expt (- x) 0.45d0)) 0.099d0))
	(t (* x 4.5d0))))


(defparameter +srgb+
  (make-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:linearizer #'linearize-srgb				
		:delinearizer #'delinearize-srgb
		:force-normal t)
  "sRGB, 8-bit per channel")

(defparameter +bg-srgb-10+
  (make-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:linearizer #'linearize-srgb				
		:delinearizer #'delinearize-srgb
		:lmin -0.53d0
		:lmax 1.68d0
		:bit-per-channel 10)
  "bg-sRGB, 10-bit per channel
http://www.color.org/chardata/rgb/bgsrgb.xalter")

(defparameter +bg-srgb-12+
  (copy-rgbspace +bg-srgb-10+ :bit-per-channel 12)
  "bg-sRGB, 12-bit per channel,
http://www.color.org/chardata/rgb/bgsrgb.xalter")

(defparameter +bg-srgb-16+
  (copy-rgbspace +bg-srgb-10+ :bit-per-channel 16)
  "bg-sRGB, 16-bit per channel,
http://www.color.org/chardata/rgb/bgsrgb.xalter")

(defparameter +scrgb-16+
  (make-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:lmin -0.5d0
		:lmax 7.4999d0
		:bit-per-channel 16)
  "scRGB(16), IEC 61966-2-2:2003
http://www.color.org/chardata/rgb/scrgb.xalter")

(defparameter +scrgb-nl+
  (make-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:lmin -0.6038d0
		:lmax 7.5913d0
		:linearizer #'linearize-scrgb-nl
		:delinearizer #'delinearize-scrgb-nl
		:bit-per-channel 12)
  "scRGB-nl, IEC 61966-2-2:2003
http://www.color.org/chardata/rgb/scrgb-nl.xalter")


(defparameter +adobe+
  (make-rgbspace 0.64d0 0.33d0 0.21d0 0.71d0 0.15d0 0.06d0
		:linearizer (gen-linearizer #.(float 563/256 1d0))
		:delinearizer (gen-delinearizer #.(float 563/256 1d0)))
  "Adobe RGB (1998), 8-bit per channel")

(defparameter +adobe-16+
  (copy-rgbspace +adobe+ :bit-per-channel 16)
  "Adobe RGB (1998), 16-bit per channel.")

(defparameter +wide-gamut+
  (make-rgbspace 0.7347d0 0.2653d0 0.1152d0 0.8264d0 0.1566d0 0.0177d0
		 :linearizer (gen-linearizer #.(float 563/256 1d0))
		 :delinearizer (gen-delinearizer #.(float 563/256 1d0)))
  "Wide-gamut RGB, 8-bit per channel.")

(defparameter +ntsc1953+
  (make-rgbspace 0.67d0 0.33d0 0.21d0 0.71d0 0.14d0 0.08d0
		:illuminant +illum-c+
		:linearizer (gen-linearizer 2.2d0)
		:delinearizer (gen-delinearizer 2.2d0))
  "NTSC RGB, Rec. ITU-R BT.470-6, System M, 8-bit per channel.
http://www.itu.int/dms_pubrec/itu-r/rec/bt/R-REC-BT.470-6-199811-S!!PDF-E.pdf")

(defparameter +pal/secam+
  (make-rgbspace 0.64d0 0.33d0 0.29d0 0.60d0 0.15d0 0.06d0
		:linearizer (gen-linearizer 2.8d0)
		:delinearizer (gen-delinearizer 2.8d0))
  "PAL/SECAM RGB, Rec. ITU-R BT.470-6, 8-bit per channel.
http://www.itu.int/dms_pubrec/itu-r/rec/bt/R-REC-BT.470-6-199811-S!!PDF-E.pdf")

(defun linearize-prophoto (x)
  (declare (optimize (speed 3) (safety 1))
	   (double-float x))
  (cond ((> x #.(* 1/512 16d0))
	 (expt x 1.8d0))
	((< x #.(* -1/512 16d0))
	 (- (expt (- x) 1.8d0)))
	(t (* x #.(float 1/16 1d0)))))
  
(defun delinearize-prophoto (x)
  (declare (optimize (speed 3) (safety 1))
	   (double-float x))
  (cond ((> x #.(float 1/512 1d0))
	 (expt x #.(/ 1.8d0)))
	((< x #.(float -1/512 1d0))
	 (- (expt (- x) #.(/ 1.8d0))))
	(t (* x 16d0))))
	    
(defparameter +prophoto+
  (make-rgbspace 0.7347d0 0.2653d0 0.1596d0 0.8404d0 0.0366d0 0.0001d0
		:illuminant +illum-d50+
		:linearizer #'linearize-prophoto
		:delinearizer #'delinearize-prophoto)
  "Prophoto RGB (also known as ROMM RGB), 8-bit per channel,
http://www.color.org/ROMMRGB.pdf")		      
 
(defparameter +prophoto-12+
  (copy-rgbspace +prophoto+ :bit-per-channel 12)
  "Prophoto RGB (also known as ROMM RGB), 12-bit per channel,
http://www.color.org/ROMMRGB.pdf")

(defparameter +prophoto-16+
  (copy-rgbspace +prophoto+ :bit-per-channel 16)
  "Prophoto RGB (also known as ROMM RGB), 16-bit per channel,
http://www.color.org/ROMMRGB.pdf")



;;;
;;; Linear RGB, gamma-corrected RGB and quantized RGB
;;;


(defun lrgb-out-of-gamut-p (lr lg lb &key (rgbspace +srgb+) (threshold 1d-4))
  "Returns true, if at least one of LR, LG and LB is outside the
interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX + THRESHOLD]"
  (let ((inf (- (rgbspace-lmin rgbspace) threshold))
	(sup (+ (rgbspace-lmax rgbspace) threshold)))
    (not (and  (<= inf lr sup)
	       (<= inf lg sup)
	       (<= inf lb sup)))))

(defun linearize (x &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (funcall (rgbspace-linearizer rgbspace) (float x 1d0)))

(defun delinearize (x &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (funcall (rgbspace-delinearizer rgbspace) (float x 1d0)))

(declaim (inline lrgb-to-rgb))
(defun lrgb-to-rgb (lr lg lb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((delin (rgbspace-delinearizer rgbspace)))
    (values (funcall delin (float lr 1d0))
	    (funcall delin (float lg 1d0))
	    (funcall delin (float lb 1d0)))))

(declaim (inline rgb-to-lrgb))
(defun rgb-to-lrgb (r g b &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((lin (rgbspace-linearizer rgbspace)))
    (values (funcall lin (float r 1d0))
	    (funcall lin (float g 1d0))
	    (funcall lin (float b 1d0)))))

(defun rgb-out-of-gamut-p (r g b &key (rgbspace +srgb+) (threshold 1d-4))
  "Returns true, if at least one of R, G and B is outside the interval
[RGBSPACE-MIN - THRESHOLD, RGBSPACE-MAX + THRESHOLD]"
  (declare (optimize (speed 3) (safety 1)))
  (let ((threshold (float threshold 1d0)))
    (let ((inf (- (rgbspace-min rgbspace) threshold))
	  (sup (+ (rgbspace-max rgbspace) threshold)))
      (not (and (<= inf r sup)
		(<= inf g sup)
		(<= inf b sup))))))

(declaim (inline xyz-to-rgb))
(defun xyz-to-rgb (x y z &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'lrgb-to-rgb
    (xyz-to-lrgb (float x 1d0) (float y 1d0) (float z 1d0) rgbspace)
    rgbspace))

(declaim (inline rgb-to-xyz))
(defun rgb-to-xyz (r g b &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'lrgb-to-xyz
    (rgb-to-lrgb (float r 1d0) (float g 1d0) (float b 1d0) rgbspace)
    rgbspace))


(declaim (inline qrgb-out-of-gamut-p))
(defun qrgb-out-of-gamut-p (qr qg qb &key (rgbspace +srgb+) (threshold 0))
  (declare (optimize (speed 3) (safety 1))
	   (integer qr qg qb threshold))
  (let ((inf (- threshold))
	(sup (+ (rgbspace-qmax rgbspace) threshold)))
    (not (and (<= inf qr sup)
	      (<= inf qg sup)
	      (<= inf qb sup)))))

(declaim (inline rgb-to-qrgb))
(defun rgb-to-qrgb (r g b &key (rgbspace +srgb+) (clamp t))
  "Quantizes RGB values from [RGBSPACE-MIN, RGBSPACE-MAX] ([0, 1], typically) to {0, 1,
..., RGBSPACE-QMAX} ({0, 1, ..., 255}, typically), though it accepts
all the real values."
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (r g b)
    (let ((min (rgbspace-min rgbspace))
	  (qmax-float/len (rgbspace-qmax-float/len rgbspace))
	  (clamper (if clamp
		       (the function (rcurry #'clamp 0 (rgbspace-qmax rgbspace)))
		       #'identity)))
      (values (funcall clamper (round (* (- r min) qmax-float/len)))
	      (funcall clamper (round (* (- g min) qmax-float/len)))
	      (funcall clamper (round (* (- b min) qmax-float/len)))))))   


(declaim (inline qrgb-to-rgb))
(defun qrgb-to-rgb (qr qg qb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer qr qg qb))
  (let ((min (rgbspace-min rgbspace))
	(len/qmax-float (rgbspace-len/qmax-float rgbspace)))
    (values (+ min (* qr len/qmax-float))
	    (+ min (* qg len/qmax-float))
	    (+ min (* qb len/qmax-float)))))


(declaim (inline lrgb-to-qrgb))
(defun lrgb-to-qrgb (lr lg lb &key (rgbspace +srgb+) (clamp t))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-qrgb
    (lrgb-to-rgb (float lr 1d0) (float lg 1d0) (float lb 1d0) rgbspace)
    :rgbspace rgbspace
    :clamp clamp))

(declaim (inline qrgb-to-lrgb))
(defun qrgb-to-lrgb (qr qg qb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer qr qg qb))
  (multiple-value-call #'rgb-to-lrgb
    (qrgb-to-rgb qr qg qb rgbspace)
    rgbspace))


(declaim (inline xyz-to-qrgb))
(defun xyz-to-qrgb (x y z &key (rgbspace +srgb+) (clamp t))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-qrgb
    (xyz-to-rgb (float x 1d0) (float y 1d0) (float z 1d0) rgbspace)
    :rgbspace rgbspace
    :clamp clamp))

(declaim (inline qrgb-to-xyz))
(defun qrgb-to-xyz (qr qg qb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer qr qg qb))
  (multiple-value-call #'rgb-to-xyz
    (qrgb-to-rgb qr qg qb rgbspace)
    rgbspace))


(declaim (inline qrgb-to-int))
(defun qrgb-to-int (qr qg qb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer qr qg qb))
  (let ((bpc (rgbspace-bit-per-channel rgbspace))
	(qmax (rgbspace-qmax rgbspace)))
    (+ (ash (clamp qr 0 qmax) (+ bpc bpc))
       (ash (clamp qg 0 qmax) bpc)
       (clamp qb 0 qmax))))

(declaim (inline int-to-qrgb))
(defun int-to-qrgb (int &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer int))
  (let ((minus-bpc (- (rgbspace-bit-per-channel rgbspace)))
	(qmax (rgbspace-qmax rgbspace)))
    (values (logand (ash int (+ minus-bpc minus-bpc)) qmax)
	    (logand (ash int minus-bpc) qmax)
	    (logand int qmax))))


;; For handling alpha channel.
;; These are improvised and not exported.
(defun qrgba-to-int (qr qg qb qalpha &optional (rgbspace +srgb+) (order :argb))
  "The order can be :ARGB or :RGBA. Note that it is different from the
  'physical' byte order in a machine, which depends on the endianess."
  (declare (optimize (speed 3) (safety 1))
	   (integer qr qg qb qalpha))
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

(defun int-to-qrgba (int &optional (rgbspace +srgb+) (order :argb))
  "The order can be :ARGB or :RGBA. Note that it is different from the
  'physical' byte order in a machine, which depends on the endianess."
  (declare (optimize (speed 3) (safety 1))
	   (integer int))
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

(defun bench-qrgb (&optional (num 5000000))
  (time-after-gc (dotimes (i num)
		   (multiple-value-call #'qrgb-to-int
		     (int-to-qrgb (random #.(expt 2 64)) +bg-srgb-16+)
		     +bg-srgb-16+))))



(declaim (inline int-to-rgb))
(defun int-to-rgb (int &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'qrgb-to-rgb
    (int-to-qrgb int rgbspace)
    rgbspace))

(declaim (inline rgb-to-int))
(defun rgb-to-int (r g b &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (r g b)
    (multiple-value-call #'qrgb-to-int
      (rgb-to-qrgb r g b :rgbspace rgbspace)
      rgbspace)))


(declaim (inline int-to-lrgb))
(defun int-to-lrgb (int &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'qrgb-to-lrgb
    (int-to-qrgb int rgbspace)
    rgbspace))

(declaim (inline lrgb-to-int))
(defun lrgb-to-int (lr lg lb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-int
    (lrgb-to-rgb (float lr 1d0) (float lg 1d0) (float lb 1d0) rgbspace)
    rgbspace))


(declaim (inline int-to-xyz))
(defun int-to-xyz (int &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer int))
  (multiple-value-call #'qrgb-to-xyz
    (int-to-qrgb int rgbspace)
    rgbspace))

(declaim (inline xyz-to-int))
(defun xyz-to-int (x y z &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'qrgb-to-int
    (xyz-to-qrgb (float x 1d0) (float y 1d0) (float z 1d0) :rgbspace rgbspace)
    rgbspace))



;;;
;;; HSV/HSL
;;;

(declaim (inline hsv-to-rgb))
(defun hsv-to-rgb (hue sat val)
  "HUE is in the circle group R/360. The nominal range of SAT and VAL is [0,
1]; all the real values outside the interval are also acceptable."
  (declare (optimize (speed 3) (safety 1)))
  (let ((hue (the (double-float 0d0 360d0) (mod (float hue 1d0) 360d0)))
	(sat (float sat 1d0))
	(val (float val 1d0)))
    (let* ((c (* val sat))
	   (h-prime (* hue #.(float 1/60 1d0)))
	   (h-prime-int (floor h-prime))
	   (x (* c (- 1d0 (abs (- (mod h-prime 2d0) 1d0)))))
	   (base (- val c)))
      (cond ((= sat 0d0) (values base base base))
	    ((= 0 h-prime-int) (values (+ base c) (+ base x) base))
	    ((= 1 h-prime-int) (values (+ base x) (+ base c) base))
	    ((= 2 h-prime-int) (values base (+ base c) (+ base x)))
	    ((= 3 h-prime-int) (values base (+ base x) (+ base c)))
	    ((= 4 h-prime-int) (values (+ base x) base (+ base c)))
	    ((= 5 h-prime-int) (values (+ base c) base (+ base x)))))))

(declaim (inline hsv-to-qrgb))
(defun hsv-to-qrgb (hue sat val &key (rgbspace +srgb+) (clamp t))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-qrgb
    (hsv-to-rgb hue sat val)
    :rgbspace rgbspace
    :clamp clamp))

(declaim (inline hsv-to-xyz))
(defun hsv-to-xyz (hue sat val &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-xyz
    (hsv-to-rgb hue sat val)
    rgbspace))

(declaim (inline rgb-to-hsv))
(defun rgb-to-hsv (r g b)
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (r g b)
    (let* ((maxrgb (max r g b))
	   (minrgb (min r g b))
	   (s (if (= maxrgb 0d0)
		  0d0
		  (/ (- maxrgb minrgb) maxrgb)))
	   (h (cond ((= minrgb maxrgb) 0d0)
		    ((= minrgb b) (+ (* 60d0 (/ (- g r) (- maxrgb minrgb))) 60d0))
		    ((= minrgb r) (+ (* 60d0 (/ (- b g) (- maxrgb minrgb))) 180d0))
		    ((= minrgb g) (+ (* 60d0 (/ (- r b) (- maxrgb minrgb))) 300d0)))))
      (values h s maxrgb))))

(declaim (inline qrgb-to-hsv))
(defun qrgb-to-hsv (qr qg qb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1))
	   (integer qr qg qb))
  (multiple-value-call #'rgb-to-hsv
    (qrgb-to-rgb qr qg qb rgbspace)))

(declaim (inline xyz-to-hsv))
(defun xyz-to-hsv (x y z &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-hsv
    (xyz-to-rgb x y z rgbspace)))
  

(declaim (inline hsl-to-rgb))
(defun hsl-to-rgb (hue sat lum)
  "HUE is in the circle group R/360. The nominal range of SAT and LUM is [0,
1]; all the real values outside the interval are also acceptable."
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (hue sat lum)
    (let* ((tmp (* 0.5d0 sat (- 1d0 (abs (- (* lum 2d0) 1d0)))))
	   (max (+ lum tmp))
	   (min (- lum tmp))
	   (delta (- max min))
	   (h-prime (floor (the (double-float 0d0 6d0)
				(* (mod hue 360d0) #.(float 1/60 1d0))))))
      (cond ((= sat 0d0) (values max max max))
	    ((= 0 h-prime) (values max
				   (+ min (* delta hue #.(float 1/60 1d0)))
				   min))
	    ((= 1 h-prime) (values (+ min (* delta (- 120d0 hue) #.(float 1/60 1d0)))
				   max
				   min))
	    ((= 2 h-prime) (values min
				   max
				   (+ min (* delta (- hue 120d0) #.(float 1/60 1d0)))))
	    ((= 3 h-prime) (values min
				   (+ min (* delta (- 240d0 hue) #.(float 1/60 1d0)))
				   max))
	    ((= 4 h-prime) (values (+ min (* delta (- hue 240d0) #.(float 1/60 1d0)))
				   min
				   max))
	    ((= 5 h-prime) (values max
				   min
				   (+ min (* delta (- 360d0 hue) #.(float 1/60 1d0)))))))))
 

(declaim (inline hsl-to-qrgb))
(defun hsl-to-qrgb (hue sat lum &key (rgbspace +srgb+) (clamp t))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-qrgb
    (hsl-to-rgb hue sat lum)
    :rgbspace rgbspace
    :clamp clamp))

(declaim (inline hsl-to-xyz))
(defun hsl-to-xyz (hue sat lum &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-xyz
    (hsl-to-rgb hue sat lum)
    rgbspace))

(declaim (inline rgb-to-hsl))
(defun rgb-to-hsl (r g b)
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (r g b)
    (let ((min (min r g b))
	  (max (max r g b)))
      (let ((hue (cond ((= min max) 0d0)
		       ((= min b) (+ 60d0 (* 60d0 (/ (- g r) (- max min)))))
		       ((= min r) (+ 180d0 (* 60d0 (/ (- b g) (- max min)))))
		       ((= min g) (+ 300d0 (* 60d0 (/ (- r b) (- max min))))))))
	(values hue
		(let ((denom (- 1d0 (abs (+ max min -1d0)))))
		  (if (zerop denom)
		      0d0
		      (/ (- max min) denom)))
		(* 0.5d0 (+ max min)))))))
	  

(declaim (inline qrgb-to-hsl))
(defun qrgb-to-hsl (qr qg qb &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-hsl
    (qrgb-to-rgb qr qg qb rgbspace)))


(declaim (inline xyz-to-hsl))
(defun xyz-to-hsl (x y z &optional (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'rgb-to-hsl
    (xyz-to-rgb x y z rgbspace)))

