(in-package :dufy)

;;;
;;; Quotization model (currently not used)
;;;

(defstruct (quantization (:constructor $make-quantization))
  "Definition of quantization method of RGB"
  (normal t :type boolean) ;; t, if the range of dequantized value is [0, 1].
  (bit-per-channel 8 :type (integer 1 #.(floor (log most-positive-fixnum 2))))
  (quantizer #'(lambda (x) (round (* 255d0 x))) :type function)
  (dequantizer #'(lambda (n) (* n #.(/ 255d0))) :type function)
  ;; nominal range of gamma-corrected values
  (nominal-min 0d0 :type double-float)
  (nominal-max 1d0 :type double-float)
  (quantized-max 255 :type (integer 0 #.most-positive-fixnum)))

(defun make-quantization (&optional (bit-per-channel 8)
			    (min 0d0)
			    (max 1d0)
			    (quantizer nil)
			    (dequantizer nil))
  (let* ((quantized-max (- (expt 2 bit-per-channel) 1))
	 (quantized-max-float (float quantized-max 1d0))
	 (/quantized-max-float (/ quantized-max-float))
	 (normal (if (and (= min 0d0) (max 1d0)) t nil))
	 (len (- max min)))
    ($make-quantization
     :normal normal
     :bit-per-channel bit-per-channel
     :quantized-max quantized-max
     :nominal-min (float min 1d0)
     :nominal-max (float max 1d0)
     :quantizer (or quantizer
		    (if normal
			#'(lambda (x) (round (* quantized-max-float x)))
			#'(lambda (x) (round (lerp (/ (- x min) len)
						   0 quantized-max-float)))))
     :dequantizer (or dequantizer
		      #'(lambda (n) (+ min (* len n /quantized-max-float)))))))

;; Standard quantization methods  [0, 1] and {0, 1, ..., 2^bit-per-channel -1}
(defparameter +qtz-8bit-normal+ (make-quantization 8)
  "Quantization between [0, 1] and {0, 1, ..., #xFF}")
(defparameter +qtz-12bit-normal+ (make-quantization 12)
  "Quantization between [0, 1] and {0, 1, ..., #xFFF}")
(defparameter +qtz-16bit-normal+ (make-quantization 16)
  "Quantization between [0, 1] and {0, 1, ..., #xFFFF}")

(defparameter +qtz-16bit-scrgb+
  (make-quantization 16 -0.5d0 7.4999d0
		     #'(lambda (x) (round (+ (* 8192d0 x) 4096d0)))
		     #'(lambda (n) (* (- n 4096) #.(/ 8192d0))))
  "Quantization method in scRGB, IEC 61966-2-2:2003
http://www.color.org/chardata/rgb/scrgb.xalter")


;;;
;;; RGB Color Space
;;;

(defun gen-linearizer (gamma)
  (let ((gamma$ (float gamma 1d0)))
    #'(lambda (x) (if (>= x 0)
		      (expt x gamma$)
		      (- (expt (- x) gamma$))))))

(defun gen-delinearizer (gamma)
  (let ((gamma-recipro (/ 1d0 (float gamma 1d0))))
    #'(lambda (x) (if (>= x 0)
		      (expt x gamma-recipro)
		      (- (expt (- x) gamma-recipro))))))

(defstruct (rgbspace (:constructor $make-rgbspace)
		     (:copier nil))
  "Structure of RGB space, including encoding characteristics"
  ;; primary coordinates
  (xr 0d0 :type double-float) (yr 0d0 :type double-float)
  (xg 0d0 :type double-float) (yg 0d0 :type double-float)
  (xb 0d0 :type double-float) (yb 0d0 :type double-float)
  
  (illuminant +illum-d65+ :type illuminant)
  (to-xyz-matrix +identity-matrix+ :type (simple-array double-float (3 3)))
  (from-xyz-matrix +identity-matrix+ :type (simple-array double-float (3 3)))

  ;; nominal range of linear values
  (lmin 0d0 :type double-float)
  (lmax 1d0 :type double-float)
  (linearizer #'identity :type function)
  (delinearizer #'identity :type function)

  ;; nominal range of gamma-corrected values
  (min 0d0 :type double-float)
  (max 1d0 :type double-float)
  (normal t :type boolean) ; t, if min = 0d0 and max = 1d0

  ;; quantization
  (bit-per-channel 8 :type (integer 1 #.(floor (log most-positive-fixnum 2))))
  (qmax 255 :type (integer 1 #.most-positive-fixnum)) ; maximum of quantized values
  (quantizer #'(lambda (x) (round (* 255d0 x))) :type function)
  (dequantizer #'(lambda (n) (* n #.(/ 255d0))) :type function))




(defun make-rgbspace (xr yr xg yg xb yb &key (illuminant +illum-d65+) (lmin 0d0) (lmax 1d0) (linearizer #'identity) (delinearizer #'identity) (bit-per-channel 8) (quantizer nil) (dequantizer nil) (force-normal nil))
  (let ((coordinates
	 (make-array '(3 3)
		     :element-type 'double-float
		     :initial-contents (list (list xr xg xb)
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
			  :initial-contents (list (list (* sr (aref coordinates 0 0))
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
	     (/qmax-float (/ qmax-float))
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
			:normal normal
			:bit-per-channel bit-per-channel
			:qmax qmax
			:quantizer (or quantizer
				       (if normal
					   #'(lambda (x) (round (* qmax-float x)))
					   #'(lambda (x) (round (lerp (/ (- x min) len)
								      0 qmax-float)))))
			:dequantizer (or dequantizer
					 #'(lambda (n) (+ min (* len n /qmax-float)))))))))


(defun linearize-srgb (x)
  "actually the same as bg-sRGB"
  (cond ((> x #.(* 0.0031308d0 12.92d0))
	 (expt (/ (+ 0.055d0 x) 1.055d0) 2.4d0))
	((< x #.(* -0.0031308d0 12.92d0))
	 (- (expt (/ (- 0.055d0 x) 1.055d0) 2.4d0)))
	(t (* x #.(/ 12.92d0)))))

(defun delinearize-srgb (x)
  "actually the same as bg-sRGB"
  (cond ((> x 0.0031308d0)
	 (+ (* 1.055d0 (expt x #.(/ 1 2.4d0))) -0.055d0))
	((< x -0.0031308d0)
	 (+ (* -1.055d0 (expt (- x) #.(/ 1 2.4d0))) 0.055d0))
	(t (* x 12.92d0))))

(defun linearize-scrgb-nl (x)
  (cond ((> x #.(* 4.5d0 0.018d0))
	 (expt (/ (+ 0.099d0 x) 1.099d0) #.(/ 0.45d0)))
	((< x (* 4.5d0 -0.018d0))
	 (- (expt (/ (- 0.099d0 x) 1.099d0) #.(/ 0.45d0))))
	(t (* x #.(/ 4.5d0)))))

(defun delinearize-scrgb-nl (x)
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
  "sRGB")

(defparameter +bg-srgb-10+
  (make-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:linearizer #'linearize-srgb				
		:delinearizer #'delinearize-srgb
		:lmin -0.53d0
		:lmax 1.68d0
		:bit-per-channel 10)
  "bg-sRGB, 10-bit per channel
http://www.color.org/chardata/rgb/bgsrgb.xalter")

(defparameter +scrgb-16+
  (make-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:lmin -0.5d0
		:lmax 7.4999d0
		:bit-per-channel 16)
  "scRGB, IEC 61966-2-2:2003
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

		

;; (defun linearize-adobe (x)
;;   (clamp (if (<= x 0.0556d0)
;; 	     (* x #.(float 1/32 1d0))
;; 	     (expt x 2.2d0))
;; 	 0d0 1d0))

;; (defun delinearize-adobe (x)
;;   (clamp (if (<= x 0.00174d0)
;; 	     (* x 32d0)
;; 	     (expt x #.(/ 1 2.2d0)))
;; 	 0d0 1d0))

(defparameter +adobe+
  (make-rgbspace 0.64d0 0.33d0 0.21d0 0.71d0 0.15d0 0.06d0
		:linearizer (gen-linearizer #.(float 563/256 1d0))
		:delinearizer (gen-delinearizer #.(float 563/256 1d0)))
  "Adobe RGB (1998), 8-bit per channel")

(defparameter +ntsc1953+
  (make-rgbspace 0.67d0 0.33d0 0.21d0 0.71d0 0.14d0 0.08d0
		:illuminant +illum-c+
		:linearizer (gen-linearizer 2.2d0)
		:delinearizer (gen-delinearizer 2.2d0)))

(defparameter +pal/secam+
  (make-rgbspace 0.64d0 0.33d0 0.29d0 0.60d0 0.15d0 0.06d0
		:linearizer (gen-linearizer 2.8d0)
		:delinearizer (gen-delinearizer 2.8d0)))

(defun linearize-prophoto (x)
  (cond ((> x #.(* 1/512 16d0))
	 (expt x 1.8d0))
	((< x #.(* -1/512 16d0))
	 (- (expt (- x) 1.8d0)))
	(t (* x #.(float 1/16 1d0)))))
  
(defun delinearize-prophoto (x)
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
 

(defun xyz-to-lrgb (x y z &key (rgbspace +srgb+) (threshold 1d-4))
  "Returns multiple values: (LR LG LB), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of LR, LG and LB is outside
the interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX + THRESHOLD]."
  (let ((inf (- (rgbspace-lmin rgbspace) threshold))
	(sup (+ (rgbspace-lmax rgbspace) threshold)))
    (multiple-value-bind (lr lg lb)
	(multiply-mat-vec (rgbspace-from-xyz-matrix rgbspace)
			  x y z)
      (values lr lg lb
	      (not (and  (<= inf lr sup)
			 (<= inf lg sup)
			 (<= inf lb sup)))))))

(defun lrgb-to-xyz (lr lg lb &optional (rgbspace +srgb+))
  (multiply-mat-vec (rgbspace-to-xyz-matrix rgbspace)
		    lr lg lb))		       

(defun copy-rgbspace (rgbspace &key (illuminant nil) (bit-per-channel nil))
  "Returns a new RGBSPACE with different standard illuminant and/or
bit-per-channel. All the parameters are properly recalculated. If both
are nil, it is just a copier."
  (destructuring-bind (new-xr new-yr new-xg new-yg new-xb new-yb)
      (if illuminant
	  (let ((ca-func (gen-cat-function (rgbspace-illuminant rgbspace) illuminant)))
	    (labels ((get-new-xy (r g b)
		       (subseq (apply #'xyz-to-xyy
				      (apply ca-func
					     (lrgb-to-xyz r g b rgbspace)))
			       0 2)))
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

;; (defparameter +srgbd50+
;;   (copy-rgbspace +srgb+ :illuminant +illum-d50+))

;; (defparameter +adobed50+
;;   (copy-rgbspace +adobe+ :illuminant +illum-d50+))


(defparameter +adobe-16+
  (copy-rgbspace +adobe+ :bit-per-channel 16)
  "Adobe RGB (1998), 16-bit per channel.")

(defparameter +bg-srgb-12+
  (copy-rgbspace +bg-srgb-10+ :bit-per-channel 12)
  "bg-sRGB, 12-bit per channel,
http://www.color.org/chardata/rgb/bgsrgb.xalter")

(defparameter +bg-srgb-16+
  (copy-rgbspace +bg-srgb-10+ :bit-per-channel 16)
  "bg-sRGB, 16-bit per channel,
http://www.color.org/chardata/rgb/bgsrgb.xalter")

(defparameter +prophoto-12+
  (copy-rgbspace +prophoto+ :bit-per-channel 12)
  "Prophoto RGB (also known as ROMM RGB), 12-bit per channel,
http://www.color.org/ROMMRGB.pdf")

(defparameter +prophoto-16+
  (copy-rgbspace +prophoto+ :bit-per-channel 16)
  "Prophoto RGB (also known as ROMM RGB), 16-bit per channel,
http://www.color.org/ROMMRGB.pdf")

(defun linearize (x &optional (rgbspace +srgb+))
  (funcall (rgbspace-linearizer rgbspace) x))

(defun delinearize (x &optional (rgbspace +srgb+))
  (funcall (rgbspace-delinearizer rgbspace) x))

(defun lrgb-to-rgb (lr lg lb &optional (rgbspace +srgb+))
  (let ((delin (rgbspace-delinearizer rgbspace)))
    (values (funcall delin lr)
	    (funcall delin lg)
	    (funcall delin lb))))

(defun rgb-to-lrgb (r g b &optional (rgbspace +srgb+))
  (let ((lin (rgbspace-linearizer rgbspace)))
    (values (funcall lin r)
	    (funcall lin g)
	    (funcall lin b))))

(defun xyz-to-rgb (x y z &key (rgbspace +srgb+) (threshold 1d-4))
  "Returns multiple values: (R G B), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX +
THRESHOLD]."
  (multiple-value-bind (lr lg lb out-of-gamut)
      (xyz-to-lrgb x y z :rgbspace rgbspace :threshold threshold)
    (multiple-value-bind (r g b)
	(lrgb-to-rgb lr lg lb rgbspace)
      (values r g b out-of-gamut))))

(defun rgb-to-xyz (r g b &optional (rgbspace +srgb+))
  (multiple-value-call #'lrgb-to-xyz
    (rgb-to-lrgb r g b rgbspace)
    rgbspace))


(defun rgb-to-qrgb (r g b &key (rgbspace +srgb+) (clamp nil))
  "Quantizes RGB values from [RGBSPACE-MIN, RGBSPACE-MAX] ([0, 1], typically) to {0, 1,
..., RGBSPACE-QMAX} ({0, 1, ..., 255}, typically), though it accepts
all the real values."
  (let ((quantizer (compose (the single-valued-function
				 (if clamp
				     (rcurry #'clamp 0d0 (rgbspace-qmax rgbspace))
				     #'identity))
			    (rgbspace-quantizer rgbspace))))
    (values (funcall quantizer r) 
	    (funcall quantizer g)
	    (funcall quantizer b))))

(defun qrgb-to-rgb (qr qg qb &optional (rgbspace +srgb+))
  (let ((dequantizer (rgbspace-dequantizer rgbspace)))
    (values (funcall dequantizer qr)
	    (funcall dequantizer qg)
	    (funcall dequantizer qb))))

(defun xyz-to-qrgb (x y z &key (rgbspace +srgb+) (threshold 1d-4) (clamp nil))
  "Returns multiple values: (QR QG QB), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX +
THRESHOLD]."
  (multiple-value-bind (r g b out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (multiple-value-bind (qr qg qb)
	(rgb-to-qrgb r g b :rgbspace rgbspace :clamp clamp)
      (values qr qg qb out-of-gamut))))

(defun qrgb-to-xyz (qr qg qb &optional (rgbspace +srgb+))
  (multiple-value-call #'rgb-to-xyz
    (qrgb-to-rgb qr qg qb rgbspace)
    rgbspace))


(defun qrgb-to-hex (qr qg qb &optional (rgbspace +srgb+))
  (let ((bpc (rgbspace-bit-per-channel rgbspace))
	(qmax (rgbspace-qmax rgbspace)))
    (+ (ash (clamp qr 0 qmax) (+ bpc bpc))
       (ash (clamp qg 0 qmax) bpc)
       (clamp qb 0 qmax))))

(defun hex-to-qrgb (hex &optional (rgbspace +srgb+))
  (let ((minus-bpc (- (rgbspace-bit-per-channel rgbspace)))
	(qmax (rgbspace-qmax rgbspace)))
    (values (logand (ash hex (+ minus-bpc minus-bpc)) qmax)
	    (logand (ash hex minus-bpc) qmax)
	    (logand hex qmax))))

;; (defun hex-to-rgb (hex)
;;   (apply #'qrgb-to-rgb
;; 	 (hex-to-qrgb hex)))

;; (defun rgb-to-hex (r g b)
;;   (apply #'qrgb-to-hex
;; 	 (rgb-to-qrgb r g b)))

(defun hex-to-xyz (hex &optional (rgbspace +srgb+))
  (multiple-value-call #'qrgb-to-xyz
    (hex-to-qrgb hex rgbspace)
    rgbspace))

(defun xyz-to-hex (x y z &key (rgbspace +srgb+) (threshold 1d-4))
  "Returns multiple values: HEX, OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX +
THRESHOLD]."
  (multiple-value-bind (qr qg qb out-of-gamut)
      (xyz-to-qrgb x y z :rgbspace rgbspace :threshold threshold)
    (values (qrgb-to-hex qr qg qb rgbspace)
	    out-of-gamut)))

;;;
;;; L*a*b*, L*u*v*, LCh
;;;

(declaim (inline function-f)
	 (ftype (function * double-float) function-f))
(defun function-f (x)
  (declare (optimize (speed 3) (safety 0))
	   (double-float x))
  (if (> x #.(float 216/24389 1d0))
      (expt x #.(float 1/3 1d0))
      (+ (* #.(/ 24389/27 116d0) x) #.(float 16/116 1d0))))

(defun xyz-to-lab (x y z &optional (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((fx (function-f (/ (float x 1d0) (illuminant-x illuminant))))
	(fy (function-f (float y 1d0)))
	(fz (function-f (/ (float z 1d0) (illuminant-z illuminant)))))
    (values (- (* 116d0 fy) 16d0)
	    (* 500d0 (- fx fy))
	    (* 200d0 (- fy fz)))))

(defun xyy-to-lab (small-x small-y y &optional (illuminant +illum-d65+))
  (multiple-value-bind (new-x new-y new-z) (xyy-to-xyz small-x small-y y)
    (xyz-to-lab new-x new-y new-z illuminant)))

(defun lab-to-xyz (lstar astar bstar &optional (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1))
	   (real lstar astar bstar))
  (let* ((fy (* (+ lstar 16d0) #.(float 1/116 1d0)))
	 (fx (+ fy (* astar 0.002d0)))
	 (fz (- fy (* bstar 0.005d0))))
    (values (if (> fx #.(float 6/29 1d0))
		(* (illuminant-x illuminant) fx fx fx)
		(* (- fx #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29) (illuminant-x illuminant)))
	    (if (> fy #.(float 6/29 1d0))
		(* (illuminant-y illuminant) fy fy fy)
		(* (- fy #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29) (illuminant-y illuminant)))
	    (if (> fz #.(float 6/29 1d0))
		(* (illuminant-z illuminant) fz fz fz)
		(* (- fz #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29) (illuminant-z illuminant))))))

(defun lstar-to-y (lstar)
  (declare (optimize (speed 3) (safety 1))
	   (real lstar))
  (let* ((fy (* (+ lstar 16d0) #.(float 1/116 1d0))))
    (if (> fy #.(float 6/29 1d0))
	(* fy fy fy)
	(* (- fy #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29)))))

(defun y-to-lstar (y)
  (declare (optimize (speed 3) (safety 1))
	   (real y))
  (- (* 116d0 (function-f (float y 1d0))) 16d0))
 
(defun lab-to-xyy (lstar astar bstar &optional (illuminant +illum-d65+))
  (multiple-value-call #'xyz-to-xyy
    (lab-to-xyz lstar astar bstar illuminant)))

(define-constant CONST-TWO-PI/360 (/ TWO-PI 360))
(define-constant CONST-360/TWO-PI (/ 360 TWO-PI))

(defun lab-to-lchab (lstar astar bstar)
  (values lstar
	  (sqrt (+ (* astar astar) (* bstar bstar)))
	  (mod (* (atan bstar astar) CONST-360/TWO-PI) 360d0)))

(defun lchab-to-lab (lstar cstarab hab)
  (let ((hue-two-pi (* hab CONST-TWO-PI/360)))
    (values lstar
	    (* cstarab (cos hue-two-pi))
	    (* cstarab (sin hue-two-pi)))))

(defun xyz-to-lchab (x y z &optional (illuminant +illum-d65+))
  (multiple-value-call #'lab-to-lchab (xyz-to-lab x y z illuminant)))

(defun xyy-to-lchab (small-x small-y y &optional (illuminant +illum-d65+))
  (multiple-value-call #'lab-to-lchab (xyy-to-lab small-x small-y y illuminant)))

(defun lchab-to-xyz (lstar cstarab hab &optional (illuminant +illum-d65+))
  (multiple-value-call #'lab-to-xyz
      (lchab-to-lab lstar cstarab hab)
      illuminant))

(defun lchab-to-xyy (lstar cstarab hab &optional (illuminant +illum-d65+))
  (multiple-value-call #'xyz-to-xyy
    (lchab-to-xyz lstar cstarab hab illuminant)) )


(declaim (ftype (function (double-float double-float) (values double-float double-float)) calc-uvprime))
(defun calc-uvprime (x y)
  (declare (optimize (speed 3) (safety 0)))
  (let ((denom (+ (* -2d0 x) (* 12d0 y) 3d0)))
    (values (/ (* 4d0 x) denom)
	    (/ (* 9d0 y) denom))))

(declaim (ftype (function (double-float double-float double-float) (values double-float double-float)) calc-uvprime-from-xyz))
(defun calc-uvprime-from-xyz (x y z)
  (declare (optimize (speed 3) (safety 0)))
  (let ((denom (+ x (* 15d0 y) (* 3d0 z))))
    (values (/ (* 4d0 x) denom)
	    (/ (* 9d0 y) denom))))

(defun xyz-to-luv (x y z &optional (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((x (float x 1d0)) (y (float y 1d0)) (z (float z 1d0)))
    (multiple-value-bind (uprime vprime)
	(calc-uvprime-from-xyz x y z)
      (multiple-value-bind (urprime vrprime)
	  (calc-uvprime (illuminant-small-x illuminant) (illuminant-small-y illuminant))
	(let* ((yr (/ y (illuminant-y illuminant)))
	       (lstar (if (> yr #.(expt 6/29 3d0))
			  (- (* 116d0 (expt yr #.(float 1/3 1d0))) 16d0)
			  (* #.(expt 29/3 3d0) yr))))
	  (values lstar
		  (* 13d0 lstar (- uprime urprime))
		  (* 13d0 lstar (- vprime vrprime))))))))

(defun luv-to-xyz (lstar ustar vstar &optional (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((lstar (float lstar 1d0)) (ustar (float ustar 1d0)) (vstar (float vstar 1d0)))
    (multiple-value-bind (urprime vrprime)
	(calc-uvprime (illuminant-small-x illuminant) (illuminant-small-y illuminant))
      (let* ((uprime (+ (/ ustar (* 13d0 lstar)) urprime))
	     (vprime (+ (/ vstar (* 13d0 lstar)) vrprime))
	     (l (/ (+ lstar 16d0) 116d0))
	     (y (if (<= lstar 8d0)
		    (* (illuminant-y illuminant)
		       lstar
		       (expt 3/29 3d0))
		    (* (illuminant-y illuminant)
		       (* l l l)))))
	(values (* y (/ (* 9d0 uprime) (* 4d0 vprime)))
		y
		(* y (/ (- 12d0 (* 3d0 uprime) (* 20d0 vprime)) (* 4d0 vprime))))))))
	    
(defun luv-to-lchuv (lstar ustar vstar)
  (values lstar
	  (sqrt (+ (* ustar ustar) (* vstar vstar)))
	  (mod (* (atan vstar ustar) CONST-360/TWO-PI) 360d0)))

(defun lchuv-to-luv (lstar cstaruv huv)
  (let ((hue-two-pi (* huv CONST-TWO-PI/360)))
    (values lstar (* cstaruv (cos hue-two-pi)) (* cstaruv (sin hue-two-pi)))))

(defun xyz-to-lchuv (x y z &optional (illuminant +illum-d65+))
  (multiple-value-call #'luv-to-lchuv (xyz-to-luv x y z illuminant)))

(defun lchuv-to-xyz (lstar cstaruv huv &optional (illuminant +illum-d65+))
  (multiple-value-call #'luv-to-xyz
    (lchuv-to-luv lstar cstaruv huv)
    illuminant))


;;;
;;; HSV/HSL
;;;


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
      (cond ((= sat 0d0) (list base base base))
	    ((= 0 h-prime-int) (list (+ base c) (+ base x) base))
	    ((= 1 h-prime-int) (list (+ base x) (+ base c) base))
	    ((= 2 h-prime-int) (list base (+ base c) (+ base x)))
	    ((= 3 h-prime-int) (list base (+ base x) (+ base c)))
	    ((= 4 h-prime-int) (list (+ base x) base (+ base c)))
	    ((= 5 h-prime-int) (list (+ base c) base (+ base x)))))))
	 
(defun hsv-to-qrgb (hue sat val &optional (rgbspace +srgb+))
  (apply (rcurry #'rgb-to-qrgb rgbspace)
	 (hsv-to-rgb hue sat val)))

(defun hsv-to-xyz (hue sat val &optional (rgbspace +srgb+))
  (apply (rcurry #'rgb-to-xyz rgbspace)
	 (hsv-to-rgb hue sat val)))

(defun rgb-to-hsv (r g b)
  (let* ((maxrgb (coerce (max r g b) 'double-float))
	 (minrgb (coerce (min r g b) 'double-float))
	 (s (if (= maxrgb 0)
		0d0
		(/ (- maxrgb minrgb) maxrgb)))
	 (h (cond ((= minrgb maxrgb) 0d0)
		  ((= minrgb b) (+ (* 60d0 (/ (- g r) (- maxrgb minrgb))) 60d0))
		  ((= minrgb r) (+ (* 60d0 (/ (- b g) (- maxrgb minrgb))) 180d0))
		  ((= minrgb g) (+ (* 60d0 (/ (- r b) (- maxrgb minrgb))) 300d0)))))
    (list h s maxrgb)))
	 
(defun qrgb-to-hsv (qr qg qb &optional (rgbspace +srgb+))
  (apply #'rgb-to-hsv
	 (qrgb-to-rgb qr qg qb rgbspace)))

(defun xyz-to-hsv (x y z &key (rgbspace +srgb+) (threshold 1d-4))
  "Returns multiple values: (H S V), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of **linear** RGB values are
outside the interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX +
THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-hsv rgb)
	    out-of-gamut)))
  

(defun hsl-to-rgb (hue sat lum)
  "HUE is in the circle group R/360. The nominal range of SAT and VAL is [0,
1]; all the real values outside the interval are also acceptable."
  (let* ((tmp (* 0.5d0 sat (- 1d0 (abs (- (* lum 2d0) 1d0)))))
	 (max (+ lum tmp))
	 (min (- lum tmp))
	 (delta (- max min))
	 (h-prime (* (mod hue 360d0) #.(float 1/60 1d0)))
	 (h-prime-int (floor h-prime)))
    (cond ((= sat 0d0) (list max max max))
	  ((= 0 h-prime-int) (list max
				   (+ min (* delta hue #.(float 1/60 1d0)))
				   min))
	  ((= 1 h-prime-int) (list (+ min (* delta (- 120d0 hue) #.(float 1/60 1d0)))
				   max
				   min))
	  ((= 2 h-prime-int) (list min
				   max
				   (+ min (* delta (- hue 120d0) #.(float 1/60 1d0)))))
	  ((= 3 h-prime-int) (list min
				   (+ min (* delta (- 240d0 hue) #.(float 1/60 1d0)))
				   max))
	  ((= 4 h-prime-int) (list (+ min (* delta (- hue 240d0) #.(float 1/60 1d0)))
				   min
				   max))
	  ((= 5 h-prime-int) (list max
				   min
				   (+ min (* delta (- 360d0 hue) #.(float 1/60 1d0))))))))
 

(defun hsl-to-qrgb (hue sat lum &optional (rgbspace +srgb+))
  (apply (rcurry #'rgb-to-qrgb rgbspace)
	 (hsl-to-rgb hue sat lum)))

(defun hsl-to-xyz (hue sat lum &optional (rgbspace +srgb+))
  (apply (rcurry #'rgb-to-xyz rgbspace)
	 (hsl-to-rgb hue sat lum)))

(defun rgb-to-hsl (r g b)
  (let ((min (min r g b))
	(max (max r g b)))
    (let ((hue (cond ((= min max) 0d0)
		     ((= min b) (+ 60d0 (* 60d0 (/ (- g r) (- max min)))))
		     ((= min r) (+ 180d0 (* 60d0 (/ (- b g) (- max min)))))
		     ((= min g) (+ 300d0 (* 60d0 (/ (- r b) (- max min))))))))
      (list hue
	    (let ((denom (- 1d0 (abs (+ max min -1d0)))))
	      (if (zerop denom)
		  0d0
		  (/ (- max min) denom)))
	    (* 0.5d0 (+ max min))))))
	  

(defun qrgb-to-hsl (qr qg qb &optional (rgbspace +srgb+))
  (apply #'rgb-to-hsl
	 (qrgb-to-rgb qr qg qb rgbspace)))


(defun xyz-to-hsl (x y z &key (rgbspace +srgb+) (threshold 1d-4))
    "Returns multiple values: (H S L), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX +
THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-hsl rgb)
	    out-of-gamut)))
