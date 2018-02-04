(in-package :dufy)

;;;
;;; Quotization model
;;;

(defstruct (quantization (:constructor $make-quantization))
  "Definition of quantization method of RGB"
  (normal t :type boolean) ;; t, if the range of dequantized value is [0, 1].
  (bit-per-channel 8 :type (integer 1 #.(floor (log most-positive-fixnum 2))))
  (quantizer #'(lambda (x) (round (* 255d0 x))) :type function)
  (dequantizer #'(lambda (n) (* n #.(/ 255d0))) :type function)
  (nominal-min 0d0 :type double-float)
  (nominal-max 1d0 :type double-float)
  (quantized-max 255 :type (integer 1 #.most-positive-fixnum)))

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
		      #'(lambda (n) (+ min (* n /quantized-max-float)))))))

;; Standard quantization methods between [0, 1] and {0, 1, ..., 2^bit-per-channel -1}
(defparameter qtz-8bit-normal (make-quantization 8))
(defparameter qtz-12bit-normal (make-quantization 16))
(defparameter qtz-16bit-normal (make-quantization 16))

(defparameter qtz-16bit-scrgb
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
  (xr 0d0 :type double-float) (yr 0d0 :type double-float)
  (xg 0d0 :type double-float) (yg 0d0 :type double-float)
  (xb 0d0 :type double-float) (yb 0d0 :type double-float)
  (illuminant illum-d65)
  (quantization qtz-8bit-normal)
  (linearizer #'identity)
  (delinearizer #'identity)
  (to-xyz-matrix identity-matrix :type (simple-array double-float (3 3)))
  (from-xyz-matrix identity-matrix :type (simple-array double-float (3 3))))



(defun make-rgbspace (xr yr xg yg xb yb &key (illuminant illum-d65) (quantization qtz-8bit-normal) (linearizer #'identity) (delinearizer #'identity))
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
	($make-rgbspace :xr xr :yr yr :xg xg :yg yg :xb xb :yb yb
			:illuminant illuminant
			:quantization quantization
			:linearizer linearizer
			:delinearizer delinearizer
			:to-xyz-matrix m
			:from-xyz-matrix (invert-matrix33 m))))))


(defun srgb-linearizer (x)
  "actually the same as bg-sRGB"
  (cond ((> x #.(* 0.0031308d0 12.92d0))
	 (expt (/ (+ 0.055d0 x) 1.055d0) 2.4d0))
	((< x #.(* -0.0031308d0 12.92d0))
	 (- (expt (/ (- 0.055d0 x) 1.055d0) 2.4d0)))
	(t (* x #.(/ 12.92d0)))))

(defun srgb-delinearizer (x)
  "actually the same as bg-sRGB"
  (cond ((> x 0.0031308d0)
	 (+ (* 1.055d0 (expt x #.(/ 1 2.4d0))) -0.055d0))
	((< x -0.0031308d0)
	 (+ (* -1.055d0 (expt (- x) #.(/ 1 2.4d0))) 0.055d0))
	(t (* x 12.92d0))))

(defparameter srgb
  (make-rgbspace 0.64d0 0.33d0  0.30d0 0.60d0 0.15d0 0.06d0
		:linearizer #'srgb-linearizer				
		:delinearizer #'srgb-delinearizer))

(defparameter srgbd65 srgb)

;; (defun adobe-linearizer (x)
;;   (clamp (if (<= x 0.0556d0)
;; 	     (* x #.(float 1/32 1d0))
;; 	     (expt x 2.2d0))
;; 	 0d0 1d0))

;; (defun adobe-delinearizer (x)
;;   (clamp (if (<= x 0.00174d0)
;; 	     (* x 32d0)
;; 	     (expt x #.(/ 1 2.2d0)))
;; 	 0d0 1d0))

(defparameter adobe
  (make-rgbspace 0.64d0 0.33d0 0.21d0 0.71d0 0.15d0 0.06d0
		:linearizer (gen-linearizer #.(float 563/256 1d0))
		:delinearizer (gen-delinearizer #.(float 563/256 1d0))))

(defparameter adobed65 adobe)


(defparameter ntsc1953
  (make-rgbspace 0.67d0 0.33d0 0.21d0 0.71d0 0.14d0 0.08d0
		:illuminant illum-c
		:linearizer (gen-linearizer 2.2d0)
		:delinearizer (gen-delinearizer 2.2d0)))

(defparameter pal/secam
  (make-rgbspace 0.64d0 0.33d0 0.29d0 0.60d0 0.15d0 0.06d0
		:linearizer (gen-linearizer 2.8d0)
		:delinearizer (gen-delinearizer 2.8d0)))

(defun prophoto-linearizer (x)
  (cond ((> x #.(* 1/512 16d0))
	 (expt x 1.8d0))
	((< x #.(* -1/512 16d0))
	 (- (expt (- x) 1.8d0)))
	(t (* x #.(float 1/16 1d0)))))
  
(defun prophoto-delinearizer (x)
  (cond ((> x #.(float 1/512 1d0))
	 (expt x #.(/ 1.8d0)))
	((< x #.(float -1/512 1d0))
	 (- (expt (- x) #.(/ 1.8d0))))
	(t (* x 16d0))))
	    
(defparameter prophoto
  (make-rgbspace 0.7347d0 0.2653d0 0.1596d0 0.8404d0 0.0366d0 0.0001d0
		:illuminant illum-d50
		:linearizer #'prophoto-linearizer
		:delinearizer #'prophoto-delinearizer))		      
 

;; convert XYZ to linear RGB in [0, 1]
(defun xyz-to-lrgb (x y z &key (rgbspace srgb) (threshold 1d-4))
  "Returns multiple values: (LR LG LB), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of LR, LG and LB is outside
the interval [-THRESHOLD, 1+THRESHOLD]."
  (destructuring-bind (lr lg lb)
      (multiply-matrix-and-vec (rgbspace-from-xyz-matrix rgbspace)
				x y z)
    (let ((out-of-gamut (not (and  (nearly<= threshold 0 lr 1)
				   (nearly<= threshold 0 lg 1)
				   (nearly<= threshold 0 lb 1)))))
      (values (list lr lg lb) out-of-gamut))))

(defun lrgb-to-xyz (lr lg lb &optional (rgbspace srgb))
  (multiply-matrix-and-vec (rgbspace-to-xyz-matrix rgbspace)
			    lr lg lb))		       

(defun copy-rgbspace (rgbspace &optional (illuminant nil))
  "Copies RGBSPACE with different standard illuminant. All the
parameters are properly recalculated. If ILLLUMINANT is nil, it is
just a copier."
  (if illuminant
      (let ((ca-func (gen-cat-function (rgbspace-illuminant rgbspace) illuminant)))
	(destructuring-bind (new-xr new-yr zr)
	    (apply #'xyz-to-xyy
		   (apply ca-func
			  (lrgb-to-xyz 1 0 0 rgbspace)))
	  (declare (ignore zr))
	  (destructuring-bind (new-xg new-yg zg)
	      (apply #'xyz-to-xyy
		     (apply ca-func
			    (lrgb-to-xyz 0 1 0 rgbspace)))
	    (declare (ignore zg))
	    (destructuring-bind (new-xb new-yb zb)
		(apply #'xyz-to-xyy
		       (apply ca-func
			      (lrgb-to-xyz 0 0 1 rgbspace)))
	      (declare (ignore zb))
	      (make-rgbspace new-xr new-yr new-xg new-yg new-xb new-yb
			     :illuminant illuminant
			     :quantization (rgbspace-quantization rgbspace )
			     :linearizer (rgbspace-linearizer rgbspace)
			     :delinearizer (rgbspace-delinearizer rgbspace))))))
      (make-rgbspace (rgbspace-xr rgbspace) (rgbspace-yr rgbspace)
		     (rgbspace-xg rgbspace) (rgbspace-yg rgbspace)
		     (rgbspace-xb rgbspace) (rgbspace-yb rgbspace)
		     :illuminant (rgbspace-illuminant rgbspace)
		     :quantization (rgbspace-quantization rgbspace)
		     :linearizer (rgbspace-linearizer rgbspace)
		     :delinearizer (rgbspace-delinearizer rgbspace))))


(defparameter srgbd50
  (copy-rgbspace srgbd65 illum-d50))

(defparameter adobed50
  (copy-rgbspace adobed65 illum-d50))


(defun linearize (x &optional (rgbspace srgb))
  (funcall (rgbspace-linearizer rgbspace) x))

(defun delinearize (x &optional (rgbspace srgb))
  (funcall (rgbspace-delinearizer rgbspace) x))

(defun lrgb-to-rgb (lr lg lb &optional (rgbspace srgb))
  (let ((delin (rgbspace-delinearizer rgbspace)))
    (list (funcall delin lr)
	  (funcall delin lg)
	  (funcall delin lb))))

(defun rgb-to-lrgb (r g b &optional (rgbspace srgb))
  (let ((lin (rgbspace-linearizer rgbspace)))
    (list (funcall lin r)
	  (funcall lin g)
	  (funcall lin b))))

(defun xyz-to-rgb (x y z &key (rgbspace srgb) (threshold 1d-4))
  "Returns multiple values: (R G B), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (lrgb out-of-gamut)
      (xyz-to-lrgb x y z :rgbspace rgbspace :threshold threshold)
    (values (mapcar (rgbspace-delinearizer rgbspace) lrgb)
	    out-of-gamut)))

(defun rgb-to-xyz (r g b &optional (rgbspace srgb))
  (apply (rcurry #'lrgb-to-xyz rgbspace)
	 (rgb-to-lrgb r g b rgbspace)))


(defun rgb-to-qrgb (r g b)
  "Quantizes RGB values from [0, 1] to {0, 1, ..., 255}, though it
accepts all the real values."
  (list (round (* r 255))
	(round (* g 255))
	(round (* b 255))))

(defun qrgb-to-rgb (qr qg qb)
  (list (* qr #.(float 1/255 1d0))
	(* qg #.(float 1/255 1d0))
	(* qb #.(float 1/255 1d0))))

(defun xyz-to-qrgb (x y z &key (rgbspace srgb) (threshold 1d-4))
  "Returns multiple values: (QR QG QB), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-qrgb rgb)
	    out-of-gamut)))

;; convert RGB ({0, 1, ..., 255}) to XYZ ([0, 1])
(defun qrgb-to-xyz (qr qg qb &optional (rgbspace srgb))
  (rgb-to-xyz (* qr #.(float 1/255 1d0))
	      (* qg #.(float 1/255 1d0))
	      (* qb #.(float 1/255 1d0))
	      rgbspace))

(defun qrgb-to-hex (qr qg qb)
  (+ (ash qr 16) (ash qg 8) qb))

(defun hex-to-qrgb (hex)
  (list (logand (ash hex -16) #xff)
	(logand (ash hex -8) #xff)
	(logand hex #xff)))

(defun hex-to-rgb (hex)
  (apply #'qrgb-to-rgb
	 (hex-to-qrgb hex)))

(defun rgb-to-hex (r g b)
  (apply #'qrgb-to-hex
	 (rgb-to-qrgb r g b)))

(defun hex-to-xyz (hex &optional (rgbspace srgb))
  (apply (rcurry #'qrgb-to-xyz rgbspace)
	 (hex-to-qrgb hex)))

(defun xyz-to-hex (x y z &key (rgbspace srgb) (threshold 1d-4))
  "Returns multiple values: HEX, OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (qrgb out-of-gamut)
      (xyz-to-qrgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'qrgb-to-hex qrgb)
	    out-of-gamut)))
	 

;;;
;;; L*a*b*, L*u*v*, LCh
;;;

(declaim (inline function-f)
	 (ftype (function (double-float) double-float) function-f))
(defun function-f (x)
  (declare (optimize (speed 3) (safety 0))
	   (double-float x))
  (if (> x #.(float 216/24389 1d0))
      (expt x #.(float 1/3 1d0))
      (+ (* #.(/ 24389/27 116d0) x) #.(float 16/116 1d0))))

(defun xyz-to-lab (x y z &optional (illuminant illum-d65))
  (let ((fx (function-f (/ (float x 1d0) (illuminant-largex illuminant))))
	(fy (function-f (float y 1d0)))
	(fz (function-f (/ (float z 1d0) (illuminant-largez illuminant)))))
    (list (- (* 116d0 fy) 16d0)
	  (* 500d0 (- fx fy))
	  (* 200d0 (- fy fz)))))

(defun xyy-to-lab (x y largey &optional (illuminant illum-d65))
  (destructuring-bind (x y z) (xyy-to-xyz x y largey)
    (xyz-to-lab x y z illuminant)))

(defun lab-to-xyz (lstar astar bstar &optional (illuminant illum-d65))
  (declare (optimize (speed 3) (safety 1))
	   (real lstar astar bstar))
  (let* ((fy (* (+ lstar 16d0) #.(float 1/116 1d0)))
	 (fx (+ fy (* astar 0.002d0)))
	 (fz (- fy (* bstar 0.005d0))))
    (list (if (> fx #.(float 6/29 1d0))
	      (* (illuminant-largex illuminant) fx fx fx)
	      (* (- fx #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29) (illuminant-largex illuminant)))
	  (if (> fy #.(float 6/29 1d0))
	      (* fy fy fy)
	      (* (- fy #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29)))
	  (if (> fz #.(float 6/29 1d0))
	      (* (illuminant-largez illuminant) fz fz fz)
	      (* (- fz #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29) (illuminant-largez illuminant))))))

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
 
(defun lab-to-xyy (lstar astar bstar &optional (illuminant illum-d65))
  (destructuring-bind (x y z) (lab-to-xyz lstar astar bstar illuminant)
    (xyz-to-xyy x y z)))

(define-constant CONST-TWO-PI/360 (/ TWO-PI 360))
(define-constant CONST-360/TWO-PI (/ 360 TWO-PI))

(defun lab-to-lchab (lstar astar bstar)
  (list lstar
	(sqrt (+ (* astar astar) (* bstar bstar)))
	(mod (* (atan bstar astar) CONST-360/TWO-PI) 360d0)))

(defun lchab-to-lab (lstar cstarab hab)
  (let ((hue-two-pi (* hab CONST-TWO-PI/360)))
    (list lstar (* cstarab (cos hue-two-pi)) (* cstarab (sin hue-two-pi)))))

(defun xyz-to-lchab (x y z &optional (illuminant illum-d65))
  (apply #'lab-to-lchab (xyz-to-lab x y z illuminant)))

(defun xyy-to-lchab (x y largey &optional (illuminant illum-d65))
  (apply #'lab-to-lchab (xyy-to-lab x y largey illuminant)))

(defun lchab-to-xyz (lstar cstarab hab &optional (illuminant illum-d65))
  (destructuring-bind (l a b) (lchab-to-lab lstar cstarab hab)
    (lab-to-xyz l a b illuminant)))

(defun lchab-to-xyy (lstar cstarab hab &optional (illuminant illum-d65))
  (destructuring-bind (x y z) (lchab-to-xyz lstar cstarab hab illuminant)
    (xyz-to-xyy x y z)))

(defun qrgb-to-lab (qr qg qb &optional (rgbspace srgb))
  (apply (rcurry #'xyz-to-lab (rgbspace-illuminant rgbspace))
	 (qrgb-to-xyz qr qg qb rgbspace)))


(defun calc-uvprime (x y)
  (let ((denom (+ (* -2d0 x) (* 12d0 y) 3d0)))
    (list (/ (* 4d0 x) denom)
	  (/ (* 9d0 y) denom))))

(defun calc-uvprime-from-xyz (x y z)
  (let ((denom (+ x (* 15d0 y) (* 3d0 z))))
    (list (/ (* 4d0 x) denom)
	  (/ (* 9d0 y) denom))))

(defun xyz-to-luv (x y z &optional (illuminant illum-d65))
  (destructuring-bind (uprime vprime) (calc-uvprime-from-xyz x y z)
    (destructuring-bind (urprime vrprime) (calc-uvprime (illuminant-x illuminant) (illuminant-y illuminant))
      (let* ((yr (/ y (illuminant-largey illuminant)))
	     (lstar (if (> yr 0.008856451679035631d0)
			(- (* 116d0 (expt yr 0.3333333333333333d0)) 16d0)
			(* 903.2962962962963d0 yr))))
	(list lstar
	      (* 13d0 lstar (- uprime urprime))
	      (* 13d0 lstar (- vprime vrprime)))))))

(defun luv-to-xyz (lstar ustar vstar &optional (illuminant illum-d65))
  (destructuring-bind (urprime vrprime) (calc-uvprime (illuminant-x illuminant) (illuminant-y illuminant))
    (let* ((uprime (+ (/ ustar (* 13d0 lstar)) urprime))
	   (vprime (+ (/ vstar (* 13d0 lstar)) vrprime))
	   (l (/ (+ lstar 16d0) 116d0))
	   (y (if (<= lstar 8d0)
		  (* (illuminant-largey illuminant)
		     lstar
		     0.008856451679035631d0)
		  (* (illuminant-largey illuminant)
		     (* l l l)))))
      (list (* y (/ (* 9d0 uprime) (* 4d0 vprime)))
	    y
	    (* y (/ (- 12d0 (* 3d0 uprime) (* 20d0 vprime)) (* 4d0 vprime)))))))
	    
(defun luv-to-lchuv (lstar ustar vstar)
  (list lstar
	(sqrt (+ (* ustar ustar) (* vstar vstar)))
	(mod (* (atan vstar ustar) CONST-360/TWO-PI) 360d0)))

(defun lchuv-to-luv (lstar cstaruv huv)
  (let ((hue-two-pi (* huv CONST-TWO-PI/360)))
    (list lstar (* cstaruv (cos hue-two-pi)) (* cstaruv (sin hue-two-pi)))))

(defun xyz-to-lchuv (x y z &optional (illuminant illum-d65))
  (apply #'luv-to-lchuv (xyz-to-luv x y z illuminant)))

(defun lchuv-to-xyz (lstar cstaruv huv &optional (illuminant illum-d65))
  (destructuring-bind (l u v) (lchuv-to-luv lstar cstaruv huv)
    (luv-to-xyz l u v illuminant)))



;; obsolete
;; (defun polar-mean-of-xy (x1 y1 x2 y2)
;;   (destructuring-bind (r1 theta1) (xy-to-polar x1 y1)
;;     (destructuring-bind (r2 theta2) (xy-to-polar x2 y2)
;;       (polar-to-xy (* 0.5d0 (+ r1 r2))
;; 		   (circular-lerp theta1 theta2 0.5d0)))))

;; (defun xy-to-polar (x y)
;;   (let ((dx (- x 0.31006d0))
;; 	(dy (- y 0.31616d0)))
;;     (list (sqrt (+ (* dx dx) (* dy dy)))
;; 	  (mod (atan dy dx) TWO-PI))))

;; (defun polar-to-xy (r theta)
;;   (let ((dx (* r (cos theta)))
;; 	(dy (* r (sin theta))))
;;     (list (+ dx 0.31006d0) (+ dy 0.31616d0))))



;;;
;;; HSV/HSL
;;;


(defun hsv-to-rgb (hue sat val)
  "H is in R/360. S and V are in [0, 1]."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((hue (the (double-float 0d0 360d0) (mod (float hue 1d0) 360d0)))
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
	 
(defun hsv-to-qrgb (hue sat val)
  (mapcar #'(lambda (x) (round (* x 255d0)))
	  (hsv-to-rgb hue sat val)))

;; The given HSV color is regarded as converted from a non-linear (i.e. gamma-corrected) RGB color.
(defun hsv-to-xyz (hue sat val &optional (rgbspace srgb))
  (destructuring-bind (r g b) (hsv-to-rgb hue sat val)
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
	 
(defun qrgb-to-hsv (qr qg qb)
  (rgb-to-hsv (* qr #.(float 1/255 1d0))
	      (* qg #.(float 1/255 1d0))
	      (* qb #.(float 1/255 1d0))))

(defun xyz-to-hsv (x y z &key (rgbspace srgb) (threshold 1d-4))
  "Returns multiple values: (H S V), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of **linear** RGB values are
outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-hsv
		   (mapcar #'(lambda (i) (clamp i 0d0 1d0)) rgb))
	    out-of-gamut)))
  

(defun hsl-to-rgb (hue sat lum)
  "H is in R/360. S and V are in [0, 1]."
  (let* ((tmp (* 0.5d0 sat (- 1d0 (abs (- (* lum 2d0) 1d0)))))
	 (max (+ lum tmp))
	 (min (- lum tmp))
	 (delta (- max min))
	 (h-prime (/ (mod hue 360d0) 60d0))
	 (h-prime-int (floor h-prime)))
    (cond ((= sat 0d0) (list max max max))
	  ((= 0 h-prime-int) (list max
				   (+ min (* delta hue 0.016666666666666667d0))
				   min))
	  ((= 1 h-prime-int) (list (+ min (* delta (- 120d0 hue) 0.016666666666666667d0))
				   max
				   min))
	  ((= 2 h-prime-int) (list min
				   max
				   (+ min (* delta (- hue 120d0) 0.016666666666666667d0))))
	  ((= 3 h-prime-int) (list min
				   (+ min (* delta (- 240d0 hue) 0.016666666666666667d0))
				   max))
	  ((= 4 h-prime-int) (list (+ min (* delta (- hue 240d0) 0.016666666666666667d0))
				   min
				   max))
	  ((= 5 h-prime-int) (list max
				   min
				   (+ min (* delta (- 360d0 hue) 0.016666666666666667d0)))))))
				   

(defun hsl-to-qrgb (hue sat lum)
  (mapcar #'(lambda (x) (round (* x 255d0)))
	  (hsl-to-rgb hue sat lum)))

(defun hsl-to-xyz (hue sat lum &optional (rgbspace srgb))
  (destructuring-bind (r g b) (hsl-to-rgb hue sat lum)
    (rgb-to-xyz r g b rgbspace)))

(defun rgb-to-hsl (r g b)
  (let ((min (min r g b))
	(max (max r g b)))
    (let ((hue (cond ((= min max) 0d0)
		     ((= min b) (+ 60d0 (* 60d0 (/ (- g r) (- max min)))))
		     ((= min r) (+ 180d0 (* 60d0 (/ (- b g) (- max min)))))
		     ((= min g) (+ 300d0 (* 60d0 (/ (- r b) (- max min))))))))
      (list hue
	    (/ (- max min) (- 1d0 (abs (+ max min -1d0))))
	    (* 0.5d0 (+ max min))))))
	  

(defun qrgb-to-hsl (qr qg qb)
  (rgb-to-hsl (* qr #.(float 1/255 1d0))
	      (* qg #.(float 1/255 1d0))
	      (* qb #.(float 1/255 1d0))))

(defun xyz-to-hsl (x y z &key (rgbspace srgb) (threshold 1d-4))
    "Returns multiple values: (H S L), OUT-OF-GAMUT-P.
OUT-OF-GAMUT-P is true, if at least one of the **linear** RGB values
are outside the interval [-THRESHOLD, 1+THRESHOLD]."
  (multiple-value-bind (rgb out-of-gamut)
      (xyz-to-rgb x y z :rgbspace rgbspace :threshold threshold)
    (values (apply #'rgb-to-hsl
		   (mapcar #'(lambda (i) (clamp i 0d0 1d0)) rgb))
	    out-of-gamut)))
  
