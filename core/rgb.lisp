;;;
;;; RGB and HSV/HSL
;;;

(uiop:define-package :dufy/core/rgb
  (:use :cl :alexandria :dufy/internal/* :dufy/core/spectrum :dufy/core/illuminants-data :dufy/core/xyz)
  (:export #:lrgb #:rgb #:qrgb #:rgbpack
           #:rgba #:qrgba #:rgbapack
           #:rgbspace
           #:make-rgbspace
           #:rgbspace-illuminant
           #:rgbspace-xr
           #:rgbspace-yr
           #:rgbspace-xg
           #:rgbspace-yg
           #:rgbspace-xb
           #:rgbspace-yb
           #:rgbspace-to-xyz-matrix
           #:rgbspace-from-xyz-matrix
           #:rgbspace-normal
           #:rgbspace-lmin
           #:rgbspace-lmax
           #:rgbspace-bit-per-channel
           #:rgbspace-min
           #:rgbspace-max
           #:rgbspace-qmax
           #:rgbspace-linearizer
           #:rgbspace-delinearizer
           #:gen-linearizer
           #:gen-delinearizer

           #:linearize
           #:delinearize
           #:xyz-to-lrgb
           #:lrgb-to-xyz
           #:lrgb-out-of-gamut-p
           #:rgb-to-lrgb
           #:lrgb-to-rgb
           #:rgb-out-of-gamut-p
           #:xyz-to-rgb
           #:rgb-to-xyz
           #:quantize
           #:dequantize
           #:rgb-to-qrgb
           #:qrgb-to-rgb
           #:qrgb-out-of-gamut-p
           #:lrgb-to-qrgb
           #:qrgb-to-lrgb
           #:xyz-to-qrgb
           #:qrgb-to-xyz
           
           #:qrgb-to-rgbpack
           #:rgbpack-to-qrgb
           #:rgb-to-rgbpack
           #:rgbpack-to-rgb
           #:lrgb-to-rgbpack
           #:rgbpack-to-lrgb
           #:xyz-to-rgbpack
           #:rgbpack-to-xyz

           #:hsv
           #:hsv-to-rgb
           #:rgb-to-hsv
           #:hsv-to-lrgb
           #:lrgb-to-hsv
           #:hsv-to-qrgb
           #:qrgb-to-hsv
           #:hsv-to-rgbpack
           #:rgbpack-to-hsv
           #:hsv-to-xyz
           #:xyz-to-hsv

           #:hsl
           #:hsl-to-rgb
           #:rgb-to-hsl
           #:hsl-to-lrgb
           #:lrgb-to-hsl
           #:hsl-to-qrgb
           #:qrgb-to-hsl
           #:hsl-to-rgbpack
           #:rgbpack-to-hsl
           #:hsl-to-xyz
           #:xyz-to-hsl))

(in-package :dufy/core/rgb)

(define-colorspace lrgb (lr lg lb)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Is linear RGB. The nominal range of each value depends on the RGB space but is typically [0, 1]")

(define-colorspace rgb (r g b)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Is gamma-corrected RGB. The nominal range of each value depends on the RGB space but is typically [0, 1]")

(define-colorspace qrgb (qr qg qb)
  :arg-types (fixnum fixnum fixnum)
  :return-types (fixnum fixnum fixnum)
  :documentation "Is quantized RGB. The nominal range of each value depends on the RGB space but is typically {0, 1, ..., 255}")

(define-colorspace rgbpack (int)
  :arg-types ((integer 0))
  :return-types ((integer 0))
  :documentation "Is RGB encoded to an unsigned integer. The size depends on the RGB space but is 24 bit for example.")

(define-colorspace rgba (r g b alpha)
  :arg-types (real real real real)
  :return-types (double-float double-float double-float double-float)
  :documentation "Is gamma-corrected RGBA. The nominal range of each value depends on the RGB space but is typically [0, 1]")

(define-colorspace qrgba (qr qg qb qalpha)
  :arg-types (fixnum fixnum fixnum fixnum)
  :return-types (fixnum fixnum fixnum fixnum)
  :documentation "Is quantized RGBA. The nominal range of each value depends on the RGB space but is typically {0, 1, ..., 255}")

(define-colorspace rgbapack (int)
  :arg-types ((integer 0))
  :return-types ((integer 0))
  :documentation "Is RGBA encoded to an unsigned integer. The order can be ARGB or RGBA.")

(defun gen-linearizer (gamma)
  "Returns a linearization function for a given gamma value. You
shouldn't call the returned function directly as it is not safe."
  (let ((gamma (float gamma 1d0)))
    #'(lambda (x)
        (declare (optimize (speed 3) (safety 0))
                 (double-float x))
        (if (plusp x)
            (expt x gamma)
            (- (expt (- x) gamma))))))

(defun gen-delinearizer (gamma)
  "Returns a gamma-correction function for a given gamma value. You
shouldn't call the returned function directly as it is not safe."
  (let ((/gamma (/ (float gamma 1d0))))
    #'(lambda (x)
        (declare (optimize (speed 3) (safety 0))
                 (double-float x))
        (if (plusp x)
            (expt x /gamma)
            (- (expt (- x) /gamma))))))

(defstruct (rgbspace (:constructor %make-rgbspace)
                     (:copier nil)) ; COPY-RGBSPACE is defined later.
  "Is structure of RGB space including encoding characteristics. You
shouldn't write to any slots directly; instead MAKE-RGBSPACE and
COPY-RGBSPACE are available."
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
  (normal t :type boolean) ; T, if min = 0d0 and max = 1d0

  ;; quantization characteristics
  (bit-per-channel 8 :type (integer 1 #.(floor (log most-positive-fixnum 2))))
  (qmax 255 :type (integer 1 #.most-positive-fixnum)) ; max. of quantized values
  (qmax-float 255d0 :type double-float)
  (length/qmax-float (float 1/255 1d0) :type double-float)
  (qmax-float/length 255d0 :type double-float))

(defun make-rgbspace (xr yr xg yg xb yb &key (illuminant +illum-d65+) (lmin 0d0) (lmax 1d0) (linearizer (rcurry #'float 1d0)) (delinearizer (rcurry #'float 1d0)) (bit-per-channel 8) (force-normal nil))
  "xr, yr, xg, yg, xb, yb := primary coordinates in the xy plane.
[lmin, lmax] := range of linear values ([0, 1] typically).

LINEARIZER and DELINEARIZER must be (FUNCTION * (VALUES DOUBLE-FLOAT
&OPTIONAL)).

If FORCE-NORMAL is T, the nominal range of gamma-corrected values is
forcibly set to [0d0, 1d0]. It is used to avoid the computed range
being e.g. [0d0, 0.9999999999999999d0]."
  (declare (optimize (speed 3) (safety 1))
           ((function * (values double-float &optional)) linearizer delinearizer))
  (with-ensuring-type double-float (xr yr xg yg xb yb)
    (let ((coordinates (make-array '(3 3)
                                   :element-type 'double-float
                                   :initial-contents
                                   `((,xr ,xg ,xb)
                                     (,yr ,yg ,yb)
                                     (,(- 1d0 xr yr) ,(- 1d0 xg yg) ,(- 1d0 xb yb))))))
      (multiple-value-bind (sr sg sb)
          (multiply-mat-vec (invert-matrix coordinates)
                            (illuminant-x illuminant)
                            1d0
                            (illuminant-z illuminant))
        (let* ((mat (make-array '(3 3)
                                :element-type 'double-float
                                :initial-contents
                                `((,(* sr (aref coordinates 0 0))
                                   ,(* sg (aref coordinates 0 1))
                                   ,(* sb (aref coordinates 0 2)))
                                  (,(* sr (aref coordinates 1 0))
                                   ,(* sg (aref coordinates 1 1))
                                   ,(* sb (aref coordinates 1 2)))
                                  (,(* sr (aref coordinates 2 0))
                                   ,(* sg (aref coordinates 2 1))
                                   ,(* sb (aref coordinates 2 2))))))
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
                          :from-xyz-matrix (invert-matrix mat)
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

;; FIXME: +SRGB+ is later defined though it is necessary here for
;; the default value of the keyword arg :RGBSPACE.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :dufy/core/rgbspaces-data)
    (make-package :dufy/core/rgbspaces-data)
    (uiop:export* :+srgb+ :dufy/core/rgbspaces-data))
  (import (uiop:find-symbol* :+srgb+ :dufy/core/rgbspaces-data)))
(defvar +srgb+)

(define-primary-converter (xyz lrgb) (x y z &key (rgbspace +srgb+) &aux (illuminant (rgbspace-illuminant rgbspace)))
  (declare (optimize (speed 3) (safety 1))
           (ignorable illuminant))
  (multiply-mat-vec (rgbspace-from-xyz-matrix rgbspace)
                    (float x 1d0)
                    (float y 1d0)
                    (float z 1d0)))

(define-primary-converter (lrgb xyz) (lr lg lb &key (rgbspace +srgb+) &aux (illuminant (rgbspace-illuminant rgbspace)))
  (declare (optimize (speed 3) (safety 1))
           (ignorable illuminant))
  (multiply-mat-vec (rgbspace-to-xyz-matrix rgbspace)
                    (float lr 1d0)
                    (float lg 1d0)
                    (float lb 1d0)))

;;;
;;; Linear RGB, gamma-corrected RGB, and quantized RGB
;;;

(defun lrgb-out-of-gamut-p (lr lg lb &key (rgbspace +srgb+) (threshold 1d-4))
  (declare (optimize (speed 3) (safety 1)))
  "Returns true if at least one of LR, LG, and LB is outside the
interval [RGBSPACE-LMIN - THRESHOLD, RGBSPACE-LMAX + THRESHOLD]."
  (with-ensuring-type double-float (lr lg lb threshold)
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
  (declare (optimize (speed 3) (safety 1)))
  "Returns true if at least one of R, G, and B is outside the interval
[RGBSPACE-MIN - THRESHOLD, RGBSPACE-MAX + THRESHOLD]."
  (with-ensuring-type double-float (r g b threshold)
    (let ((inf (- (rgbspace-min rgbspace) threshold))
          (sup (+ (rgbspace-max rgbspace) threshold)))
      (not (and (<= inf r sup)
                (<= inf g sup)
                (<= inf b sup))))))

(defconverter xyz rgb)
(defconverter rgb xyz)

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
  "Quantizes an RGB value to a QRGB value"
  (declare (optimize (speed 3) (safety 1)))
  (if clamp
      (clamp (round (* (- (float x 1d0) (rgbspace-min rgbspace))
                       (rgbspace-qmax-float/length rgbspace)))
             0
             (rgbspace-qmax rgbspace))
      (round (* (- (float x 1d0) (rgbspace-min rgbspace))
                (rgbspace-qmax-float/length rgbspace)))))

(declaim (inline dequantize))
(defun dequantize (n &key (rgbspace +srgb+))
  "Dequantizes a QRGB value to an RGB value"
  (declare (optimize (speed 3) (safety 1))
           (fixnum n))
  (+ (rgbspace-min rgbspace)
     (* n (rgbspace-length/qmax-float rgbspace))))

(define-primary-converter (rgb qrgb) (r g b &key (rgbspace +srgb+) (clamp t))
  (declare (optimize (speed 3) (safety 1)))
  "Quantizes RGB values from [RGBSPACE-MIN, RGBSPACE-MAX] ([0, 1]
typically) to {0, 1, ..., RGBSPACE-QMAX} ({0, 1, ..., 255} typically),
though it accepts all the real values."
  (with-ensuring-type double-float (r g b)
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

(define-primary-converter (rgba qrgba) (r g b alpha &key (rgbspace +srgb+) (clamp t))
  "Quantizes RGBA values from [RGBSPACE-MIN, RGBSPACE-MAX] ([0, 1],
typically) to {0, 1, ..., RGBSPACE-QMAX} ({0, 1, ..., 255},
typically), though it accepts all the real values."
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (r g b alpha)
    (let ((min (rgbspace-min rgbspace))
          (qmax-float/length (rgbspace-qmax-float/length rgbspace))
          (qmax (rgbspace-qmax rgbspace)))
      (if clamp
          (values (clamp (round (* (- r min) qmax-float/length)) 0 qmax)
                  (clamp (round (* (- g min) qmax-float/length)) 0 qmax)
                  (clamp (round (* (- b min) qmax-float/length)) 0 qmax)
                  (clamp (round (* (- alpha min) qmax-float/length)) 0 qmax))
          (values (round (* (- r min) qmax-float/length))
                  (round (* (- g min) qmax-float/length))
                  (round (* (- b min) qmax-float/length))
                  (round (* (- alpha min) qmax-float/length)))))))

(define-primary-converter (qrgb rgb) (qr qg qb &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((min (rgbspace-min rgbspace))
        (length/qmax-float (rgbspace-length/qmax-float rgbspace)))
    (values (+ min (* qr length/qmax-float))
            (+ min (* qg length/qmax-float))
            (+ min (* qb length/qmax-float)))))

(define-primary-converter (qrgba rgba) (qr qg qb qalpha &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  (let ((min (rgbspace-min rgbspace))
        (length/qmax-float (rgbspace-length/qmax-float rgbspace)))
    (values (+ min (* qr length/qmax-float))
            (+ min (* qg length/qmax-float))
            (+ min (* qb length/qmax-float))
            (+ min (* qalpha length/qmax-float)))))

(defconverters (xyz lrgb) qrgb)
(defconverters qrgb (xyz lrgb))

(define-primary-converter (qrgb rgbpack) (qr qg qb &key (rgbspace +srgb+) &aux (clamp nil))
  (declare (optimize (speed 3) (safety 1))
           (ignorable clamp))
  (let ((bpc (rgbspace-bit-per-channel rgbspace))
        (qmax (rgbspace-qmax rgbspace)))
    (+ (ash (clamp qr 0 qmax) (+ bpc bpc))
       (ash (clamp qg 0 qmax) bpc)
       (clamp qb 0 qmax))))

(define-primary-converter (rgbpack qrgb) (int &key (rgbspace +srgb+))
  (declare (optimize (speed 3) (safety 1)))
  "Decodes a packed RGB value, whose type depends on RGBSPACE but is
typically unsigned 24-bit integer.

It is guaranteed that this converter can also process a packed RGBA
value correctly if its order is ARGB."
  (let ((minus-bpc (- (rgbspace-bit-per-channel rgbspace)))
        (qmax (rgbspace-qmax rgbspace)))
    (values (logand (ash int (+ minus-bpc minus-bpc)) qmax)
            (logand (ash int minus-bpc) qmax)
            (logand int qmax))))

(define-primary-converter (qrgba rgbapack) (qr qg qb qalpha &key (rgbspace +srgb+) (order :argb) &aux (clamp nil))
  (declare (optimize (speed 3) (safety 1))
           (ignorable clamp))
  "Decodes a packed RGBA value, whose type depends on RGBSPACE but is
typically unsigned 32-bit integer.

The order can be :ARGB or :RGBA. Note that it is different from the
'physical' byte order in your machine, which depends on the
endianess."
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
  (declare (optimize (speed 3) (safety 1)))
  "The order can be :ARGB or :RGBA. Note that it is different from the
  'physical' byte order in your machine, which depends on the
  endianess."
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

(defconverters rgbpack (rgb lrgb xyz))
(defconverters (rgb lrgb xyz) rgbpack)

(defconverter rgbapack rgba)
(defconverter rgba rgbapack)


;;;
;;; HSV/HSL
;;;

(define-colorspace hsv (hue sat val)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "HUE is in the circle group R/360Z. The nominal range of SAT and VAL is [0, 1].")
(define-colorspace hsl (hue sat lum)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "HUE is in the circle group R/360Z. The nominal range of SAT and LUM is [0, 1].")

(define-primary-converter (hsv rgb) (hue sat val)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the
situation whether the returned values are meaningful."
  (let ((hue (the (double-float 0d0 360d0) (mod (float hue 1d0) 360d0)))
        (sat (float sat 1d0))
        (val (float val 1d0)))
    (let* ((c (* val sat))
           (h-prime (* hue #.(float 1/60 1d0)))
           (h-prime-int (floor h-prime))
           (x (* c (- 1d0 (abs (- (mod h-prime 2d0) 1d0)))))
           (base (- val c)))
      (cond ((= sat 0d0) (values base base base))
            ((= 0 h-prime-int) (values val (+ base x) base))
            ((= 1 h-prime-int) (values (+ base x) val base))
            ((= 2 h-prime-int) (values base val (+ base x)))
            ((= 3 h-prime-int) (values base (+ base x) val))
            ((= 4 h-prime-int) (values (+ base x) base val))
            ((= 5 h-prime-int) (values val base (+ base x)))
            (t (values 0d0 0d0 0d0) ; unreachable. just for avoiding compiler warnings
               )))))

(defconverters hsv (rgbpack qrgb lrgb xyz))

(define-primary-converter (rgb hsv) (r g b)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the
situation whether the returned values are meaningful."
  (with-ensuring-type double-float (r g b)
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

(defconverters (qrgb rgbpack lrgb xyz) hsv)

(define-primary-converter (hsl rgb) (hue sat lum)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the
situation whether the returned values are meaningful."
  (with-ensuring-type double-float (hue sat lum)
    (let* ((hue (mod hue 360d0))
           (tmp (* 0.5d0 sat (- 1d0 (abs (+ lum lum -1d0)))))
           (max (+ lum tmp))
           (min (- lum tmp))
           (delta (- max min))
           (h-prime (floor (the (double-float 0d0 6d0) (* hue 1/60)))))
      (cond ((= sat 0d0) (values max max max))
            ((= 0 h-prime) (values max
                                   (+ min (* delta hue 1/60))
                                   min))
            ((= 1 h-prime) (values (+ min (* delta (- 120d0 hue) 1/60))
                                   max
                                   min))
            ((= 2 h-prime) (values min
                                   max
                                   (+ min (* delta (- hue 120d0) 1/60))))
            ((= 3 h-prime) (values min
                                   (+ min (* delta (- 240d0 hue) 1/60))
                                   max))
            ((= 4 h-prime) (values (+ min (* delta (- hue 240d0) 1/60))
                                   min
                                   max))
            ((= 5 h-prime) (values max
                                   min
                                   (+ min (* delta (- 360d0 hue) 1/60))))
            (t (values 0d0 0d0 0d0) ; unreachable. just for avoiding compiler warnings
               )))))

(defconverters hsl (rgbpack qrgb lrgb xyz))

(define-primary-converter (rgb hsl) (r g b)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the
situation whether the returned values are meaningful."
  (with-ensuring-type double-float (r g b)
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
              (* 0.5d0 (+ maxrgb minrgb))))))

(defconverters (qrgb rgbpack lrgb xyz) hsl)
