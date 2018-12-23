;;;
;;; Built-in RGB spaces
;;;

(in-package :dufy/core)

(declaim (inline linearize-srgb delinearize-srgb))
(defun linearize-srgb (x)
  "linearizer of sRGB (actually the same as bg-sRGB)"
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (cond ((> x #.(* 0.0031308d0 12.92d0))
         (expt (* (+ 0.055d0 x) #.(/ 1.055d0)) 2.4d0))
        ((< x #.(* -0.0031308d0 12.92d0))
         (- (expt (* (- 0.055d0 x) #.(/ 1.055d0)) 2.4d0)))
        (t (* x #.(/ 12.92d0)))))

(defun delinearize-srgb (x)
  "delinealizer of sRGB (actually the same as bg-sRGB)"
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (cond ((> x 0.0031308d0)
         (+ (* 1.055d0 (expt x #.(/ 2.4d0))) -0.055d0))
        ((< x -0.0031308d0)
         (+ (* -1.055d0 (expt (- x) #.(/ 2.4d0))) 0.055d0))
        (t (* x 12.92d0))))

(defun linearize-scrgb-nl (x)
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (cond ((> x #.(* 4.5d0 0.018d0))
         (expt (* (+ 0.099d0 x) #.(/ 1.099d0)) #.(/ 0.45d0)))
        ((< x (* 4.5d0 -0.018d0))
         (- (expt (* (- 0.099d0 x) #.(/ 1.099d0)) #.(/ 0.45d0))))
        (t (* x #.(/ 4.5d0)))))

(defun delinearize-scrgb-nl (x)
  (declare (optimize (speed 3) (safety 0))
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

(defparameter +cie-rgb+
  (make-rgbspace 0.7347d0 0.2653d0 0.2738d0 0.7174d0 0.1666d0 0.0089d0
                      :illuminant +illum-e+)
  "CIE RGB (1931), no gamma-correction, 8-bit per channel.")

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
                 :illuminant +illum-d50+
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
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (cond ((> x #.(* 1/512 16d0))
         (expt x 1.8d0))
        ((< x #.(* -1/512 16d0))
         (- (expt (- x) 1.8d0)))
        (t (* x #.(float 1/16 1d0)))))
  
(defun delinearize-prophoto (x)
  (declare (optimize (speed 3) (safety 0))
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

