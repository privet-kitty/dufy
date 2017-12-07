(in-package :clcl)

;; (require :cl-ppcre)

;; the largest chroma in the Munsell renotation data
;; (defparameter max-chroma-overall (apply #'max (mapcar #'third munsell-renotation-data)))
(defparameter *max-chroma-overall* 50)

(defun max-chroma-integer-case (hue40 value)
  (aref max-chroma-arr hue40 value))

(defun max-chroma-integer-case-dark (hue40 dark-value)
  (aref max-chroma-arr-dark hue40 dark-value))

;; max-chroma returns the largest chroma which the MUNSELL-HVC-TO- functions can receive.
;; The behavior of the MUNSELL-HVC-TO- functions is undefined, when chroma is larger than (max-chroma hue value)
(defun max-chroma (hue40 value &key (use-dark t))
  (let* ((hue (mod hue40 40))
	 (hue1 (floor hue))
	 (hue2 (mod (ceiling hue) 40)))
    (if (or (>= value 1)
	    (not use-dark))
	(let ((val1 (floor value))
	      (val2 (ceiling value)))
	  (min (max-chroma-integer-case hue1 val1)
	       (max-chroma-integer-case hue1 val2)
	       (max-chroma-integer-case hue2 val1)
	       (max-chroma-integer-case hue2 val2)))
	(let* ((dark-value (* value 5))
	       (dark-val1 (floor dark-value))
	       (dark-val2 (ceiling dark-value)))
	  (min (max-chroma-integer-case-dark hue1 dark-val1)
	       (max-chroma-integer-case-dark hue1 dark-val2)
	       (max-chroma-integer-case-dark hue2 dark-val1)
	       (max-chroma-integer-case-dark hue2 dark-val2))))))


;; convert munsell value to Y in [0, 1]
(defun munsell-value-to-y (v)
  (* v (+ 1.1914d0 (* v (+ -0.22533d0 (* v (+ 0.23352d0 (* v (+ -0.020484d0 (* v 0.00081939d0)))))))) 0.01d0))

(defun munsell-value-to-lstar (v)
  (- (* 116 (function-f (munsell-value-to-y v))) 16))

  
(defun munsell-value-to-achromatic-rgb255 (v)
  (let ((x (round (* (delinearize (munsell-value-to-y v)) 255))))
    (list x x x)))
-
(defun munsell-value-to-achromatic-xyy (v)
  (let* ((y (munsell-value-to-y v))
	 (largex (+ (* 0.4124564d0 y) (* 0.3575761d0 y) (* 0.1804375d0 y)))
	 (largey (+ (* 0.2126729d0 y) (* 0.7151522d0 y) (* 0.0721750d0 y)))
	 (largez (+ (* 0.0193339d0 y) (* 0.1191920d0 y) (* 0.9503041d0 y))))
    (apply #'(lambda (x y largey) (list x y (clamp largey 0d0 1d0)))
	   (apply #'xyz-to-xyy
		  (bradford largex largey largez clcl:d65 clcl:c)))))

(defun munsell-value-to-achromatic-lchab (v)
  (apply #'lab-to-lchab
	 (apply (rcurry #'xyz-to-lab clcl:c)
		(xyy-to-xyz (illuminant-x clcl:c)
			    (illuminant-y clcl:c)
			    (munsell-value-to-y v)))))

;; the version corresponding with Y of the munsell renotation data
;; v must be integer.
;; nearly equal to munsell-value-to-achromatic-xyy

(defun munsell-value-to-achromatic-xyy-from-mrd (v)
  (list 0.31006d0 0.31616d0
	(clamp (* (aref (vector 0d0 0.0121d0 0.03126d0 0.0655d0 0.120d0 0.1977d0 0.3003d0 0.4306d0 0.591d0 0.7866d0 1.0257d0) v) 0.975d0)
	       0d0 1d0)))

;; y should be in [0,1]
(defun y-to-munsell-value (y)
  (let* ((y1000 (* (clamp y 0 1) 1000))
	 (y1 (floor y1000))
	 (y2 (ceiling y1000)))
    (if (= y1 y2)
	(aref y-to-munsell-value-arr y1)
	(let ((r (- y1000 y1)))
	  (+ (* (- 1 r) (aref y-to-munsell-value-arr y1))
	     (* r (aref y-to-munsell-value-arr y2)))))))

(defun rgb255-to-munsell-value (r g b &optional (rgbspace srgbd65))
  (y-to-munsell-value (second (rgb255-to-xyz r g b rgbspace))))


;; hue ∈ Z/40, tmp-value ∈ {0, 1, ..., 10}, half-chroma ∈ {0, 1, ..., max-chroma/2}
;; If dark is t, tmp-value ∈ {0, 1, 2, 3, 4 ,5} and treated as {0, 0.2, ...., 1.0}.
(defun munsell-hvc-to-xyy-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((hue (mod hue40 40)))
    (if dark
	(list (aref mrd-array-dark hue tmp-value half-chroma 0)
	      (aref mrd-array-dark hue tmp-value half-chroma 1)
	      (aref mrd-array-dark hue tmp-value half-chroma 2))
	(list (aref mrd-array hue tmp-value half-chroma 0)
	      (aref mrd-array hue tmp-value half-chroma 1)
	      (aref mrd-array hue tmp-value half-chroma 2)))))


(defun munsell-hvc-to-lrgb-simplest-case (hue value half-chroma)
  (apply #'xyz-to-lrgb
	 (apply (rcurry #'bradford clcl:c clcl:d65)
		(apply #'xyy-to-xyz
		       (munsell-hvc-to-xyy-simplest-case hue value half-chroma nil)))))

;; CAUTION: This LCH(ab) values are under Illuminant C.
;; (defun munsell-hvc-to-lchab-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (apply #'(lambda (lstar cstarab hab) (list (clamp lstar 0d0 100d0) cstarab hab))
;; 	 (apply #'lab-to-lchab
;; 		(apply (rcurry #'xyy-to-lab clcl:c)
;; 		       (munsell-hvc-to-xyy-simplest-case hue40 tmp-value half-chroma dark)))))

(defun munsell-hvc-to-lchab-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((hue (mod hue40 40)))
    (if dark
	(list (aref mrd-array-lchab-dark hue tmp-value half-chroma 0)
	      (aref mrd-array-lchab-dark hue tmp-value half-chroma 1)
	      (aref mrd-array-lchab-dark hue tmp-value half-chroma 2))
	(list (aref mrd-array-lchab hue tmp-value half-chroma 0)
	      (aref mrd-array-lchab hue tmp-value half-chroma 1)
	      (aref mrd-array-lchab hue tmp-value half-chroma 2)))))

	 

(defun munsell-hvc-to-xyy-value-chroma-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let* ((hue (mod hue40 40))
	 (hue1 (floor hue))
	 (hue2 (ceiling hue)))
    (cond ((= hue1 hue2)
	   (munsell-hvc-to-xyy-simplest-case (round hue) tmp-value half-chroma dark))
	  ((or (zerop tmp-value) (zerop half-chroma))
	   (munsell-value-to-achromatic-xyy (if dark (* tmp-value 0.2d0) tmp-value))) ;avoid division with zero
	  (t 
	   (destructuring-bind (x1 y1 largey)
	       (munsell-hvc-to-xyy-simplest-case hue1 tmp-value half-chroma dark)
	     (destructuring-bind (x2 y2 nil)
		 (munsell-hvc-to-xyy-simplest-case hue2 tmp-value half-chroma dark)
	       (destructuring-bind (r1 theta1)
		   (xy-to-polar x1 y1)
		 (destructuring-bind (r2 theta2)
		     (xy-to-polar x2 y2)
		   (let* ((theta (lerp-in-circle-group theta1 theta2 (- hue hue1)))
			  (r (+ (* r1 (/ (subtract-with-mod theta2 theta)
					 (subtract-with-mod theta2 theta1)))
				(* r2 (/ (subtract-with-mod theta theta1)
					 (subtract-with-mod theta2 theta1))))))
		     (append (polar-to-xy r theta) (list largey)))))))))))

(defun munsell-hvc-to-lchab-value-chroma-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let* ((hue (mod hue40 40))
	 (hue1 (floor hue))
	 (hue2 (ceiling hue)))
    (cond ((= hue1 hue2)
	   (munsell-hvc-to-lchab-simplest-case (round hue) tmp-value half-chroma dark))
	  ((or (zerop tmp-value) (zerop half-chroma))
	   (munsell-value-to-achromatic-lchab (if dark (* tmp-value 0.2d0) tmp-value))) ;avoid division with zero
	  (t 
	   (destructuring-bind (lstar cstarab1 hab1)
	       (munsell-hvc-to-lchab-simplest-case hue1 tmp-value half-chroma dark)
	     (destructuring-bind (nil cstarab2 hab2)
		 (munsell-hvc-to-lchab-simplest-case hue2 tmp-value half-chroma dark)
	       (let* ((hab (lerp-in-circle-group hab1 hab2 (- hue hue1) 360d0))
		      (cstarab (+ (* cstarab1 (/ (subtract-with-mod hab2 hab 360d0)
						 (subtract-with-mod hab2 hab1 360d0)))
				  (* cstarab2 (/ (subtract-with-mod hab hab1 360d0)
						 (subtract-with-mod hab2 hab1 360d0))))))
		 (list lstar cstarab hab))))))))


(defun munsell-hvc-to-xyy-value-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((hchroma1 (floor half-chroma))
	(hchroma2 (ceiling half-chroma)))
    (if (= hchroma1 hchroma2)
	(munsell-hvc-to-xyy-value-chroma-integer-case hue40 tmp-value (round half-chroma) dark)
	(destructuring-bind (x1 y1 largey)
	    (munsell-hvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma1 dark)
	  (destructuring-bind (x2 y2 nil)
	      (munsell-hvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma2 dark)
	    (let* ((x (+ (* x1 (- hchroma2 half-chroma))
			 (* x2 (- half-chroma hchroma1))))
		   (y (+ (* y1 (- hchroma2 half-chroma))
			 (* y2 (- half-chroma hchroma1)))))
	      (list x y largey)))))))


(defun munsell-hvc-to-lchab-value-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((hchroma1 (floor half-chroma))
	(hchroma2 (ceiling half-chroma)))
    (if (= hchroma1 hchroma2)
	(munsell-hvc-to-lchab-value-chroma-integer-case hue40 tmp-value (round half-chroma) dark)
	(destructuring-bind (lstar astar1 bstar1)
	    (apply #'lchab-to-lab
		   (munsell-hvc-to-lchab-value-chroma-integer-case hue40 tmp-value hchroma1 dark))
	  (destructuring-bind (nil astar2 bstar2)
	      (apply #'lchab-to-lab
		     (munsell-hvc-to-lchab-value-chroma-integer-case hue40 tmp-value hchroma2 dark))
	    (let* ((astar (+ (* astar1 (- hchroma2 half-chroma))
			     (* astar2 (- half-chroma hchroma1))))
		   (bstar (+ (* bstar1 (- hchroma2 half-chroma))
			     (* bstar2 (- half-chroma hchroma1)))))
	      (lab-to-lchab lstar astar bstar)))))))

(defun munsell-hvc-to-xyy-general-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
    (let  ((tmp-val1 (floor tmp-value))
	   (tmp-val2 (ceiling tmp-value))
	   (largey (munsell-value-to-y true-value)))
      (if (= tmp-val1 tmp-val2)
	  (munsell-hvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
	  (destructuring-bind (x1 y1 largey1)
	      (munsell-hvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
	    (destructuring-bind (x2 y2 largey2)
		(munsell-hvc-to-xyy-value-integer-case hue40 tmp-val2 half-chroma dark)
	      (let* ((x (+ (* x1 (/ (- largey2 largey) (- largey2 largey1)))
			   (* x2 (/ (- largey largey1) (- largey2 largey1)))))
		     (y (+ (* y1 (/ (- largey2 largey) (- largey2 largey1)))
			   (* y2 (/ (- largey largey1) (- largey2 largey1))))))
		(list x y largey))))))))

(defun munsell-hvc-to-lchab-general-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
    (let  ((tmp-val1 (floor tmp-value))
	   (tmp-val2 (ceiling tmp-value))
	   (lstar (munsell-value-to-lstar true-value)))
      (if (= tmp-val1 tmp-val2)
	  (munsell-hvc-to-lchab-value-integer-case hue40 tmp-val1 half-chroma dark)
	  ; If the given color is too dark to interpolate it by the MRD,
	  ; we use the fact that the chroma of LCH(ab) corresponds roughly
	  ; to the one of Munsell.
	  (if (= tmp-val1 0)
	      (destructuring-bind (nil cstarab hab)
		  (munsell-hvc-to-lchab-value-integer-case hue40 1 half-chroma dark)
		(list lstar cstarab hab))
	      (destructuring-bind (lstar1 astar1 bstar1)
		  (apply #'lchab-to-lab
			 (munsell-hvc-to-lchab-value-integer-case hue40 tmp-val1 half-chroma dark))
		(destructuring-bind (lstar2 astar2 bstar2)
		    (apply #'lchab-to-lab
			   (munsell-hvc-to-lchab-value-integer-case hue40 tmp-val2 half-chroma dark))
		  (let* ((astar (+ (* astar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
				   (* astar2 (/ (- lstar lstar1) (- lstar2 lstar1)))))
			 (bstar (+ (* bstar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
				   (* bstar2 (/ (- lstar lstar1) (- lstar2 lstar1))))))
		    (lab-to-lchab lstar astar bstar)))))))))

; Error:
; (clcl::munsell-hvc-to2-xyy 0.9999999999999999d0 2.84d0 3d0)
; (clcl::munsell-hvc-to-lchab-value-chroma-integer-case 0.9999999999999999d0 2 1)

; CAUTION: the Standard Illuminant is C
(defun munsell-hvc-to-xyy (hue40 value chroma)
  (if (>= value 1)
      (munsell-hvc-to-xyy-general-case hue40 value (/ chroma 2) nil)
      (munsell-hvc-to-xyy-general-case hue40 (* value 5) (/ chroma 2) t)))

(defun munsell-hvc-to-lchab (hue40 value chroma)
  (if (>= value 1)
      (munsell-hvc-to-lchab-general-case hue40 value (/ chroma 2) nil)
      (munsell-hvc-to-lchab-general-case hue40 (* value 5) (/ chroma 2) t)))

(defun compare-munsell-converter (mc)
  (let ((deltaes nil)
	(max-deltae 0)
	(outlier nil)
	(number-within-gamut 0))
    (dotimes (x mc)
      (let* ((hue40 (random 40d0))
	     (value (+ 0.2d0 (random 9.8d0)))
	     (chroma (random (coerce (max-chroma hue40 value) 'double-float))))
	(let* ((lab1 (apply (rcurry #'xyy-to-lab clcl:c)
			    (munsell-hvc-to-xyy hue40 value chroma)))
	       (lab2 (apply #'lchab-to-lab
			    (munsell-hvc-to-lchab hue40 value chroma)))
	       (deltae (apply #'deltae (append lab1 lab2))))
	  (unless (nth-value 1 (munsell-hvc-to-lrgb hue40 value chroma :threshold 0))
	    (incf number-within-gamut)
	    (when (> deltae max-deltae)
	      (setf max-deltae deltae)
	      (setf outlier (list hue40 value chroma)))
	    (push deltae deltaes)))))
    (format t "Processed colors within sRGB(d65) gamut = ~A~%" number-within-gamut)
    (format t "Mean Delta E = ~A~%" (funcall #'alexandria:mean deltaes))
    (format t "Maximum Delta E = ~A~%" (apply #'max deltaes))
    (format t "Outlier (H, V, C) = ~A~%" outlier)))

;; (clcl::compare-munsell-converter 100000)
;; =>
;; Processed colors within sRGB(d65) gamut = 48480
;; Mean Delta E = 0.35641873193747986d0
;; Maximum Delta E = 4.67977844149827d0
;; Outlier (H, V, C) = (15.07830806002132d0 1.4835458770925156d0
;;                      5.06906007523528d0)

(defun munsell-hvc-to-lab (hue40 value chroma)
  (apply #'xyy-to-lab (munsell-hvc-to-xyy hue40 value chroma)))

(defun munsell-hvc-to2-lab (hue40 value chroma)
  (apply #'lchab-to-lab (munsell-hvc-to-lchab hue40 value chroma)))

(defun munsell-hvc-to2-xyy (hue40 value chroma)
  (apply #'lchab-to-xyy (munsell-hvc-to-lchab hue40 value chroma)))

(defun munsell-hvc-to-xyz (hue40 value chroma)
  (apply (rcurry #'bradford clcl:c clcl:d65)
	 (apply #'xyy-to-xyz (munsell-hvc-to-xyy hue40 value chroma))))

;; return multiple values: (lr lg lb),  out-of-gamut-p
;; Note that the pure white (N 10.0) could be judged as out of gamut by numerical error, if threshold is too small.
(defun munsell-hvc-to-lrgb (hue40 value chroma &key (threshold 0.001d0))
  (apply (rcurry #'xyz-to-lrgb :threshold threshold)
	 (munsell-hvc-to-xyz hue40 value chroma)))

;; Unlike MUNSELL-HVC-TO-LRGB, all of achromatic colors are judged as within gamut.
;; return multiple values: (r g b),  out-of-gamut-p
(defun munsell-hvc-to-rgb255 (hue40 value chroma &key (threshold 0.001d0))
  (multiple-value-bind (rgb255 out-of-gamut)
      (apply (rcurry #'xyz-to-rgb255 :threshold threshold)
	     (munsell-hvc-to-xyz hue40 value chroma))
    (if (and (= chroma 0))
	(values rgb255 nil)
	(values rgb255 out-of-gamut))))


(defun munsellspec-to-hvc (spec)
  (destructuring-bind (hue-suffix value chroma)
      (mapcar #'read-from-string (cl-ppcre:split "[^0-9.]+" spec))
    (let* ((hue-name (intern (cl-ppcre:scan-to-strings "[A-Z]+" spec) "KEYWORD"))
	   (hue-number
	    (case hue-name
	      (:R 0) (:YR 1) (:Y 2) (:GY 3) (:G 4)
	      (:BG 5) (:B 6) (:PB 7) (:P 8) (:RP 9)
	      (otherwise (error "invalid spec")))))
      (list (+ (* hue-number 4) (/ (* hue-suffix 2) 5))
	    value
	    chroma))))

;; return multiple values: (x, y, Y), out-of-macadam-limit-p
(defun munsellspec-to-xyy (spec)
  (destructuring-bind (h v c) (munsellspec-to-hvc spec)
    (if (> c (max-chroma h v))
	(values (list most-negative-single-float most-negative-single-float most-negative-single-float) t) ;out of MacAdam limit
	(values (funcall #'munsell-hvc-to-xyy h v c) nil))))

;; return multiple values: (x, y, Y), out-of-gamut-p
(defun munsellspec-to-rgb255 (spec &key (threshold 0.0001d0))
  (destructuring-bind (nil v chroma) (munsellspec-to-hvc spec)
    (multiple-value-bind (xyy out-of-macadam-limit) (munsellspec-to-xyy spec)
      (if out-of-macadam-limit
	  (values (list -1 -1 -1) t)
	  (multiple-value-bind (rgb255 out-of-gamut) 
	      (apply #'(lambda (x y z) (xyz-to-rgb255 x y z :threshold threshold))
		     (apply (rcurry #'bradford clcl:c clcl:d65)
			    (apply #'xyy-to-xyz xyy)))
	    (if (and (= v 10) (= chroma 0))
		(values rgb255 nil)
		(values rgb255 out-of-gamut)))))))


(defconstant possible-colors 16777216) ;256*256*256

(defun encode-munsell-hvc1000 (h1000 v1000 c500 &optional (interpolated-flag 0))
  (+ (ash interpolated-flag 31)
     (ash h1000 20)
     (ash v1000 10)
     c500))

(defun interpolatedp (u32)
  (not (zerop (logand u32 #b10000000000000000000000000000000))))

(defun set-interpolated (u32)
  (logior #b10000000000000000000000000000000 u32))

(defun decode-munsell-hvc1000 (u32)
  (list (logand (ash u32 -20) #b1111111111)
	(logand (ash u32 -10) #b1111111111)
	(logand u32 #b1111111111)))

(defun decode-munsell-hvc (u32)
  (destructuring-bind (h1000 v1000 c500) (decode-munsell-hvc1000 u32)
    (list (/ h1000 25.0)
	  (/ v1000 100.0)
	  (/ c500 10.0))))

(defconstant +maxu32+ #xffffffff)

;; (defparameter munsell-inversion-data nil)

;; (defun initialize-munsell-inversion-data ()
;;   (setf munsell-inversion-data
;; 	(make-array possible-colors
;; 		    :element-type '(unsigned-byte 32)
;; 		    :initial-element +maxu32+)))

(defun make-munsell-inversion-data (&optional (with-interpolation t))
  (let ((mid (make-array possible-colors :element-type '(unsigned-byte 32) :initial-element +maxu32+))
	(deltae-arr (make-array possible-colors :element-type 'double-float :initial-element most-positive-double-float)))
    (dotimes (h1000 1000)
      (let ((hue (/ h1000 25.0d0)))
	(format t "processing data at hue ~a / 1000~%" h1000)
	(dotimes (v1000 1001)
	  (let* ((value (/ v1000 100.0d0))
		 (maxc500 (1+ (* (clcl:max-chroma hue value) 10))))
	    (dotimes (c500 maxc500)
	      (let ((chroma (/ c500 10.0d0)))
		(destructuring-bind (x y z)
		    (clcl:munsell-hvc-to-xyz hue value chroma)
		  (multiple-value-bind (rgb255 out-of-gamut)
		      (clcl:xyz-to-rgb255 x y z :threshold 0.001d0)
		    (unless out-of-gamut
		      (let ((hex (apply #'clcl:rgb255-to-hex rgb255)))
			(destructuring-bind (true-x true-y true-z)
			    (apply #'clcl:rgb255-to-xyz rgb255)
			  (let ((old-deltae (aref deltae-arr hex))
				(new-deltae (clcl:xyz-deltae x y z true-x true-y true-z)))
			    (when (< new-deltae old-deltae)
					;rotate if the new color is nearer to the true color than the old one.
			      (setf (aref mid hex)
				    (encode-munsell-hvc1000 h1000 v1000 c500))
			      (setf (aref deltae-arr hex)
				    new-deltae))))))))))))))
    (let ((gaps (number-of-gaps mid)))
      (format t "Primary data are set. Number of gaps is ~A (~A).~%"
	      gaps (/ gaps (float possible-colors))))
    (when with-interpolation
      (format t "Now interpolating...~%")
      (interpolate-munsell-inversion-data mid))
    mid))
  

;; return 0 if it is already fully interpolated
;; (defun interpolate-once ()
;;   (let ((original-mid (copy-seq munsell-inversion-data))
;; 	(not-interpolated 0))
;;     (dotimes (hex possible-colors not-interpolated)
;;       (let ((u32 (aref munsell-inversion-data hex)))
;; 	(when (= u32 +maxu32+)
;; 	  (destructuring-bind (r g b) (clcl:hex-to-rgb255 hex)
;; 	    (let ((u32-neighbor1 (aref original-mid
;; 				       (clcl:rgb255-to-hex r g (clcl:rgb1+ b)))))
;; 	      (if (not (= u32-neighbor1 +maxu32+))
;; 		  (setf (aref munsell-inversion-data hex)
;; 			(set-interpolated u32-neighbor1))
;; 		  (let ((u32-neighbor2 (aref original-mid
;; 					     (clcl:rgb255-to-hex r g (clcl:rgb1- b)))))
;; 		    (if (not (= u32-neighbor2 +maxu32+))
;; 			(setf (aref munsell-inversion-data hex)
;; 			      (set-interpolated u32-neighbor2))
;; 			(let ((u32-neighbor3 (aref original-mid
;; 						   (clcl:rgb255-to-hex r (clcl:rgb1+ g) b))))
;; 			  (if (not (= u32-neighbor3 +maxu32+))
;; 			      (setf (aref munsell-inversion-data hex)
;; 				    (set-interpolated u32-neighbor3))
;; 			      (let ((u32-neighbor4 (aref original-mid
;; 							 (clcl:rgb255-to-hex r (clcl:rgb1- g) b))))
;; 				(if (not (= u32-neighbor4 +maxu32+))
;; 				    (setf (aref munsell-inversion-data hex)
;; 					  (set-interpolated u32-neighbor4))
;; 				    (let ((u32-neighbor5 (aref original-mid
;; 							       (clcl:rgb255-to-hex (clcl:rgb1+ r) g b))))
;; 				      (if (not (= u32-neighbor5 +maxu32+))
;; 					  (setf (aref munsell-inversion-data hex)
;; 						(set-interpolated u32-neighbor5))
;; 					  (let ((u32-neighbor6 (aref original-mid
;; 								     (clcl:rgb255-to-hex (clcl:rgb1- r) g b))))
;; 					    (if (not (= u32-neighbor6 +maxu32+))
;; 						(setf (aref munsell-inversion-data hex)
;; 						      (set-interpolated u32-neighbor6))
;;						(incf not-interpolated)))))))))))))))))))

(defun find-least-score-rec (testfunc lst l-score l-node)
  (if (null lst)
      l-node
      (let ((score (funcall testfunc (car lst))))
	(if (< score l-score)
	    (find-least-score-rec testfunc (cdr lst) score (car lst))
	    (find-least-score-rec testfunc (cdr lst) l-score l-node)))))

;; return a node where the testfunc gives the minimum value.
(defun find-least-score (testfunc lst)
  (find-least-score-rec testfunc
			lst
			(funcall testfunc (car lst))
			(car lst)))

(defun interpolate-once (munsell-inversion-data)
  (let ((source-mid (copy-seq munsell-inversion-data))
	(not-interpolated 0))
    (dotimes (hex possible-colors not-interpolated)
      (let ((u32 (aref source-mid hex)))
	(when (= u32 +maxu32+)
	  (destructuring-bind (r g b) (clcl:hex-to-rgb255 hex)
	    (destructuring-bind (x y z) (clcl:rgb255-to-xyz r g b)
	      (let ((neighbors
		     (list (list r g (clcl:rgb1+ b))
			   (list r g (clcl:rgb1- b))
			   (list r (clcl:rgb1+ g) b)
			   (list r (clcl:rgb1- g) b)
			   (list (clcl:rgb1+ r) g b)
			   (list (clcl:rgb1- r) g b))))
		(let ((nearest-hex
		       (apply #'clcl:rgb255-to-hex
			(find-least-score
			 #'(lambda (n-rgb255)
			     (let* ((n-hex (apply #'clcl:rgb255-to-hex n-rgb255))
				    (n-u32 (aref source-mid n-hex)))
			       (if (= n-u32 +maxu32+)
				   most-positive-double-float
				   (destructuring-bind (n-x n-y n-z)
				       (apply #'clcl:munsell-hvc-to-xyz (decode-munsell-hvc n-u32))
				     (clcl:xyz-deltae x y z n-x n-y n-z)))))
			 neighbors))))
		  (if (= (aref source-mid nearest-hex) +maxu32+)
		      (incf not-interpolated)
		      (setf (aref munsell-inversion-data hex)
			    (set-interpolated (aref source-mid nearest-hex)))))))))))))



(defun interpolate-munsell-inversion-data (munsell-inversion-data)
  (let ((i 0))
    (loop
       (let ((remaining (interpolate-once munsell-inversion-data)))
	 (if (zerop remaining)
	     (progn
	       (format t "Loop: ~a: Perfectly interpolated.~%" (incf i))
	       (return))
	     (format t "Loop: ~a: Remaining nodes = ~A~%." (incf i) remaining))))))

; set value by y-to-munsell-value in MID. Thereby chroma is properly corrected.
(defun set-atsm-value (munsell-inversion-data)
  (dotimes (hex possible-colors)
    (destructuring-bind (h1000 nil c500)
	(decode-munsell-hvc1000 (aref munsell-inversion-data hex))
      (let* ((hue40 (clamp (/ h1000 25.0) 0 40))
	     (new-value (y-to-munsell-value (second (apply (rcurry #'bradford clcl:d65 clcl:c)
							(apply #'rgb255-to-xyz
							       (hex-to-rgb255 hex))))))
	     (chroma (* c500 0.1))
	     (v1000-new (round (* new-value 100)))
	     (c500-new (round (* (min (max-chroma hue40 new-value) chroma) 10))))
	(setf (aref munsell-inversion-data hex) (encode-munsell-hvc1000 h1000 v1000-new c500-new))))))


;; save/load Munsell inversion data to/from a binary file with big endian

(defun save-munsell-inversion-data (munsell-inversion-data &optional (filename "srgbd65-to-munsell-be.dat"))
  (let ((path (merge-pathnames (asdf:system-source-directory :clcl) filename)))
    (with-open-file (out path
			 :direction :output
			 :element-type '(unsigned-byte 8)
			 :if-exists :supersede)
      (dotimes (x possible-colors)
	(nibbles:write-ub32/be (aref munsell-inversion-data x) out))
      (format t "Munsell inversion data is saved in ~A.~%" path))))

(defun load-munsell-inversion-data (&optional (filename "srgbd65-to-munsell-be.dat"))
  (let ((path (merge-pathnames (asdf:system-source-directory :clcl) filename)))
    (with-open-file (in path
			:direction :input
			:element-type '(unsigned-byte 8))
      (let ((munsell-inversion-data (make-array possible-colors :element-type '(unsigned-byte 32) :initial-element +maxu32+)))
	(dotimes (x possible-colors munsell-inversion-data)
	  (setf (aref munsell-inversion-data x) (nibbles:read-ub32/be in)))))))

(defun check-data-from-srgb (munsell-inversion-data r g b)
  (let ((u32 (aref munsell-inversion-data (clcl:rgb255-to-hex r g b))))
    (if (= u32 +maxu32+)
	nil
	(apply #'clcl:munsell-hvc-to-rgb255 (decode-munsell-hvc u32)))))

(defun check-all-data (munsell-inversion-data)
  (dotimes (x possible-colors)
    (let* ((srgb (clcl:hex-to-rgb255 x))
	   (srgb2 (apply #'check-data-from-srgb (append (list munsell-inversion-data) srgb))))
      (unless (null srgb2)
	(when (not (equal srgb srgb2))
	  (format t "inacurrate value at position: ~a" x))))))
	  

;; sRGB d65 to munsell HVC	  
(defun rgb255-to-munsell-hvc (r g b munsell-inversion-data)
  (decode-munsell-hvc (aref munsell-inversion-data (clcl:rgb255-to-hex r g b))))


;; one-in-all function
;; (defun generate-all (&key (filename "srgbd65-to-munsell-be.dat") (with-interpolate t))
;;   (time
;;    (progn
;;      (format t "generating Munsell inversion data...~%")
;;      (make-munsell-inversion-data)
;;      ;; (format t "checking the reliability of the data...~%")
;;      ;; (check-all-data)
;;      (when with-interpolate
;;        (format t "interpolating the Munsell inversion data...~%")
;;        (interpolate-munsell-inversion-data))
;;      (format t "save data to ~a.~%" filename)
;;      (save-dat-file filename))))

(defun build-mid (&optional (filename "srgbd65-to-munsell-be.dat") (with-interpolation t))
  (format t "generating Munsell inversion data...~%")
  (let ((mid (make-munsell-inversion-data with-interpolation)))
    (save-munsell-inversion-data mid filename)))

(defun number-of-gaps (munsell-inversion-data)
  (let ((gaps 0))
    (dotimes (hex possible-colors)
      (when (= +maxu32+ (aref munsell-inversion-data hex))
	  (incf gaps)))
    gaps))
  
(defun gap-rate-b (munsell-inversion-data)
  (let ((gaps-sum 0))
    (dotimes (b 256)
     (let ((gaps 0))
       (dotimes (r 256)
	 (dotimes (g 256)
	   (if (= +maxu32+ (aref munsell-inversion-data (clcl:rgb255-to-hex r g b)))
	       (incf gaps))))
       (format t "b = ~a, gap rate = ~a~%" b (/ gaps 65536.0))
       (setf gaps-sum (+ gaps-sum gaps))))
    (format t "total gap rate = ~a~%" (/ gaps-sum (float possible-colors)))))


(defun gap-rate-by-flag (munsell-inversion-data)
  (let ((gaps-sum 0))
    (dotimes (b 256)
     (let ((gaps 0))
       (dotimes (r 256)
	 (dotimes (g 256)
	   (if (interpolatedp (aref munsell-inversion-data (clcl:rgb255-to-hex r g b)))
	       (incf gaps))))
       (format t "b = ~a, gap rate = ~a~%" b (/ gaps 65536.0))
       (setf gaps-sum (+ gaps-sum gaps))))
    (format t "total gap rate = ~a~%" (/ gaps-sum (float possible-colors)))))

(defun gap-rate-by-brightness (munsell-inversion-data)
  (let ((gaps-sum 0))
    (loop for brightness-sum from 0 to 765 do
	 (let ((gaps 0)
	       (number-of-colors 0)
	       (max-r (min 255 brightness-sum)))
	   (loop for r from 0 to max-r do
		(let ((min-g (max 0 (- brightness-sum 255 r)))
		      (max-g (min 255 (- brightness-sum r))))
		  (loop for g from min-g to max-g do
		       (let ((min-b (max 0 (- brightness-sum r g)))
			     (max-b (min 255 (- brightness-sum r g))))
			 (loop for b from min-b to max-b do
			      (incf number-of-colors)
			      (when (= +maxu32+ (aref munsell-inversion-data (clcl:rgb255-to-hex r g b)))
				(incf gaps)
				(incf gaps-sum)))))))
	   (format t "brightness = ~a, gap rate = ~a (= ~a / ~a).~%"
		   brightness-sum
		   (/ (float gaps) number-of-colors) 
		   gaps
		   number-of-colors)))
    (format t "total gap rate = ~a~%" (/ gaps-sum (float possible-colors)))))
    

;; examine the total error of interpolated data in MID	
(defun examine-interpolation-error (munsell-inversion-data &optional (start 0) (end possible-colors))
  (let ((maximum 0)
	(worst-hex nil)
	(sum 0)
	(nodes 0))
    (loop for hex from start below end do
      (let ((u32 (aref munsell-inversion-data hex)))
	(if (interpolatedp u32)
	    (destructuring-bind  (r1 g1 b1) (clcl:hex-to-rgb255 hex)
	      (destructuring-bind (r2 g2 b2) (apply #'clcl:munsell-hvc-to-rgb255 (decode-munsell-hvc u32))
		(let ((delta (clcl:rgb255-deltae r1 g1 b1 r2 g2 b2)))
		  (setf sum (+ sum delta))
		  (when (> delta maximum)
		    (setf maximum delta)
		    (setf worst-hex hex))
		  (incf nodes)))))))
    (format t "Number of Interpolated Nodes = ~A (~,3F%)~%" nodes (* 100d0 (/ nodes (- end start))))
    (format t "Mean Color Difference: ~a~%" (/ sum nodes))
    (format t "Maximum Color Difference: ~a at hex ~a~%" maximum worst-hex)))

;;; Errors of the interpolated nodes of MID with threshold 0.001:
;; Number of Interpolated Nodes = 3729095 (22.227%)
;; Mean Color Difference: 0.3134200498636899d0
;; Maximum Color Difference: 8.859190406312553d0 at hex 19198


;; get the maximun radius of the spheres of missing values in the non-interpolated munsell inversion data.
(defun get-radius-of-blank-sphere (mid depth r g b)
  (if (not (= +maxu32+ (aref mid (clcl:rgb255-to-hex r g b))))
      depth
      (max (get-radius-of-blank-sphere mid (1+ depth) r g (clcl:rgb1+ b))
	   (get-radius-of-blank-sphere mid (1+ depth) r g (clcl:rgb1- b))
	   (get-radius-of-blank-sphere mid (1+ depth) r (clcl:rgb1+ g) b)
	   (get-radius-of-blank-sphere mid (1+ depth) r (clcl:rgb1- g) b)
	   (get-radius-of-blank-sphere mid (1+ depth) (clcl:rgb1+ r) g b)
	   (get-radius-of-blank-sphere mid (1+ depth) (clcl:rgb1- r) g b))))

(defun maximum-radius-of-blank-sphere (mid)
  (let ((maximum 0))
    (dotimes (hex possible-colors maximum)
      (when (= (mod hex 10000) 0)
	(format t "~a / ~a hues were processed." hex possible-colors))
      (let ((rad (apply #'get-radius-of-blank-sphere mid 0 (clcl:hex-to-rgb255 hex))))
	(if (> rad maximum)
	    (setf maximum rad))))))



;; (defun find-value-in-mrd (value)
;;   (let ((xyy-lst nil))
;;     (dolist (line munsell-renotation-data xyy-lst)
;;       (when (= (second line) value)
;; 	(push (cdddr line) xyy-lst)))))


;; (defun find-value-in-general (value)
;;   (let ((xyy-lst nil))
;;     (dotimes (hue40 40 xyy-lst)
;;       (let ((max-c (clcl:max-chroma hue40 value)))
;; 	(dotimes (chroma max-c)
;; 	  (push (clcl:munsell-hvc-to-xyy hue40 value chroma)
;; 		xyy-lst))))))

(defun test-blue (lb)
  (apply #'xyz-to-lchab (lrgb-to-xyz 0 0 lb)))
