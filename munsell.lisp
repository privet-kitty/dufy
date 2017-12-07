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
    (format t "Mean Delta E = ~A~%" (funcall #'mean deltaes))
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
  (apply (rcurry #'lchab-to-xyy clcl:c)
	 (munsell-hvc-to-lchab hue40 value chroma)))

(defun munsell-hvc-to-xyz (hue40 value chroma)
  (apply (rcurry #'bradford clcl:c clcl:d65)
	 (apply #'xyy-to-xyz (munsell-hvc-to-xyy hue40 value chroma))))

(defun munsell-hvc-to2-xyz (hue40 value chroma)
  (apply (rcurry #'bradford clcl:c clcl:d65)
	 (apply (rcurry #'lchab-to-xyz clcl:c)
		(munsell-hvc-to-lchab hue40 value chroma))))

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

(defun munsell-hvc-to2-rgb255 (hue40 value chroma &key (threshold 0.001d0))
  (multiple-value-bind (rgb255 out-of-gamut)
      (apply (rcurry #'xyz-to-rgb255 :threshold threshold)
	     (munsell-hvc-to2-xyz hue40 value chroma))
    (if (and (= chroma 0))
	(values rgb255 nil)
	(values rgb255 out-of-gamut))))


(defun munsell-spec-to-hvc (spec)
  (destructuring-bind (hue-prefix value chroma)
      (mapcar #'read-from-string (cl-ppcre:split "[^0-9.a-z#\-]+" spec))
    (let* ((hue-name (cl-ppcre:scan-to-strings "[A-Z]+" spec))
	   (hue-number
	    (switch (hue-name :test #'string=)
	      ("R" 0) ("YR" 1) ("Y" 2) ("GY" 3) ("G" 4)
	      ("BG" 5) ("B" 6) ("PB" 7) ("P" 8) ("RP" 9)
	      (t (error "invalid spec")))))
      (list (+ (* hue-number 4) (/ (* hue-prefix 2) 5))
	    value
	    chroma))))

(defun munsell-hvc-to-spec (hue40 value chroma &optional (digits 2))
  (let* ((hue40$ (mod hue40 40d0))
	 (hue-number (floor (/ hue40$ 4)))
	 (hue-prefix (* (mod hue40$ 4) 2.5d0))
	 (hue-name (aref #("R" "YR" "Y" "GY" "G" "BG" "B" "PB" "P" "RP") hue-number))
	 (unit (concatenate 'string "~," (write-to-string digits) "F")))
    (format nil (concatenate 'string unit "~A " unit "/" unit)
	    hue-prefix hue-name value chroma)))

;; (clcl:munsell-spec-to-hvc "2.13d-2R .8999/   #x0f")
;; => (0.00852d0 0.8999 15)
;; (clcl:munsell-spec-to-hvc "2.13D-2R .8999/   #x0F")
;; => ERROR

;; return multiple values: (x, y, Y), out-of-macadam-limit-p
(defun munsell-spec-to-xyy (spec)
  (destructuring-bind (h v c) (munsell-spec-to-hvc spec)
    (if (> c (max-chroma h v))
	(values (list most-negative-single-float most-negative-single-float most-negative-single-float) t) ;out of MacAdam limit
	(values (funcall #'munsell-hvc-to-xyy h v c) nil))))

;; return multiple values: (x, y, Y), out-of-gamut-p
(defun munsell-spec-to-rgb255 (spec &key (threshold 0.0001d0))
  (destructuring-bind (nil v chroma) (munsell-spec-to-hvc spec)
    (multiple-value-bind (xyy out-of-macadam-limit) (munsell-spec-to-xyy spec)
      (if out-of-macadam-limit
	  (values (list -1 -1 -1) t)
	  (multiple-value-bind (rgb255 out-of-gamut) 
	      (apply #'(lambda (x y z) (xyz-to-rgb255 x y z :threshold threshold))
		     (apply (rcurry #'bradford clcl:c clcl:d65)
			    (apply #'xyy-to-xyz xyy)))
	    (if (and (= v 10) (= chroma 0))
		(values rgb255 nil)
		(values rgb255 out-of-gamut)))))))

