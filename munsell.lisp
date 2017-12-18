(in-package :dufy)

;; the bradford transformation D65 <-> C is frequently used here.
(defparameter d65-to-c
  (gen-ca-converter illum-d65 illum-c))
(defparameter c-to-d65
  (gen-ca-converter illum-c illum-d65))

(defparameter *max-chroma-overall* 50
  "The largest chroma in the Munsell renotation data.
for devel.: (defparameter max-chroma-overall (apply #'max (mapcar #'third munsell-renotation-data)))")

(defun max-chroma-integer-case (hue40 value)
  (aref max-chroma-arr hue40 value))

(defun max-chroma-integer-case-dark (hue40 dark-value)
  (aref max-chroma-arr-dark hue40 dark-value))

(defun max-chroma (hue40 value &key (use-dark t))
  "returns the largest chroma which the MUNSELL-HVC-TO- functions can receive.
The behavior of the MUNSELL-HVC-TO- functions is undefined, when chroma is larger than (MAC-CHROMA HUE40 VALUE)."
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
		  (funcall d65-to-c largex largey largez)))))

(defun munsell-value-to-achromatic-lchab (v)
  (apply #'lab-to-lchab
	 (apply (rcurry #'xyz-to-lab illum-c)
		(xyy-to-xyz (illuminant-x illum-c)
			    (illuminant-y illum-c)
			    (munsell-value-to-y v)))))

;; the version corresponding with Y of the munsell renotation data
;; v must be integer.
;; nearly equal to munsell-value-to-achromatic-xyy

(defun munsell-value-to-achromatic-xyy-from-mrd (v)
  (list 0.31006d0 0.31616d0
	(clamp (* (aref (vector 0d0 0.0121d0 0.03126d0 0.0655d0 0.120d0 0.1977d0 0.3003d0 0.4306d0 0.591d0 0.7866d0 1.0257d0)
			v)
		  0.975d0)
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
;; (defun munsell-hvc-to-xyy-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let ((hue (mod hue40 40)))
;;     (if dark
;; 	(list (aref mrd-array-dark hue tmp-value half-chroma 0)
;; 	      (aref mrd-array-dark hue tmp-value half-chroma 1)
;; 	      (aref mrd-array-dark hue tmp-value half-chroma 2))
;; 	(list (aref mrd-array hue tmp-value half-chroma 0)
;; 	      (aref mrd-array hue tmp-value half-chroma 1)
;; 	      (aref mrd-array hue tmp-value half-chroma 2)))))


;; (defun munsell-hvc-to-lrgb-simplest-case (hue value half-chroma)
;;   (apply #'xyz-to-lrgb
;; 	 (apply c-to-d65
;; 		(apply #'xyy-to-xyz
;; 		       (munsell-hvc-to-xyy-simplest-case hue value half-chroma nil)))))

;; CAUTION: This LCH(ab) values are under Illuminant C.
;; (defun munsell-hvc-to-lchab-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (apply #'(lambda (lstar cstarab hab) (list (clamp lstar 0d0 100d0) cstarab hab))
;; 	 (apply #'lab-to-lchab
;; 		(apply (rcurry #'xyy-to-lab illum-c)
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

	 

;; (defun munsell-hvc-to-xyy-value-chroma-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let* ((hue (mod hue40 40))
;; 	 (hue1 (floor hue))
;; 	 (hue2 (ceiling hue)))
;;     (cond ((= hue1 hue2)
;; 	   (munsell-hvc-to-xyy-simplest-case (round hue) tmp-value half-chroma dark))
;; 	  ((or (zerop tmp-value) (zerop half-chroma))
;; 	   (munsell-value-to-achromatic-xyy (if dark (* tmp-value 0.2d0) tmp-value))) ;avoid division with zero
;; 	  (t 
;; 	   (destructuring-bind (x1 y1 largey)
;; 	       (munsell-hvc-to-xyy-simplest-case hue1 tmp-value half-chroma dark)
;; 	     (destructuring-bind (x2 y2 nil)
;; 		 (munsell-hvc-to-xyy-simplest-case hue2 tmp-value half-chroma dark)
;; 	       (destructuring-bind (r1 theta1)
;; 		   (xy-to-polar x1 y1)
;; 		 (destructuring-bind (r2 theta2)
;; 		     (xy-to-polar x2 y2)
;; 		   (let* ((theta (lerp-in-circle-group theta1 theta2 (- hue hue1)))
;; 			  (r (+ (* r1 (/ (subtract-with-mod theta2 theta)
;; 					 (subtract-with-mod theta2 theta1)))
;; 				(* r2 (/ (subtract-with-mod theta theta1)
;; 					 (subtract-with-mod theta2 theta1))))))
;; 		     (append (polar-to-xy r theta) (list largey)))))))))))

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


;; (defun munsell-hvc-to-xyy-value-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let ((hchroma1 (floor half-chroma))
;; 	(hchroma2 (ceiling half-chroma)))
;;     (if (= hchroma1 hchroma2)
;; 	(munsell-hvc-to-xyy-value-chroma-integer-case hue40 tmp-value (round half-chroma) dark)
;; 	(destructuring-bind (x1 y1 largey)
;; 	    (munsell-hvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma1 dark)
;; 	  (destructuring-bind (x2 y2 nil)
;; 	      (munsell-hvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma2 dark)
;; 	    (let* ((x (+ (* x1 (- hchroma2 half-chroma))
;; 			 (* x2 (- half-chroma hchroma1))))
;; 		   (y (+ (* y1 (- hchroma2 half-chroma))
;; 			 (* y2 (- half-chroma hchroma1)))))
;; 	      (list x y largey)))))))


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

;; (defun munsell-hvc-to-xyy-general-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
;;     (let  ((tmp-val1 (floor tmp-value))
;; 	   (tmp-val2 (ceiling tmp-value))
;; 	   (largey (munsell-value-to-y true-value)))
;;       (if (= tmp-val1 tmp-val2)
;; 	  (munsell-hvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
;; 	  (destructuring-bind (x1 y1 largey1)
;; 	      (munsell-hvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
;; 	    (destructuring-bind (x2 y2 largey2)
;; 		(munsell-hvc-to-xyy-value-integer-case hue40 tmp-val2 half-chroma dark)
;; 	      (let* ((x (+ (* x1 (/ (- largey2 largey) (- largey2 largey1)))
;; 			   (* x2 (/ (- largey largey1) (- largey2 largey1)))))
;; 		     (y (+ (* y1 (/ (- largey2 largey) (- largey2 largey1)))
;; 			   (* y2 (/ (- largey largey1) (- largey2 largey1))))))
;; 		(list x y largey))))))))

(defun munsell-hvc-to-lchab-general-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
    (let  ((tmp-val1 (floor tmp-value))
	   (tmp-val2 (ceiling tmp-value))
	   (lstar (munsell-value-to-lstar true-value)))
      (if (= tmp-val1 tmp-val2)
	  (munsell-hvc-to-lchab-value-integer-case hue40 tmp-val1 half-chroma dark)
	  ; If the given color is too dark to interpolate it,
	  ; we use the fact that the chroma of LCH(ab) corresponds roughly
	  ; to that of Munsell.
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
; (dufy::munsell-hvc-to2-xyy 0.9999999999999999d0 2.84d0 3d0)
; (dufy::munsell-hvc-to-lchab-value-chroma-integer-case 0.9999999999999999d0 2 1)

; CAUTION: the Standard Illuminant is C
;; (defun munsell-hvc-to-xyy (hue40 value chroma)
;;   (if (>= value 1)
;;       (munsell-hvc-to-xyy-general-case hue40 value (/ chroma 2) nil)
;;       (munsell-hvc-to-xyy-general-case hue40 (* value 5) (/ chroma 2) t)))


(defun munsell-hvc-out-of-mrd-p (hue40 value chroma)
  (or (> value 10) (< value 0)
      (> chroma (max-chroma hue40 value))))

(defun munsell-hvc-to-lchab (hue40 value chroma)
  "CAUTION: The Standard Illuminant is C."
  (if (munsell-hvc-out-of-mrd-p hue40 value chroma)
      (error "Out of Munsell renotation data.")
      (if (>= value 1)
	  (munsell-hvc-to-lchab-general-case hue40 value (/ chroma 2) nil)
	  (munsell-hvc-to-lchab-general-case hue40 (* value 5) (/ chroma 2) t))))

;; (defun compare-munsell-converter (mc)
;;   (let ((deltaes nil)
;; 	(max-deltae 0)
;; 	(outlier nil)
;; 	(number-within-gamut 0))
;;     (dotimes (x mc)
;;       (let* ((hue40 (random 40d0))
;; 	     (value (+ 0.2d0 (random 9.8d0)))
;; 	     (chroma (random (coerce (max-chroma hue40 value) 'double-float))))
;; 	(let* ((lab1 (apply (rcurry #'xyy-to-lab illum-c)
;; 			    (munsell-hvc-to-xyy hue40 value chroma)))
;; 	       (lab2 (apply #'lchab-to-lab
;; 			    (munsell-hvc-to-lchab hue40 value chroma)))
;; 	       (deltae (apply #'deltae (append lab1 lab2))))
;; 	  (unless (nth-value 1 (munsell-hvc-to-lrgb hue40 value chroma :threshold 0))
;; 	    (incf number-within-gamut)
;; 	    (when (> deltae max-deltae)
;; 	      (setf max-deltae deltae)
;; 	      (setf outlier (list hue40 value chroma)))
;; 	    (push deltae deltaes)))))
;;     (format t "Processed colors within sRGB(d65) gamut = ~A~%" number-within-gamut)
;;     (format t "Mean Delta E = ~A~%" (funcall #'mean deltaes))
;;     (format t "Maximum Delta E = ~A~%" (apply #'max deltaes))
;;     (format t "Outlier (H, V, C) = ~A~%" outlier)))

;; (dufy::compare-munsell-converter 100000)
;; =>
;; Processed colors within sRGB(d65) gamut = 48480
;; Mean Delta E = 0.35641873193747986d0
;; Maximum Delta E = 4.67977844149827d0
;; Outlier (H, V, C) = (15.07830806002132d0 1.4835458770925156d0
;;                      5.06906007523528d0)

;; (defun munsell-hvc-to-lab (hue40 value chroma)
;;   (apply #'xyy-to-lab (munsell-hvc-to-xyy hue40 value chroma)))

;; Illuminant C
;; (defun munsell-hvc-to-lab (hue40 value chroma)
;;   (apply #'lchab-to-lab (munsell-hvc-to-lchab hue40 value chroma)))

(defun munsell-hvc-to-xyz (hue40 value chroma)
  "Illuminant D65"
  (apply c-to-d65
	 (apply (rcurry #'lchab-to-xyz illum-c)
		(munsell-hvc-to-lchab hue40 value chroma))))

(defun munsell-hvc-to-xyy (hue40 value chroma)
  "Illuminant D65"
  (apply #'xyz-to-xyy
	 (munsell-hvc-to-xyz hue40 value chroma)))

;; (defun munsell-hvc-to-xyz (hue40 value chroma)
;;   (apply c-to-d65
;; 	 (apply #'xyy-to-xyz (munsell-hvc-to-xyy hue40 value chroma))))

(defun munsell-hvc-to-lrgb (hue40 value chroma &key (rgbspace dufy:srgb) (threshold 0.001d0))
  "The standard illuminant is D65: that of RGBSPACE must also be D65.
It returns multiple values: (LR LG LB),  out-of-gamut-p.
Note that boundary colors, e.g. pure white (N 10.0), could be judged as out-of-gamut by numerical error, if threshold is too small."
  (apply (rcurry #'xyz-to-lrgb :rgbspace rgbspace :threshold threshold)
	 (munsell-hvc-to-xyz hue40 value chroma)))

;; Unlike MUNSELL-HVC-TO-LRGB, all of achromatic colors are judged as within gamut.
;; return multiple values: (r g b),  out-of-gamut-p
;; (defun munsell-hvc-to-rgb255 (hue40 value chroma &key (threshold 0.001d0))
;;   (multiple-value-bind (rgb255 out-of-gamut)
;;       (apply (rcurry #'xyz-to-rgb255 :threshold threshold)
;; 	     (munsell-hvc-to-xyz hue40 value chroma))
;;     (if (and (= chroma 0))
;; 	(values rgb255 nil)
;; 	(values rgb255 out-of-gamut))))


(defun munsell-hvc-to-rgb255 (hue40 value chroma &key (rgbspace dufy:srgb) (threshold 0.001d0))
  "The standard illuminant is D65: that of RGBSPACE must also be D65."
  (multiple-value-bind (rgb255 out-of-gamut)
      (apply (rcurry #'xyz-to-rgb255 :rgbspace rgbspace :threshold threshold)
	     (munsell-hvc-to-xyz hue40 value chroma))
    (if (and (= chroma 0))
	(values rgb255 nil)
	(values rgb255 out-of-gamut))))


(defun munsell-spec-to-hvc (spec)
  (let ((lst (mapcar #'read-from-string
		     (remove "" (cl-ppcre:split "[^0-9.a-z#\-]+" spec) :test #'string=))))
    (let* ((hue-name (cl-ppcre:scan-to-strings "[A-Z]+" spec))
	   (hue-number
	    (switch (hue-name :test #'string=)
	      ("R" 0) ("YR" 1) ("Y" 2) ("GY" 3) ("G" 4)
	      ("BG" 5) ("B" 6) ("PB" 7) ("P" 8) ("RP" 9) ("N" -1)
	      (t (error "invalid spec")))))
      (if (< hue-number 0)
	  (list 0 (car lst) 0)
	  (progn
	    (setf (car lst) (+ (* hue-number 4) (/ (* (car lst) 2) 5)))
	    lst)))))

(defun munsell-hvc-to-spec (hue40 value chroma &optional (digits 2))
  (let ((unit (concatenate 'string "~," (write-to-string digits) "F")))
    (if (< chroma (expt 0.09999999999999999d0 digits)) ; achromatic color
	(format nil (concatenate 'string "N " unit) value)
	(let* ((hue40$ (mod hue40 40d0))
	       (hue-number (floor (/ hue40$ 4)))
	       (hue-prefix (* (mod hue40$ 4) 2.5d0))
	       (hue-name (aref #("R" "YR" "Y" "GY" "G" "BG" "B" "PB" "P" "RP") hue-number)))
	  (format nil (concatenate 'string unit "~A " unit "/" unit)
		  hue-prefix hue-name value chroma)))))

;; (dufy:munsell-spec-to-hvc "2.13d-2R .8999/   #x0f")
;; => (0.00852d0 0.8999 15)
;; (dufy:munsell-spec-to-hvc "2.13D-2R .8999/   #x0F")
;; => ERROR

(defun munsell-spec-out-of-mrd-p (spec)
  (apply #'munsell-hvc-out-of-mrd-p (munsell-spec-to-hvc spec)))

(defun munsell-spec-to-lchab (spec)
  "return multiple values: (L* C*ab Hab), out-of-munsell-renotation-data-p.
Illuminant C."
  (destructuring-bind (hue40 value chroma) (munsell-spec-to-hvc spec)
    (if (munsell-hvc-out-of-mrd-p hue40 value chroma)
	(values (list most-negative-double-float most-negative-double-float most-negative-double-float)
		t)
	(values (funcall #'munsell-hvc-to-lchab hue40 value chroma)
		nil))))

(defun munsell-spec-to-xyz (spec)
    "return multiple values: (X Y Z), out-of-munsell-renotation-data-p.
Illuminant D65."
  (destructuring-bind (hue40 value chroma) (munsell-spec-to-hvc spec)
    (if (munsell-hvc-out-of-mrd-p hue40 value chroma)
	(values (list most-negative-double-float most-negative-double-float most-negative-double-float)
		t)
	(values (funcall #'munsell-hvc-to-xyz hue40 value chroma)
		nil))))


(defun munsell-spec-to-xyy (spec)
  "return multiple values: (x y Y), out-of-munsell-renotation-data-p.
Illuminant D65."
  (multiple-value-bind (xyz out-of-mrd)
      (munsell-spec-to-xyz spec)
    (values (apply #'xyz-to-xyy xyz)
	    out-of-mrd)))


(defun munsell-spec-to-rgb255 (spec &key (rgbspace dufy:srgb) (threshold 0.001d0))
  "return multiple values: (R G B), out-of-gamut-p.
The standard illuminant of RGBSPACE must be D65."
  (multiple-value-bind (xyz out-of-mrd) (munsell-spec-to-xyz spec)
    (if out-of-mrd
	(values (list -1 -1 -1) t)
	(multiple-value-bind (rgb255 out-of-gamut) 
	    (apply (rcurry #'xyz-to-rgb255 :rgbspace rgbspace :threshold threshold)
		   xyz)
	  (values rgb255 out-of-gamut)))))


; LCH(ab) value of maximum chroma boundary in MRD.
(defun max-chroma-lchab (hue40 value &key (use-dark t))
  (munsell-hvc-to-lchab hue40
			value
			(max-chroma hue40 value :use-dark use-dark)))

; avoid that x exceeds an integer slightly 
(defun modify-float-error (x epsilon)
  (if (<= (- x (floor x)) epsilon)
      (floor x)
      x))
			   
;; (defun find-hue40-floor-with-max-chroma (lstar hab)
;;   (let* ((inf 0)
;; 	 (sup 40)
;; 	 (value (lstar 
;; 	 (lchab-inf (list lstar
;; 			  (max-chroma-lchab inf (
;;     (loop
;; 	 (if (<= (- sup inf) 1)
;; 	     (return inf)
;; 	     (let ((mid (round (/ (+ sup inf) 2))))
	       
	       
	   
(defun rough-munsell-hue-to-hab (h)
  (mod (* h 9) 360))

(defun rough-munsell-chroma-to-cstarab (c)
  (* c 5))

(defun lstar-to-munsell-value (lstar)
  (y-to-munsell-value (lstar-to-y lstar)))

;; (defun lchab-to-hvc-in-block (lstar cstarab hab hue40-rb ch-rb hue40-lb ch-lb hue40-rt ch-rt hue40-lt ch-lt)
;;   (let ((munsell-v (lstar-to-munsell-value lstar)))
;;     (destructuring-bind (nil cstarab-rb hab-rb)
;; 	(munsell-hvc-to-lchab hue40-rb munsell-v ch-rb)
;;       (destructuring-bind (nil cstarab-lb hab-lb)
;; 	  (munsell-hvc-to-lchab hue40-lb munsell-v ch-lb)
;; 	(destructuring-bind (nil cstarab-rt hab-rt)
;; 	    (munsell-hvc-to-lchab hue40-rt munsell-v ch-rt)
;; 	  (destructuring-bind (nil cstarab-lt hab-lt)
;; 	      (munsell-hvc-to-lchab hue40-lt munsell-v ch-lt)
