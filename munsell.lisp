(in-package :dufy)

;; the bradford transformation between D65 and C is frequently used here.
(declaim (type function d65-to-c c-to-d65))
(defparameter d65-to-c
  (gen-ca-converter illum-d65 illum-c))
(defparameter c-to-d65
  (gen-ca-converter illum-c illum-d65))

(defparameter *max-chroma-overall* 50
  "The largest chroma in the Munsell renotation data.
for devel.: (defparameter max-chroma-overall (apply #'max (mapcar #'third munsell-renotation-data)))")

(defmacro max-chroma-integer-case (hue40 value)
  `(aref max-chroma-arr ,hue40 ,value))

(defmacro max-chroma-integer-case-dark (hue40 dark-value)
  `(aref max-chroma-arr-dark ,hue40 ,dark-value))

(declaim (ftype (function * (integer 0 50)) max-chroma))
(defun max-chroma (hue40 value &key (use-dark t))
  "Returns the largest chroma which MHVC-TO- functions can receive.
The behavior of the MHVC-TO- functions is undefined, when chroma is larger than (MAC-CHROMA HUE40 VALUE)."
  (let* ((hue (mod hue40 40d0))
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
	(let* ((dark-value (* value 5d0))
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

;; (defun munsell-value-to-achromatic-xyy (v)
;;   (let* ((y (munsell-value-to-y v))
;; 	 (largex (+ (* 0.4124564d0 y) (* 0.3575761d0 y) (* 0.1804375d0 y)))
;; 	 (largey (+ (* 0.2126729d0 y) (* 0.7151522d0 y) (* 0.0721750d0 y)))
;; 	 (largez (+ (* 0.0193339d0 y) (* 0.1191920d0 y) (* 0.9503041d0 y))))
;;     (apply #'(lambda (x y largey) (list x y (clamp largey 0d0 1d0)))
;; 	   (apply #'xyz-to-xyy
;; 		  (funcall d65-to-c largex largey largez)))))

(defun munsell-value-to-achromatic-xyy (v)
  "Illuminant C"
  (list 0.31006d0 0.31616d0 (munsell-value-to-y v)))

(defun munsell-value-to-achromatic-lchab (v)
  (apply #'lab-to-lchab
	 (apply (rcurry #'xyz-to-lab illum-c)
		(xyy-to-xyz 0.31006d0
			    0.31616d0
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

(defun y-to-munsell-value (y)
  "Interpolates the inversion table of MUNSELL-VALUE-TO-Y linearly,
whose pitch width is 10^-3. The nominal range of Y is [0, 1]."
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
;; (defun mhvc-to-xyy-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let ((hue (mod hue40 40)))
;;     (if dark
;; 	(list (aref mrd-array-dark hue tmp-value half-chroma 0)
;; 	      (aref mrd-array-dark hue tmp-value half-chroma 1)
;; 	      (aref mrd-array-dark hue tmp-value half-chroma 2))
;; 	(list (aref mrd-array hue tmp-value half-chroma 0)
;; 	      (aref mrd-array hue tmp-value half-chroma 1)
;; 	      (aref mrd-array hue tmp-value half-chroma 2)))))


  
(defun mhvc-to-lchab-simplest-case (hue40 tmp-value half-chroma &optional (dark nil))
  "There are no type checks: e.g. HUE40 must be in {0, ...., 39}."
  (declare (optimize (speed 3) (safety 0))
	   (fixnum hue40 tmp-value half-chroma))
  (if dark
      (list (aref mrd-array-lchab-dark hue40 tmp-value half-chroma 0)
	    (aref mrd-array-lchab-dark hue40 tmp-value half-chroma 1)
	    (aref mrd-array-lchab-dark hue40 tmp-value half-chroma 2))
      (list (aref mrd-array-lchab hue40 tmp-value half-chroma 0)
	    (aref mrd-array-lchab hue40 tmp-value half-chroma 1)
	    (aref mrd-array-lchab hue40 tmp-value half-chroma 2))))

	 

;; (defun mhvc-to-xyy-value-chroma-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let* ((hue (mod hue40 40))
;; 	 (hue1 (floor hue))
;; 	 (hue2 (ceiling hue)))
;;     (cond ((= hue1 hue2)
;; 	   (mhvc-to-xyy-simplest-case (round hue) tmp-value half-chroma dark))
;; 	  ((or (zerop tmp-value) (zerop half-chroma))
;; 	   (munsell-value-to-achromatic-xyy (if dark (* tmp-value 0.2d0) tmp-value))) ;avoid division with zero
;; 	  (t 
;; 	   (destructuring-bind (x1 y1 largey)
;; 	       (mhvc-to-xyy-simplest-case hue1 tmp-value half-chroma dark)
;; 	     (destructuring-bind (x2 y2 disused)
;; 		 (mhvc-to-xyy-simplest-case hue2 tmp-value half-chroma dark)
;;             (declare (ignore disused))
;; 	       (destructuring-bind (r1 theta1)
;; 		   (xy-to-polar x1 y1)
;; 		 (destructuring-bind (r2 theta2)
;; 		     (xy-to-polar x2 y2)
;; 		   (let* ((theta (circular-lerp theta1 theta2 (- hue hue1)))
;; 			  (r (+ (* r1 (/ (subtract-with-mod theta2 theta)
;; 					 (subtract-with-mod theta2 theta1)))
;; 				(* r2 (/ (subtract-with-mod theta theta1)
;; 					 (subtract-with-mod theta2 theta1))))))
;; 		     (append (polar-to-xy r theta) (list largey)))))))))))


(defun mhvc-to-lchab-value-chroma-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
	   ((double-float 0d0 40d0) hue40)
	   (fixnum tmp-value half-chroma))
  (let* ((hue1 (floor hue40))
	 (hue2 (mod (ceiling hue40) 40)))
    (if (= hue1 hue2)
	(mhvc-to-lchab-simplest-case (round hue40) tmp-value half-chroma dark)
	(destructuring-bind (lstar cstarab1 hab1)
	    (mhvc-to-lchab-simplest-case hue1 tmp-value half-chroma dark)
	  (destructuring-bind (disused cstarab2 hab2)
	      (mhvc-to-lchab-simplest-case hue2 tmp-value half-chroma dark)
	    (declare (ignore disused)
		     (double-float lstar cstarab1 hab1 cstarab2 hab2))
	    (if (= hab1 hab2)
		(list lstar cstarab1 hab1)
		(let* ((hab (circular-lerp hab1 hab2 (- hue40 hue1) 360d0))
		       (cstarab (+ (* cstarab1 (/ (the double-float (subtract-with-mod hab2 hab 360d0))
						  (the double-float (subtract-with-mod hab2 hab1 360d0))))
				   (* cstarab2 (/ (the double-float (subtract-with-mod hab hab1 360d0))
						  (the double-float (subtract-with-mod hab2 hab1 360d0)))))))
		  (list lstar cstarab hab))))))))


;; (defun mhvc-to-xyy-value-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let ((hchroma1 (floor half-chroma))
;; 	(hchroma2 (ceiling half-chroma)))
;;     (if (= hchroma1 hchroma2)
;; 	(mhvc-to-xyy-value-chroma-integer-case hue40 tmp-value (round half-chroma) dark)
;; 	(destructuring-bind (x1 y1 largey)
;; 	    (mhvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma1 dark)
;; 	  (destructuring-bind (x2 y2 disused)
;; 	      (mhvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma2 dark)
;;          (declare (ignore disused))
;; 	    (let* ((x (+ (* x1 (- hchroma2 half-chroma))
;; 			 (* x2 (- half-chroma hchroma1))))
;; 		   (y (+ (* y1 (- hchroma2 half-chroma))
;; 			 (* y2 (- half-chroma hchroma1)))))
;; 	      (list x y largey)))))))


(defun mhvc-to-lchab-value-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
	   ((double-float 0d0 40d0) hue40 half-chroma)
	   (fixnum tmp-value))
  (let ((hchroma1 (floor half-chroma))
	(hchroma2 (ceiling half-chroma)))
    (if (= hchroma1 hchroma2)
	(mhvc-to-lchab-value-chroma-integer-case hue40 tmp-value (round half-chroma) dark)
	(destructuring-bind (lstar astar1 bstar1)
	    (apply #'lchab-to-lab
		   (mhvc-to-lchab-value-chroma-integer-case hue40 tmp-value hchroma1 dark))
	  (destructuring-bind (disused astar2 bstar2)
	      (apply #'lchab-to-lab
		     (mhvc-to-lchab-value-chroma-integer-case hue40 tmp-value hchroma2 dark))
	    (declare (ignore disused)
		     (double-float lstar astar1 bstar1 astar2 bstar2))
	    (let* ((astar (+ (* astar1 (- hchroma2 half-chroma))
			     (* astar2 (- half-chroma hchroma1))))
		   (bstar (+ (* bstar1 (- hchroma2 half-chroma))
			     (* bstar2 (- half-chroma hchroma1)))))
	      (lab-to-lchab lstar astar bstar)))))))

;; (defun mhvc-to-xyy-general-case (hue40 tmp-value half-chroma &optional (dark nil))
;;   (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
;;     (let  ((tmp-val1 (floor tmp-value))
;; 	   (tmp-val2 (ceiling tmp-value))
;; 	   (largey (munsell-value-to-y true-value)))
;;       (if (= tmp-val1 tmp-val2)
;; 	  (mhvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
;; 	  (destructuring-bind (x1 y1 largey1)
;; 	      (mhvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
;; 	    (destructuring-bind (x2 y2 largey2)
;; 		(mhvc-to-xyy-value-integer-case hue40 tmp-val2 half-chroma dark)
;; 	      (let* ((x (+ (* x1 (/ (- largey2 largey) (- largey2 largey1)))
;; 			   (* x2 (/ (- largey largey1) (- largey2 largey1)))))
;; 		     (y (+ (* y1 (/ (- largey2 largey) (- largey2 largey1)))
;; 			   (* y2 (/ (- largey largey1) (- largey2 largey1))))))
;; 		(list x y largey))))))))

(defun mhvc-to-lchab-general-case (hue40 tmp-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
	   ((double-float 0d0 40d0) hue40 half-chroma tmp-value))
  (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
    (let  ((tmp-val1 (floor tmp-value))
	   (tmp-val2 (ceiling tmp-value))
	   (lstar (munsell-value-to-lstar true-value)))
      (if (= tmp-val1 tmp-val2)
	  (mhvc-to-lchab-value-integer-case hue40 tmp-val1 half-chroma dark)
	  ;; If the given color is too dark to interpolate it,
	  ;; we use the fact that the chroma of LCH(ab) corresponds roughly
	  ;; to that of Munsell.
	  (if (= tmp-val1 0)
	      (destructuring-bind (disused cstarab hab)
		  (mhvc-to-lchab-value-integer-case hue40 1 half-chroma dark)
		(declare (ignore disused))
		(list lstar cstarab hab))
	      (destructuring-bind (lstar1 astar1 bstar1)
		  (apply #'lchab-to-lab
			 (mhvc-to-lchab-value-integer-case hue40 tmp-val1 half-chroma dark))
		(destructuring-bind (lstar2 astar2 bstar2)
		    (apply #'lchab-to-lab
			   (mhvc-to-lchab-value-integer-case hue40 tmp-val2 half-chroma dark))
		  (declare (double-float lstar lstar1 astar1 bstar1 lstar2 astar2 bstar2))
		  (let* ((astar (+ (* astar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
				   (* astar2 (/ (- lstar lstar1) (- lstar2 lstar1)))))
			 (bstar (+ (* bstar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
				   (* bstar2 (/ (- lstar lstar1) (- lstar2 lstar1))))))
		    (lab-to-lchab lstar astar bstar)))))))))

; Error:
; (dufy::mhvc-to2-xyy 0.9999999999999999d0 2.84d0 3d0)
; (dufy::mhvc-to-lchab-value-chroma-integer-case 0.9999999999999999d0 2 1)


(defun mhvc-out-of-mrd-p (hue40 value chroma)
  (or (> value 10) (< value 0)
      (> chroma (max-chroma hue40 value))))

(defun mhvc-to-lchab (hue40 value chroma)
  "CAUTION: The Standard Illuminant is C."
  (let ((d-hue (mod (float hue40 1d0) 40))
	(d-value (float value 1d0))
	(d-chroma (float chroma 1d0)))
    (if (mhvc-out-of-mrd-p d-hue d-value d-chroma)
	(error "Out of Munsell renotation data.")
	(if (>= value 1)
	    (mhvc-to-lchab-general-case d-hue d-value (/ d-chroma 2) nil)
	    (mhvc-to-lchab-general-case d-hue (* d-value 5) (/ d-chroma 2) t)))))

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
;; 			    (mhvc-to-xyy hue40 value chroma)))
;; 	       (lab2 (apply #'lchab-to-lab
;; 			    (mhvc-to-lchab hue40 value chroma)))
;; 	       (deltae (apply #'deltae (append lab1 lab2))))
;; 	  (unless (nth-value 1 (mhvc-to-lrgb hue40 value chroma :threshold 0))
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

;; (defun mhvc-to-lab (hue40 value chroma)
;;   (apply #'xyy-to-lab (mhvc-to-xyy hue40 value chroma)))

;; Illuminant C
;; (defun mhvc-to-lab (hue40 value chroma)
;;   (apply #'lchab-to-lab (mhvc-to-lchab hue40 value chroma)))

(defun mhvc-to-xyz-illum-c (hue40 value chroma)
  "Illuminant C. It doesn't produce errors by a chromatic adaptation,
since the Munsell Renotation Data is measured under the Illuminant C."
  (declare (optimize (speed 3) (safety 1)))
  (apply (the function (rcurry #'lchab-to-xyz illum-c))
	 (mhvc-to-lchab hue40 value chroma)))
  
				   
(defun mhvc-to-xyz (hue40 value chroma)
  "Illuminant D65. It produces errors by a chromatic adaptation,
since the Munsell Renotation Data is measured under the Illuminant C."
  (declare (optimize (speed 3) (safety 1)))
  (apply c-to-d65
	 (mhvc-to-xyz-illum-c hue40 value chroma)))

(defun mhvc-to-xyy (hue40 value chroma)
  "Illuminant D65"
  (apply #'xyz-to-xyy
	 (mhvc-to-xyz hue40 value chroma)))

;; (defun mhvc-to-xyz (hue40 value chroma)
;;   (apply c-to-d65
;; 	 (apply #'xyy-to-xyz (mhvc-to-xyy hue40 value chroma))))

(defun mhvc-to-lrgb (hue40 value chroma &key (rgbspace dufy:srgb) (threshold 0.001d0))
  "The standard illuminant is D65: that of RGBSPACE must also be D65.
It returns multiple values: (LR LG LB),  out-of-gamut-p.
Note that boundary colors, e.g. pure white (N 10.0), could be judged as out-of-gamut by numerical error, if threshold is too small."
  (apply (rcurry #'xyz-to-lrgb :rgbspace rgbspace :threshold threshold)
	 (mhvc-to-xyz hue40 value chroma)))

;; Unlike MHVC-TO-LRGB, all of achromatic colors are judged as within gamut.
;; return multiple values: (r g b),  out-of-gamut-p
;; (defun mhvc-to-rgb255 (hue40 value chroma &key (threshold 0.001d0))
;;   (multiple-value-bind (rgb255 out-of-gamut)
;;       (apply (rcurry #'xyz-to-rgb255 :threshold threshold)
;; 	     (mhvc-to-xyz hue40 value chroma))
;;     (if (and (= chroma 0))
;; 	(values rgb255 nil)
;; 	(values rgb255 out-of-gamut))))


(defun mhvc-to-rgb255 (hue40 value chroma &key (rgbspace dufy:srgb) (threshold 0.001d0))
  "The standard illuminant is D65: that of RGBSPACE must also be D65."
  (multiple-value-bind (rgb255 out-of-gamut)
      (apply (rcurry #'xyz-to-rgb255 :rgbspace rgbspace :threshold threshold)
	     (mhvc-to-xyz hue40 value chroma))
    (if (and (= chroma 0))
	(values rgb255 nil)
	(values rgb255 out-of-gamut))))


(defun munsell-to-mhvc (munsellspec)
  (let ((lst (mapcar #'read-from-string
		     (remove "" (cl-ppcre:split "[^0-9.a-z#\-]+" munsellspec) :test #'string=))))
    (let* ((hue-name (cl-ppcre:scan-to-strings "[A-Z]+" munsellspec))
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

(defun mhvc-to-munsell (hue40 value chroma &optional (digits 2))
  (let ((unit (concatenate 'string "~," (write-to-string digits) "F")))
    (if (< chroma (expt 0.09999999999999999d0 digits)) ; achromatic color
	(format nil (concatenate 'string "N " unit) value)
	(let* ((hue40$ (mod hue40 40d0))
	       (hue-number (floor (/ hue40$ 4)))
	       (hue-prefix (* (mod hue40$ 4) 2.5d0))
	       (hue-name (aref #("R" "YR" "Y" "GY" "G" "BG" "B" "PB" "P" "RP") hue-number)))
	  (format nil (concatenate 'string unit "~A " unit "/" unit)
		  hue-prefix hue-name value chroma)))))

;; (dufy:munsell-to-mhvc "2.13d-2R .8999/   #x0f")
;; => (0.00852d0 0.8999 15)
;; (dufy:munsell-to-mhvc "2.13D-2R .8999/   #x0F")
;; => ERROR

(defun munsell-out-of-mrd-p (munsellspec)
  (apply #'mhvc-out-of-mrd-p (munsell-to-mhvc munsellspec)))

(defun munsell-to-lchab (munsellspec)
  "return multiple values: (L* C*ab Hab), out-of-munsell-renotation-data-p.
Illuminant C."
  (destructuring-bind (hue40 value chroma) (munsell-to-mhvc munsellspec)
    (if (mhvc-out-of-mrd-p hue40 value chroma)
	(values (list most-negative-double-float most-negative-double-float most-negative-double-float)
		t)
	(values (funcall #'mhvc-to-lchab hue40 value chroma)
		nil))))

(defun munsell-to-xyz (munsellspec)
    "return multiple values: (X Y Z), out-of-munsell-renotation-data-p.
Illuminant D65."
  (destructuring-bind (hue40 value chroma) (munsell-to-mhvc munsellspec)
    (if (mhvc-out-of-mrd-p hue40 value chroma)
	(values (list most-negative-double-float most-negative-double-float most-negative-double-float)
		t)
	(values (funcall #'mhvc-to-xyz hue40 value chroma)
		nil))))


(defun munsell-to-xyy (munsellspec)
  "return multiple values: (x y Y), out-of-munsell-renotation-data-p.
Illuminant D65."
  (multiple-value-bind (xyz out-of-mrd)
      (munsell-to-xyz munsellspec)
    (values (apply #'xyz-to-xyy xyz)
	    out-of-mrd)))


(defun munsell-to-rgb255 (munsellspec &key (rgbspace dufy:srgb) (threshold 0.001d0))
  "return multiple values: (R G B), out-of-gamut-p.
The standard illuminant of RGBSPACE must be D65."
  (multiple-value-bind (xyz out-of-mrd) (munsell-to-xyz munsellspec)
    (if out-of-mrd
	(values (list -1 -1 -1) t)
	(multiple-value-bind (rgb255 out-of-gamut) 
	    (apply (rcurry #'xyz-to-rgb255 :rgbspace rgbspace :threshold threshold)
		   xyz)
	  (values rgb255 out-of-gamut)))))


; LCH(ab) value of maximum chroma boundary in MRD.
(defun max-chroma-lchab (hue40 value &key (use-dark t))
  (mhvc-to-lchab hue40
			value
			(max-chroma hue40 value :use-dark use-dark)))

; avoid that x slightly exceeds an integer 
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


;; used in INVERT-LCHAB-TO-MHVC
(defun rough-lchab-to-mhvc (lstar cstarab hab)
  (declare (optimize (speed 3) (safety 0))
	   (double-float lstar cstarab hab))
  (list (* hab #.(float 40/360 1d0))
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


(defun invert-mhvc-to-lchab-with-init (lstar cstarab hab init-hue40 init-chroma &key (max-iteration 200) (factor 0.5d0) (threshold 1d-6))
  "Illuminant C."
  (declare (optimize (speed 3) (safety 1)))
  (let ((cstarab (float cstarab 1d0))
	(hab (float hab 1d0))
	(factor (float factor 1d0))
	(threshold (float threshold 1d0))
	(tmp-hue40 (float init-hue40 1d0))
	(v (lstar-to-munsell-value lstar))
	(tmp-c (float init-chroma 1d0)))
    (declare (double-float tmp-hue40))
    (values-list
     (dotimes (i max-iteration (list (list (mod tmp-hue40 40) v tmp-c)
				     max-iteration))
       (if (> tmp-c (max-chroma tmp-hue40 v))
	   (return (list '(-1d0 -1d0 -1d0) -1)) ; exceeds max-chroma
	   (destructuring-bind (disused tmp-cstarab tmp-hab)
	       (mhvc-to-lchab tmp-hue40 v tmp-c)
	     (declare (ignore disused)
		      (double-float tmp-cstarab tmp-hab))
	     (let* ((delta-cstarab (- cstarab tmp-cstarab))
		    (delta-hab (circular-delta hab tmp-hab))
		    (delta-hue40 (* delta-hab #.(float 40/360 1d0)))
		    (delta-c (* delta-cstarab #.(/ 5.5d0))))
	       ;; (format t "hue40=~A chroma~A~%" tmp-hue40 tmp-c)
	       (if (and (<= (abs delta-hue40) threshold)
			(<= (abs delta-c) threshold))
		   (return (list (list (mod tmp-hue40 40d0) v tmp-c)
				 i))
		   (setf tmp-hue40 (+ tmp-hue40 (* factor delta-hue40))
			 tmp-c (+ tmp-c (* factor delta-c)))))))))))


;; An inverter of MHVC-TO-LCHAB with a simple iteration algorithm:
;; V := LSTAR-TO-MUNSELL-VALUE(LSTAR);
;; H_(n+1) := H_n + factor * delta(H_n);
;; C_(n+1) :=  C_n + factor * delta(C_n),
;; where delta(H_n) and delta(C_n) is internally calculated at every
;; step. H and C could diverge, if FACTOR is too large; the behavior is
;; undefined, if FACTOR >= 1. The return values are as follows:
;; 1. If max(delta(H_n), delta(C_n)) falls below THRESHOLD:
;; (H V C), NUMBER-OF-ITERATION.
;; 2. If the number of iteration exceeds MAX-ITERATION:
;; (H V C), MAX-ITERATION.
;; 3. If (H_n, V, C_n) goes out of the Munsell renotation data:
;; (-1.0d0 -1.0d0 -1.0d0), -1.
;; In other words: The inversion has failed, if the second value is equal
;; to MAX-ITERATION or -1.
;; 
;; BE CAREFUL: Illuminant C.
(defun lchab-to-mhvc (lstar cstarab hab &key (max-iteration 200) (factor 0.5d0) (threshold 1d-6))
  "Illuminant C."
  (let ((lstar (float lstar 1d0))
	(cstarab (float cstarab 1d0))
	(hab (float hab 1d0)))
    (destructuring-bind (init-h disused init-c)
	(rough-lchab-to-mhvc lstar cstarab hab)
      (declare (ignore disused))
      (invert-mhvc-to-lchab-with-init lstar cstarab hab
					     init-h init-c
					     :max-iteration max-iteration
					     :factor factor
					     :threshold threshold))))


(defun test-inverter ()
  (do ((lstar 0 (+ lstar 10)))
	     ((> lstar 100) 'done)
	   (do ((hab 0 (+ hab 9)))
	       ((= hab 360))
	     (loop for cstarab = 0 then (+ cstarab 10)
		  do (multiple-value-bind (lst number)
			 (dufy::lchab-to-mhvc lstar cstarab hab :threshold 1d-8 :max-iteration 500)
		       (declare (ignore lst))
		       (when (= number -1)
			 (format t "beyond MRD at L*=~A C*ab=~A Hab=~A.~%" lstar cstarab hab)
			 (return))
		       (when (= number 500)
			 (format t "failed at L*=~A C*ab=~A Hab=~A.~%" lstar cstarab hab)))))))
