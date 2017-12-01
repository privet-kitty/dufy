(in-package :clcl)

;; (require :cl-ppcre)

(defparameter mrd-filename "munsell-renotation-data.lisp")
(defparameter mrd-pathname (merge-pathnames mrd-filename *default-pathname-defaults*))

;; the largest chroma in the Munsell renotation data
;; (defparameter max-chroma-overall (apply #'max (mapcar #'third munsell-renotation-data)))
(defparameter max-chroma-overall 50)

(defun max-chroma-integer-case (hue40 value)
  (aref max-chroma-arr hue40 value))

(defun max-chroma-integer-case-dark (hue40 dark-value)
  (aref max-chroma-arr-dark hue40 dark-value))

;; max-chroma returns the largest chroma which the hvc-to- functions can take.
;; The behavior of the hvc-to- functions is undefined, when chroma is larger than (max-chroma hue value)
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
(defun value-to-y (v)
  (* v (+ 1.1914 (* v (+ -0.22533 (* v (+ 0.23352 (* v (+ -0.020484 (* v 0.00081939)))))))) 0.01))
  
(defun value-to-achromatic-srgb (v)
  (let ((x (round (* (clcl:delinearize (value-to-y v)) 255))))
    (list x x x)))

(defun value-to-achromatic-xyy (v)
  (let* ((y (value-to-y v))
	 (xyzd65 (list (+ (* 0.4124564 y) (* 0.3575761 y) (* 0.1804375 y))
		       (+ (* 0.2126729 y) (* 0.7151522 y) (* 0.0721750 y))
		       (+ (* 0.0193339 y) (* 0.1191920 y) (* 0.9503041 y)))))
    (apply #'(lambda (x y largey) (list x y (clcl:bound largey 0 1)))
	   (apply #'clcl:xyz-to-xyy (funcall #'clcl:bradford xyzd65 clcl:d65 clcl:c)))))

;; the version corresponding with Y of the munsell renotation data
;; v must be integer.
;; nearly equal to value-to-achromatic-xyy

(defun value-to-achromatic-xyy-from-mrd (v)
  (list 0.31006 0.31616
	(clcl:bound (* (aref (vector 0 0.0121 0.03126 0.0655 0.120 0.1977 0.3003 0.4306 0.591 0.7866 1.0257) v) 0.975)
	       0 1)))

;; y should be in [0,1]
(defun y-to-value (y)
  (let* ((y1000 (* (clcl:bound y 0 1) 1000))
	 (y1 (floor y1000))
	 (y2 (ceiling y1000)))
    (if (= y1 y2)
	(aref y-to-value-arr y1)
	(let ((r (- y1000 y1)))
	  (+ (* (- 1 r) (aref y-to-value-arr y1))
	     (* r (aref y-to-value-arr y2)))))))


;; hue ∈ Z/40, tmp-value ∈ {0, 1, ..., 10}, half-chroma ∈ {0, 1, ..., max-chroma/2}
;; If dark is t, tmp-value ∈ {0, 1, 2, 3, 4 ,5} and treated as {0, 0.2, ...., 1.0}.
(defun hvc-to-xyy-simplest-case (hue40 tmp-value half-chroma dark)
  (let ((hue (mod hue40 40)))
    (if dark
	(list (aref mrd-array-dark hue tmp-value half-chroma 0)
	      (aref mrd-array-dark hue tmp-value half-chroma 1)
	      (aref mrd-array-dark hue tmp-value half-chroma 2))
	(list (aref mrd-array hue tmp-value half-chroma 0)
	      (aref mrd-array hue tmp-value half-chroma 1)
	      (aref mrd-array hue tmp-value half-chroma 2)))))

(defun hvc-to-lrgb-simplest-case (hue value half-chroma)
  (apply #'clcl:xyz-to-lrgb
	 (clcl:bradford (apply #'clcl:xyy-to-xyz
			       (hvc-to-xyy-simplest-case hue value half-chroma nil))
			clcl:c clcl:d65)))

(defun hvc-to-xyy-value-chroma-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let* ((hue (mod hue40 40))
	 (hue1 (floor hue))
	 (hue2 (ceiling hue)))
    (cond ((= hue1 hue2)
	   (hvc-to-xyy-simplest-case (round hue) tmp-value half-chroma dark))
	  ((or (zerop tmp-value) (zerop half-chroma))
	   (value-to-achromatic-xyy (if dark (* tmp-value 0.2d0) tmp-value))) ;avoid division with zero
	  (t 
	   (destructuring-bind (x1 y1 lum)
	       (hvc-to-xyy-simplest-case hue1 tmp-value half-chroma dark)
	     (destructuring-bind (x2 y2 nil)
		 (hvc-to-xyy-simplest-case hue2 tmp-value half-chroma dark)
	       (destructuring-bind (r1 theta1)
		   (clcl:xy-to-polar x1 y1)
		 (destructuring-bind (r2 theta2)
		     (clcl:xy-to-polar x2 y2)
		   (let* ((theta (clcl:interpolate-in-circle-group theta1 theta2 (- hue hue1)))
			  (r (+ (* r1 (/ (clcl:subtract-with-mod theta2 theta) (clcl:subtract-with-mod theta2 theta1)))
				(* r2 (/ (clcl:subtract-with-mod theta theta1) (clcl:subtract-with-mod theta2 theta1))))))
		     (append (clcl:polar-to-xy r theta) (list lum)))))))))))

(defun hvc-to-xyy-value-integer-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((hchroma1 (floor half-chroma))
	(hchroma2 (ceiling half-chroma)))
    (if (= hchroma1 hchroma2)
	(hvc-to-xyy-value-chroma-integer-case hue40 tmp-value (round half-chroma) dark)
	(destructuring-bind (x1 y1 lum)
	    (hvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma1 dark)
	  (destructuring-bind (x2 y2 nil)
	      (hvc-to-xyy-value-chroma-integer-case hue40 tmp-value hchroma2 dark)
	    (let* ((x (+ (* x1 (- hchroma2 half-chroma))
			 (* x2 (- half-chroma hchroma1))))
		   (y (+ (* y1 (- hchroma2 half-chroma))
			 (* y2 (- half-chroma hchroma1)))))
	      (list x y lum)))))))

(defun hvc-to-xyy-general-case (hue40 tmp-value half-chroma &optional (dark nil))
  (let ((true-value (if dark (* tmp-value 0.2d0) tmp-value)))
    (let  ((tmp-val1 (floor tmp-value))
	   (tmp-val2 (ceiling tmp-value))
	   (lum (value-to-y true-value)))
      (if (= tmp-val1 tmp-val2)
	  (hvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
	  (destructuring-bind (x1 y1 lum1)
	      (hvc-to-xyy-value-integer-case hue40 tmp-val1 half-chroma dark)
	    (destructuring-bind (x2 y2 lum2)
		(hvc-to-xyy-value-integer-case hue40 tmp-val2 half-chroma dark)
	      (let* ((x (+ (* x1 (/ (- lum2 lum) (- lum2 lum1)))
			   (* x2 (/ (- lum lum1) (- lum2 lum1)))))
		     (y (+ (* y1 (/ (- lum2 lum) (- lum2 lum1)))
			   (* y2 (/ (- lum lum1) (- lum2 lum1))))))
		(list x y lum))))))))

(defun hvc-to-xyy (hue40 value chroma)
  (if (>= value 1)
      (hvc-to-xyy-general-case hue40 value (/ chroma 2) nil)
      (hvc-to-xyy-general-case hue40 (* value 5) (/ chroma 2) t)))

(defun hvc-to-lrgb (hue40 value chroma)
  (apply #'clcl:xyz-to-lrgb
	 (clcl:bradford (apply #'clcl:xyy-to-xyz
			       (hvc-to-xyy hue40 value chroma))
			clcl:c clcl:d65)))

;; return multiple values: (r g b),  out-of-gamut-p
(defun hvc-to-srgb (hue40 value chroma &key (threshold 0.0001d0))
  (multiple-value-bind (srgb out-of-gamut)
      (destructuring-bind (x y z)
	  (clcl:bradford (apply #'clcl:xyy-to-xyz
				(hvc-to-xyy hue40 value chroma))
			 clcl:c clcl:d65)
	  (clcl:xyz-to-srgb x y z :threshold threshold))
    (if (and (= value 10) (= chroma 0)) ;pure white (N 10.0) be in gamut
	(values srgb nil)
	(values srgb out-of-gamut))))

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
	(values (funcall #'hvc-to-xyy h v c) nil))))

;; return multiple values: (x, y, Y), out-of-gamut-p
(defun munsellspec-to-srgb (spec &key (threshold 0.0001d0))
  (destructuring-bind (nil v chroma) (munsellspec-to-hvc spec)
    (multiple-value-bind (xyy out-of-macadam-limit) (munsellspec-to-xyy spec)
      (if out-of-macadam-limit
	  (values (list -1 -1 -1) t)
	  (multiple-value-bind (srgb out-of-gamut) 
	      (apply #'(lambda (x y z) (clcl:xyz-to-srgb x y z :threshold threshold))
		     (clcl:bradford (apply #'clcl:xyy-to-xyz xyy) clcl:c clcl:d65))
	    (if (and (= v 10) (= chroma 0))
		(values srgb nil)
		(values srgb out-of-gamut)))))))
