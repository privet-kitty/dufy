(require :sketch)
(require :clcl)
(defpackage experiment (:use :cl :clcl :sketch))
(in-package :experiment)

(defparameter w 600)
(defparameter h 600)

(defsketch test ((width w) (height h)))
(make-instance 'test)

(defun dotter (x y &optional (col +black+) (size 1))
  (let* ((half-w (* w 0.5d0))
	 (half-h (* h 0.5d0))
	 (coor-x (+ half-w (* (- x 0.31006d0) w 1.5d0)))
	 (coor-y (- half-h (* (- y 0.31616d0) h 1.5d0))))
    (with-pen (make-pen :fill col :stroke col :weight 0)
      (circle coor-x coor-y size))))

(defun dist (x y)
  (sqrt (+ (* (- x 0.31006d0) (- x 0.31006d0)) (* (- y 0.31616d0) (- y 0.31616d0)))))

(defun draw-isovalue-plane (value &optional (chroma-start 0.9d0) (col +red+) (step 0.1d0) (huestart 0d0) (hueend 40d0))
  (do ((hue40 huestart (+ hue40 step)))
      ((>= hue40 hueend))
    (let ((mchroma (max-chroma hue40 value)))
      (do ((chroma chroma-start (+ chroma 1d0)))
	  ((>= chroma mchroma))
	;(unless (nth-value 1 (clcl:munsell-hvc-to-lrgb hue40 value chroma :threshold 0))
	  (destructuring-bind (x y nil) (clcl::munsell-hvc-to2-xyy hue40 value chroma)
	    (dotter x y col))
	;  )
	))))

(defun hvc-dotter (hue40 value chroma &optional (col +blue+))
  (destructuring-bind (x y nil)
      (clcl::munsell-hvc-to2-xyy hue40 value chroma)
    (dotter x y col 3)))

(defsketch test ((width w) (height h))
  (with-pen (make-pen :weight 2  :stroke +red+)
    (background +white+)
    
    (draw-isovalue-plane 1.24d0 2d0 +red+ 0.2d0)
    (dotter 0.31006 0.31616 +black+ 2)
    ;(hvc-dotter (+ 0.7d0 0.1d0 0.1d0 0.1d0) 2.84d0 3d0)
))

