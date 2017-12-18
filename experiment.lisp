(require :sketch)
(require :dufy)
(defpackage experiment (:use :cl :dufy :sketch))
(in-package :experiment)

(defparameter w 600)
(defparameter h 600)

(defsketch test ((width w) (height h)))
(make-instance 'test)

(defun dotter (x y &optional (col +black+) (size 1))
  (let* ((half-w (* w 0.8d0))
	 (half-h (* h 0d0))
	 (zoom 3.5d0)
	 (coor-x (+ half-w (* (- x 0.31006d0) w zoom)))
	 (coor-y (- half-h (* (- y 0.31616d0) h zoom))))
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
	(unless (nth-value 1 (dufy:munsell-hvc-to-lrgb hue40 value chroma :threshold 0))
	  (destructuring-bind (x y nil) (dufy::munsell-hvc-to-xyy hue40 value chroma)
	    (dotter x y col))
	)
	))))

(defun hvc-dotter (hue40 value chroma &optional (col +blue+))
  (destructuring-bind (x y nil)
      (dufy::munsell-hvc-to-xyy hue40 value chroma)
    (dotter x y col 3)))

; y 3.853322597739668d0 = munsell-value 3.853322597739668d0
; xyyd65 (0.15917868650000358d0 0.093043271400013d0 0.10786004607913602d0)
; xyzd65 (0.18452726567290856d0 0.10786004607913602d0 0.8668587514632364d0)
; = 17908 = srgbd65(0 69 244) =>
; hvc(30.8 4.2 21.6) => srgbd65(0 83 244)
; = xyzd65(0.1937530066846454d0 0.1269639766102692d0 0.8698832319255377d0)
; = xyyd65(0.16273557169546618d0 0.10663863065635383d0 0.1269639766102692d0)
(defsketch test ((width w) (height h))
  (with-pen (make-pen :weight 2  :stroke +red+)
    (let ((value 3.853322597739668d0))
      (background +white+)
      (draw-isovalue-plane value 2d0 +red+ 0.5 23 37)
      (dotter 0.31006 0.31616 +black+ 2) ;center
      (dotter 0.15917868650000358d0 0.093043271400013d0 +blue+ 3)
      (hvc-dotter 31 value 22.9 +green+)
)))

