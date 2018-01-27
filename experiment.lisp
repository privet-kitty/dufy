;;
;; Experiment in development.
;;


;; Isochroma maps
(require :sketch)
(require :dufy)
(defpackage experiment (:use :cl :dufy :sketch))
(in-package :experiment)

(defparameter w 600)
(defparameter h 600)

(defsketch test ((width w) (height h)))
(make-instance 'test)

(defun dotter (x y &optional (col +black+) (size 1))
  (let* ((half-w (* w 0.5d0))
	 (half-h (* h 0.5d0))
	 (zoom 1d0)
	 (coor-x (+ half-w (* (- x 0.31006d0) w zoom)))
	 (coor-y (- half-h (* (- y 0.31616d0) h zoom))))
    (with-pen (make-pen :fill col :stroke col :weight 0)
      (circle coor-x coor-y size))))

(defun dist (x y)
  (sqrt (+ (* (- x 0.31006d0) (- x 0.31006d0)) (* (- y 0.31616d0) (- y 0.31616d0)))))

(defun draw-isovalue-plane (value &optional (chroma-start 0.9d0) (col +red+) (step 0.1d0) (huestart 0d0) (hueend 40d0))
  (do ((hue40 huestart (+ hue40 step)))
      ((>= hue40 hueend))
    (let ((mchroma 10d0))
      (do ((chroma chroma-start (+ chroma 1d0)))
	  ((>= chroma mchroma))
	(unless (nth-value 1 (dufy:mhvc-to-lrgb hue40 value chroma :threshold 0))
	  (destructuring-bind (x y largey) (dufy::mhvc-to-xyy hue40 value chroma)
	    (declare (ignore largey))
	    (dotter x y col))
	)
	))))

(defun hvc-dotter (hue40 value chroma &optional (col +blue+))
  (destructuring-bind (x y largey)
      (dufy::mhvc-to-xyy hue40 value chroma)
    (declare (ignore largey))
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
    (let ((value 2.0280881393322865d00))
      (background +white+)
      (draw-isovalue-plane value 1d0 +red+ 0.5 0 40)
      (dotter 0.31006 0.31616 +black+ 2) ;center
      (dotter 0.34104130443550906d0 0.4079645609202538d0 +blue+ 2)
      (dotter 0.1032404719542902d0 -0.05881050558066112d0 +blue+ 3)
      ;;(dotter 0.31005999999999995d0 0.31616d0 +blue+ 4)
      ;;(dotter 0.28615785225704454d0 0.3284929782525198d0 +blue+ 5)
      ;;(dotter 0.29363774584769625d0 0.3623408980758624d0 +blue+ 6)
      ;;(dotter 0.3258909754346112d0 0.4080632680942279d0 +blue+ 7)
      ;;(dotter 0.35331185453384734d0 0.43508203038444443d0 +blue+ 8)
      ;;(dotter 0.10141662554370862d0 -0.0421097676325004d0 +blue+ 12)
      (dotter 0.39325845934838966d0 0.48468540770423074d0 +black+ 5) ;;true point
)))


;;xyY =  (0.3932584163643249d0 0.48468527100036607d0 0.0022344506660951953d0)
