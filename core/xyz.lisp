;;;
;;; XYZ and xyY
;;;

(uiop:define-package :dufy/core/xyz
  (:use :cl :alexandria :dufy/internal/* :dufy/core/spectrum :dufy/core/illuminants-data)
  (:import-from :dufy/core/spectrum #:%spectrum-to-xyz #:illuminant-to-spectrum-matrix)
  (:export #:xyz #:xyy
           #:xyy-to-xyz
           #:xyz-to-xyy 
           #:spectrum-to-xyz
           #:xyz-to-spectrum))

(in-package :dufy/core/xyz)

(define-colorspace xyz (x y z)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Y is normalized: i.e. the nominal range of Y is [0, 1]")

(define-colorspace xyy (small-x small-y y)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Y is normalized: i.e. the nominal range of Y is [0, 1]")

(define-primary-converter (xyy xyz) (small-x small-y y)
  (declare (optimize (speed 3) (safety 1)))
  "Converts xyY to XYZ. The nominal range of Y is [0, 1], though all
real values are accepted."
  (with-ensuring-type double-float (small-x small-y y)
    (if (zerop small-y)
        (values 0d0 y 0d0)
        (values (/ (* small-x y) small-y) 
                y
                (/ (* (- 1d0 small-x small-y) y) small-y)))))

(define-primary-converter (xyz xyy) (x y z)
  (declare (optimize (speed 3) (safety 1)))
  "Converts XYZ to xyY. The nominal range of Y is [0, 1], though all
real values are accepted."
  (with-ensuring-type double-float (x y z)
    (let ((sum (+ x y z)))
      (if (= sum 0)
          (values 0d0 0d0 y)
          (values (/ x sum) (/ y sum) y)))))

(define-primary-converter (spectrum xyz) (spectrum &key (illuminant +illum-d65+) (begin-wl 360) (end-wl 830) (band 1))
  (declare (optimize (speed 3) (safety 1)))
  "Computes the XYZ values of SPECTRUM in reflective or transmissive
case. The function SPECTRUM, a spectral reflectance, must be defined
at least in [BEGIN-WL, END-WL]; the SPECTRUM is called for BEGIN-WL,
BEGIN-WL + BAND, BEGIN-WL + 2*BAND, ..., BEGIN-WL + n*BAND (<= END-WL)."
  (if (illuminant-no-spd-p illuminant)
      (error (make-condition 'no-spd-error :illuminant illuminant))
      (%spectrum-to-xyz spectrum
                        (illuminant-spectrum illuminant)
                        (illuminant-observer illuminant)
                        begin-wl
                        end-wl
                        band)))

(define-primary-converter (xyz spectrum) (x y z &key (illuminant +illum-d65+))
  "Converts XYZ to spectrum, which is, of course, a spectrum among
many and may contain a negative spectral density."
  (if (illuminant-no-spd-p illuminant)
      (error (make-condition 'no-spd-error :illuminant illuminant))
      (let ((observer (illuminant-observer illuminant)))
        (multiple-value-bind (fac-x fac-y fac-z)
            (multiply-mat-vec (illuminant-to-spectrum-matrix illuminant) x y z)
          #'(lambda (wl)
              (declare (optimize (speed 3) (safety 1)))
              (+ (* fac-x (funcall (observer-cmf-x observer) wl))
                 (* fac-y (funcall (observer-cmf-y observer) wl))
                 (* fac-z (funcall (observer-cmf-z observer) wl))))))))
