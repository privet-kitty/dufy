;;;
;;; Built-in Standard Illuminants
;;;

(in-package :dufy/core)

;;
;; Note: The white points of the (series of) standard illuminants A, B, C, and D
;; are here calculated with 5 nm intervals over the range 380 nm to 780 nm as
;; they are in the CIE 15.3:2004. That's because such calculated white points
;; are (close to) already frequently used ones for these illuminants.
;;

(define-constant +illum-a+
  (make-illuminant :spectrum
                   #'(lambda (wl)
                       (declare (optimize (speed 3) (safety 1)))
                       (let ((wl (float wl 1d0)))
                         (assert (>= wl 0d0))
                         (* 100d0
                            (expt (/ 560d0 (the (double-float 0d0) wl)) 5)
                            (/ #.(- (exp (/ 1.435d7 (* 2848 560))) 1d0)
                               (- (exp (/ 1.435d7 (* 2848d0 wl))) 1d0)))))
                   :begin-wl 380 :end-wl 780 :band 5
                   :compile-time t)
  :test (constantly t))

;; +ILLUM-B+ is defined in an extra module.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +illum-c-table+
    (make-array 107
                :element-type 'double-float
                :initial-contents '(0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.20d0 0.40d0 1.55d0 2.70d0 4.85d0 7.00d0 9.95d0 12.90d0 17.20d0 21.40d0 27.5d0 33.00d0 39.92d0 47.40d0 55.17d0 63.30d0 71.81d0 80.60d0 89.53d0 98.10d0 105.80d0 112.40d0 117.75d0 121.50d0 123.45d0 124.00d0 123.60d0 123.10d0 123.30d0 123.80d0 124.09d0 123.90d0 122.92d0 120.70d0 116.90d0 112.10d0 106.98d0 102.30d0 98.81d0 96.90d0 96.78d0 98.00d0 99.94d0 102.10d0 103.95d0 105.20d0 105.67d0 105.30d0 104.11d0 102.30d0 100.15d0 97.80d0 95.43d0 93.20d0 91.22d0 89.70d0 88.83d0 88.40d0 88.19d0 88.10d0 88.06d0 88.00d0 87.86d0 87.80d0 87.99d0 88.20d0 88.20d0 87.90d0 87.22d0 86.30d0 85.30d0 84.00d0 82.21d0 80.20d0 78.24d0 76.30d0 74.36d0 72.40d0 70.40d0 68.30d0 66.30d0 64.40d0 62.80d0 61.50d0 60.20d0 59.20d0 58.50d0 58.10d0 58.00d0 58.20d0 58.50d0 59.10d0 78.91d0 79.55d0 76.48d0 73.40d0 68.66d0 63.92d0 67.35d0 70.78d0 72.61d0 74.44d0))))

(define-constant +illum-c+
  (make-illuminant :spectrum (gen-spectrum +illum-c-table+ 300 830)
                   :begin-wl 380 :end-wl 780 :band 5
                   :compile-time t)
  :test (constantly t))

(define-constant +illum-d50+
  (make-illuminant :spectrum (gen-illum-d-spectrum 5000 :rectify t)
                   :begin-wl 380 :end-wl 780 :band 5
                   :compile-time t)
  :test (constantly t))

(define-constant +illum-d65+
  (make-illuminant :spectrum (gen-illum-d-spectrum 6500 :rectify t)
                   :begin-wl 380 :end-wl 780 :band 5
                   :compile-time t)
  :test (constantly t))

(define-constant +illum-e+
  (make-illuminant :x 1d0 :z 1d0 :spectrum #'flat-spectrum)
  :test (constantly t))

