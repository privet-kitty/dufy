(in-package :dufy/core)

;;;
;;; L*a*b*
;;;

(define-colorspace lab (lstar astar bstar)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "L*a*b* space. The nominal range of L* is [0, 100]")

(define-colorspace lchab (lstar cstarab hab)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "LCh(ab) space. h is in the circle group R/360Z. The nominal range of L* is [0, 100]")

(declaim (inline function-f)
         (ftype (function * (values double-float &optional)) function-f))
(defun function-f (x)
  (declare (optimize (speed 3) (safety 0))
           (double-float x))
  (if (> x #.(float 216/24389 1d0))
      (expt x #.(float 1/3 1d0))
      (+ (* #.(/ 24389/27 116d0) x) #.(float 16/116 1d0))))

(define-primary-converter (xyz lab) (x y z &key (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (x y z)
    (let ((fx (function-f (/ x (illuminant-x illuminant))))
          (fy (function-f y))
          (fz (function-f (/ z (illuminant-z illuminant)))))
      (values (- (* 116d0 fy) 16d0)
              (* 500d0 (- fx fy))
              (* 200d0 (- fy fz))))))

(define-primary-converter (lab xyz) (lstar astar bstar &key (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((fy (* (+ (float lstar 1d0) 16d0) 1/116))
         (fx (+ fy (* (float astar 1d0) 0.002d0)))
         (fz (- fy (* (float bstar 1d0) 0.005d0))))
    (values (if (> fx #.(float 6/29 1d0))
                (* (illuminant-x illuminant) fx fx fx)
                (* (- fx #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29) (illuminant-x illuminant)))
            (if (> fy #.(float 6/29 1d0))
                (* fy fy fy)
                (* (- fy #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29)))
            (if (> fz #.(float 6/29 1d0))
                (* (illuminant-z illuminant) fz fz fz)
                (* (- fz #.(float 16/116 1d0)) #.(* 3d0 6/29 6/29) (illuminant-z illuminant))))))

(declaim (inline lstar-to-y))
(defun lstar-to-y (lstar)
  "L* (of L*a*b*) to Y (of XYZ)"
  (declare (optimize (speed 3) (safety 1)))
  (let* ((fy (* (+ (float lstar 1d0) 16d0) 1/116)))
    (if (> fy #.(float 6/29 1d0))
        (* fy fy fy)
        (* (- fy 16/116) #.(* 3d0 6/29 6/29)))))

(declaim (inline y-to-lstar))
(defun y-to-lstar (y)
  "Y (of XYZ) to L* (of L*a*b*)"
  (declare (optimize (speed 3) (safety 1)))
  (- (* 116d0 (function-f (float y 1d0))) 16d0))

(define-primary-converter (lab lchab) (lstar astar bstar)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar astar bstar)
    (values lstar
            (sqrt (+ (* astar astar) (* bstar bstar)))
            (mod (* (atan bstar astar) +360/TWO-PI+) 360d0))))

(define-primary-converter (lchab lab) (lstar cstarab hab)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar cstarab hab)
    (let ((hue-two-pi (* hab +TWO-PI/360+)))
      (values lstar
              (* cstarab (cos hue-two-pi))
              (* cstarab (sin hue-two-pi))))))

(defconverter xyz lchab)
(defconverter lchab xyz)

;; for internal use
(defconverter xyy lab)
(defconverter lab xyy)
(defconverter xyy lchab)
(defconverter lchab xyy)

;;;
;;; L*u*v*
;;;

(define-colorspace luv (lstar ustar vstar)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "L*u*v* space. The nominal range of L* is [0, 100]")

(define-colorspace lchuv (lstar cstaruv huv)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "LCh(uv) space. h is in the circle group R/360Z. The nominal range of L* is [0, 100]")

(declaim (inline calc-uvprime))
(defun calc-uvprime (x y)
  (declare (optimize (speed 3) (safety 0))
           (double-float x y))
  "Calculates u' and v' from xy."
  (let ((denom (+ (* -2d0 x) (* 12d0 y) 3d0)))
    (values (/ (* 4d0 x) denom)
            (/ (* 9d0 y) denom))))

(declaim (inline calc-uvprime-from-xyz))
(defun calc-uvprime-from-xyz (x y z)
  (declare (optimize (speed 3) (safety 0))
           (double-float x y z))
  "Calculates u' and v' from XYZ."
  (let ((denom (+ x (* 15d0 y) (* 3d0 z))))
    (values (/ (* 4d0 x) denom)
            (/ (* 9d0 y) denom))))

(define-primary-converter (xyz luv) (x y z &key (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (x y z)
    (multiple-value-bind (uprime vprime)
        (calc-uvprime-from-xyz x y z)
      (multiple-value-bind (urprime vrprime)
          (calc-uvprime-from-xyz (illuminant-x illuminant)
                                 1d0
                                 (illuminant-z illuminant))
        (let ((lstar (if (> y #.(expt 6/29 3d0))
                         (- (* 116d0 (expt y #.(float 1/3 1d0))) 16d0)
                         (* #.(expt 29/3 3d0) y))))
          (values lstar
                  (* 13d0 lstar (- uprime urprime))
                  (* 13d0 lstar (- vprime vrprime))))))))

(define-primary-converter (luv xyz) (lstar ustar vstar &key (illuminant +illum-d65+))
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar ustar vstar)
    (multiple-value-bind (urprime vrprime)
        (calc-uvprime-from-xyz (illuminant-x illuminant)
                               1d0
                               (illuminant-z illuminant))
      (let* ((uprime (+ (/ ustar (* 13d0 lstar)) urprime))
             (vprime (+ (/ vstar (* 13d0 lstar)) vrprime))
             (l (/ (+ lstar 16d0) 116d0))
             (y (if (<= lstar 8d0)
                    (* lstar #.(expt 3/29 3d0))
                    (* (* l l l)))))
        (values (* y (/ (* 9d0 uprime) (* 4d0 vprime)))
                y
                (* y (/ (- 12d0 (* 3d0 uprime) (* 20d0 vprime)) (* 4d0 vprime))))))))

(define-primary-converter (luv lchuv) (lstar ustar vstar)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar ustar vstar)
    (values lstar
            (sqrt (+ (* ustar ustar) (* vstar vstar)))
            (mod (* (atan vstar ustar) +360/TWO-PI+) 360d0))))

(define-primary-converter (lchuv luv) (lstar cstaruv huv)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar cstaruv huv)
    (let ((hue-two-pi (* huv +TWO-PI/360+)))
      (values lstar
              (* cstaruv (cos hue-two-pi))
              (* cstaruv (sin hue-two-pi))))))

(defconverter xyz lchuv)
(defconverter lchuv xyz)
