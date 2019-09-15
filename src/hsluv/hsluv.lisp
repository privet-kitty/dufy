;;;
;;; HSLuv and HPLuv
;;;

(in-package #:dufy/hsluv)

(define-colorspace hsluv (huv sat lstar)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Human-friendly HSL for automatic perceptual color generation. HSLuv uses a relative saturation percentage, of the available saturation range for a fixed H and L.")

(define-colorspace hpluv (huv psat lstar)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "Human-friendly HSL for automatic perceptual color generation. HPLuv uses an absolute saturation, constrained by the minimal saturation range across hues for a given L.")

(define-constant +m-bounds+
  (make-array '(3 3) :initial-contents
              '((3.240969941904521d0 -1.537383177570093d0 -0.498610760293d0)
                (-0.96924363628087d0 1.87596750150772d0 0.041555057407175d0)
                (0.055630079696993d0 -0.20397695888897d0 1.056971514242878d0))
                     :element-type 'double-float)
  :test #'equalp)

(defconstant +cieluv-kappa+ 903.2962962d0)
(defconstant +cieluv-epsilon+ 0.0088564516d0)

(defun max-chroma-for-lh (l h)
  "Given L and H values, return the maximum chroma, constrained to
  those values."
  (declare (optimize (speed 3) (safety 1))
           (double-float l h))
  (let ((hrad (degree-to-radian h))
        (bounds (get-cieluv-bounds l)))
    (the double-float
         (reduce #'min
                 (remove-if #'negative-real-p
                            (mapcar #'(lambda (b) (mb-line-ray-intersect-distance hrad b))
                                    bounds))
                 :initial-value most-positive-double-float))))

(defun max-safe-chroma-for-l (l)
  "Given L, return the maximum chroma available over the full range of hues.

For a fixed L, the in-gamut colors are bounded by a convex polygon, whose
boundary lines are given by GET-CIELUV-BOUNDS. The maximum safe chroma is the
maximum chroma that would be valid for any hue."
  (declare (optimize (speed 3) (safety 1))
           (double-float l))
  (let ((bounds (get-cieluv-bounds l)))
    ;; The minimum from the origin to the polygon is the minimum among
    ;; distances to the lines defining the boundary.
    (the double-float (reduce #'min
                              (remove-if #'negative-real-p
                                         (mapcar #'mb-line-distance-from-origin bounds))
                              :initial-value most-positive-double-float))))


(defun get-cieluv-bounds (l)
  "Return a list of lines representing the boundaries of the polygon defining
the in-gamut colors in CIELUV for a fixed L."
  (declare (optimize (speed 3) (safety 1))
           (double-float l))
  (let* ((bounds nil)
         (sub1 (the double-float (/ (expt (+ l 16) 3) 1560896.0d0)))
         (sub2 (the double-float (if (> sub1 +cieluv-epsilon+) sub1 (/ l +cieluv-kappa+)))))
    (dotimes (c 3)
      (let ((m1 (aref +m-bounds+ c 0))
            (m2 (aref +m-bounds+ c 1))
            (m3 (aref +m-bounds+ c 2)))
        (dotimes (ti 2)
          (let ((top1 (the double-float (* sub2 (- (* 284517 m1) (* 94839d0 m3)))))
                (top2 (the double-float (- (* sub2 l (+ (* 838422 m3) (* 769860 m2) (* 731718 m1)))
                                           (* 769860 ti l))))
                (bottom (the double-float (+ (* sub2 (- (* 632260 m3) (* 126452 m2)))
                                             (* ti 126452)))))
            (push (make-mb-line :slope (/ top1 bottom)
                                :intercept (/ top2 bottom))
                  bounds)))))
    bounds))

(define-primary-converter (hsluv lchuv) (huv sat lstar)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (huv sat lstar)
    (cond
      ((> lstar 99.999999d0) (values 100.0d0 0.0d0 huv))
      ((< lstar  0.000001d0) (values 0.0d0 0.0d0 huv))
      (t
       (values lstar (* sat (/ (max-chroma-for-lh lstar huv) 100.0d0)) huv)))))

(define-primary-converter (lchuv hsluv) (lstar cstaruv huv)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar cstaruv huv)
    (cond
      ((> lstar 99.999999d0) (values huv 0.0d0 100.0d0))
      ((< lstar  0.000001d0) (values huv 0.0d0 0.0d0))
      (t
       (let ((c (max-chroma-for-lh lstar huv)))
         (values huv (* (/ cstaruv c) 100.0d0) lstar))))))

(defconverters (xyz lrgb rgb qrgb rgbpack) hsluv)
(defconverters hsluv (xyz lrgb rgb qrgb rgbpack))

(define-primary-converter (hpluv lchuv) (huv psat lstar)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (huv psat lstar)
    (cond
      ((> lstar 99.999999d0) (values 100.0d0 0.0d0 huv))
      ((< lstar  0.000001d0) (values 0.0d0 0.0d0 huv))
      (t
       (values lstar (* psat (/ (max-safe-chroma-for-l lstar) 100.0d0)) huv)))))

(define-primary-converter (lchuv hpluv) (lstar cstaruv huv)
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar cstaruv huv)
    (cond
      ((> lstar 99.999999d0) (values huv 0.0d0 100.0d0))
      ((< lstar  0.000001d0) (values huv 0.0d0 0.0d0))
      (t
       (let ((c (max-safe-chroma-for-l lstar)))
         (values huv (* (/ cstaruv c) 100.0d0) lstar))))))

(defconverters (xyz lrgb rgb qrgb rgbpack) hpluv)
(defconverters hpluv (xyz lrgb rgb qrgb rgbpack))
