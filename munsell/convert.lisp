;;;
;;; Munsell-to- converters
;;; The primary converter is MHVC-TO-LCHAB-ILLUM-C
;;;

(uiop:define-package :dufy/munsell/convert
  (:use :cl :alexandria :dufy/internal/* :dufy/core/* :dufy/munsell/renotation-data :dufy/munsell/fundamental)
  (:export #:mhvc-to-lchab-illum-c
           #:mhvc-to-xyz-illum-c
           #:mhvc-to-xyz
           #:munsell-to-lchab-illum-c
           #:munsell-to-xyz-illum-c
           #:munsell-to-xyz))

(in-package :dufy/munsell/convert)

(declaim (ftype (function * (values (double-float 0d0 360d0) double-float double-float &optional))
                mhvc-to-lchab-all-integer-case
                mhvc-to-lchab-value-chroma-integer-case
                mhvc-to-lchab-value-integer-case
                mhvc-to-lchab-general-case))

;; These converters process a dark color (value < 1) separately
;; because the values of the Munsell Renotation Data (all.dat) are not
;; evenly distributed: [0, 0.2, 0.4, 0.6, 0.8, 1, 2, 3, ..., 10]. In
;; the following functions, the actual value equals SCALED-VALUE/5 if
;; DARK is true.

;; HALF-CHROMA is a half of the actual chroma.

(declaim (inline mhvc-to-lchab-all-integer-case))
(defun mhvc-to-lchab-all-integer-case (hue40 scaled-value half-chroma &optional (dark nil))
  "All integer case. There are no type checks: e.g. HUE40 must be in
{0, 1, ...., 39}."
  (declare (optimize (speed 3) (safety 0))
           (fixnum hue40 scaled-value half-chroma))
  (macrolet ((gen-body (table-l table-c-h)
               `(if (<= half-chroma 25)
                    (values (aref ,table-l scaled-value)
                            (aref ,table-c-h hue40 scaled-value half-chroma 0)
                            (aref ,table-c-h hue40 scaled-value half-chroma 1))
                    ;; If chroma > 50, the C*ab is linearly extrapolated.
                    (let ((cstarab (aref ,table-c-h hue40 scaled-value 25 0))
                          (factor (* half-chroma #.(float 1/25 1d0))))
                      (values (aref ,table-l scaled-value)
                              (* cstarab factor)
                              (aref ,table-c-h hue40 scaled-value 25 1))))))
    (if dark
        (gen-body +mrd-table-l-dark+ +mrd-table-ch-dark+)
        (gen-body +mrd-table-l+ +mrd-table-ch+))))

(declaim (inline mhvc-to-lchab-value-chroma-integer-case))
(defun mhvc-to-lchab-value-chroma-integer-case (hue40 scaled-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
           ((double-float 0d0 40d0) hue40)
           (fixnum scaled-value half-chroma))
  (let ((hue1 (floor hue40))
        (hue2 (mod (ceiling hue40) 40)))
    (multiple-value-bind (lstar cstarab1 hab1)
        (mhvc-to-lchab-all-integer-case hue1 scaled-value half-chroma dark)
      (if (= hue1 hue2)
          (values lstar cstarab1 hab1)
          (multiple-value-bind (_ cstarab2 hab2)
              (mhvc-to-lchab-all-integer-case hue2 scaled-value half-chroma dark)
            (declare (ignore _)
                     ((double-float 0d0 360d0) hab1 hab2))
            (if (= hab1 hab2)
                (values lstar cstarab1 hab1)
                (let* ((hab (circular-lerp (- hue40 hue1) hab1 hab2 360d0))
                       (cstarab (+ (* cstarab1 (/ (mod (- hab2 hab) 360d0)
                                                  (mod (- hab2 hab1) 360d0)))
                                   (* cstarab2 (/ (mod (- hab hab1) 360d0)
                                                  (mod (- hab2 hab1) 360d0))))))
                  (declare ((double-float 0d0 360d0) hab))
                  (values lstar cstarab hab))))))))

(defun mhvc-to-lchab-value-integer-case (hue40 scaled-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
           ((double-float 0d0 40d0) hue40)
           (non-negative-non-large-double-float half-chroma)
           (fixnum scaled-value))
  (let ((hchroma1 (floor half-chroma))
        (hchroma2 (ceiling half-chroma)))
    (if (= hchroma1 hchroma2)
        (mhvc-to-lchab-value-chroma-integer-case hue40 scaled-value hchroma1 dark)
        (multiple-value-bind (lstar astar1 bstar1)
            (multiple-value-call #'lchab-to-lab
              (mhvc-to-lchab-value-chroma-integer-case hue40 scaled-value hchroma1 dark))
          (multiple-value-bind (_ astar2 bstar2)
              (multiple-value-call #'lchab-to-lab
                (mhvc-to-lchab-value-chroma-integer-case hue40 scaled-value hchroma2 dark))
            (declare (ignore _)
                     (double-float lstar astar1 bstar1 astar2 bstar2))
            (let* ((astar (+ (* astar1 (- hchroma2 half-chroma))
                             (* astar2 (- half-chroma hchroma1))))
                   (bstar (+ (* bstar1 (- hchroma2 half-chroma))
                             (* bstar2 (- half-chroma hchroma1)))))
              (lab-to-lchab lstar astar bstar)))))))

(defun mhvc-to-lchab-general-case (hue40 scaled-value half-chroma &optional (dark nil))
  (declare (optimize (speed 3) (safety 0))
           ((double-float 0d0 40d0) hue40 scaled-value)
           (non-negative-non-large-double-float half-chroma))
  (let ((true-value (if dark (* scaled-value 0.2d0) scaled-value)))
    (let  ((scaled-val1 (floor scaled-value))
           (scaled-val2 (ceiling scaled-value))
           (lstar (munsell-value-to-lstar true-value)))
      (if (= scaled-val1 scaled-val2)
          (mhvc-to-lchab-value-integer-case hue40 scaled-val1 half-chroma dark)
          ;; If the given color is so dark that it is out of MRD, we
          ;; use the fact that the chroma and hue of LCh(ab)
          ;; corresponds roughly to that of Munsell.
          (if (zerop scaled-val1)
              (multiple-value-bind (_ cstarab hab)
                  (mhvc-to-lchab-value-integer-case hue40 1 half-chroma dark)
                (declare (ignore _))
                (values lstar cstarab hab))
              (multiple-value-bind (lstar1 astar1 bstar1)
                  (multiple-value-call #'lchab-to-lab
                    (mhvc-to-lchab-value-integer-case hue40 scaled-val1 half-chroma dark))
                (multiple-value-bind (lstar2 astar2 bstar2)
                    (multiple-value-call #'lchab-to-lab
                      (mhvc-to-lchab-value-integer-case hue40 scaled-val2 half-chroma dark))
                  (declare (double-float lstar1 astar1 bstar1
                                         lstar2 astar2 bstar2))
                  (let ((astar (+ (* astar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
                                  (* astar2 (/ (- lstar lstar1) (- lstar2 lstar1)))))
                        (bstar (+ (* bstar1 (/ (- lstar2 lstar) (- lstar2 lstar1)))
                                  (* bstar2 (/ (- lstar lstar1) (- lstar2 lstar1))))))
                    (lab-to-lchab lstar astar bstar)))))))))

(define-primary-converter (mhvc lchab :name mhvc-to-lchab-illum-c)
    (hue40 value chroma &aux (illuminant +illum-c+))
  (declare (optimize (speed 3) (safety 1))
           (ignorable illuminant))
  "Illuminant C."
  (let ((hue40 (mod (float hue40 1d0) 40d0))
        (value (clamp (float value 1d0) 0d0 10d0))
        (chroma (clamp (float chroma 1d0) 0d0 *most-positive-non-large-double-float*)))
    (if (>= value 1d0)
        (mhvc-to-lchab-general-case hue40 value (* chroma 0.5d0) nil)
        (mhvc-to-lchab-general-case hue40 (* value 5d0) (* chroma 0.5d0) t))))

(defconverter mhvc xyz
  :name mhvc-to-xyz-illum-c
  :documentation "Illuminant C.")

;; The bradford transformations from C to D65
(define-cat-function c-to-d65
  +illum-c+ +illum-d65+ :cat +bradford+)

(declaim (inline mhvc-to-xyz)
         (ftype (function * (values double-float double-float double-float &optional)) mhvc-to-xyz))
(defun mhvc-to-xyz (hue40 value chroma)
  "Illuminant D65.
This converter involves the Bradford transformation from illuminant C
to illuminant D65."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'c-to-d65
    (mhvc-to-xyz-illum-c (float hue40 1d0)
                         (float value 1d0)
                         (float chroma 1d0))))

(defconverter munsell lchab
  :name munsell-to-lchab-illum-c
  :documentation "Illuminant C.")

(defconverter munsell xyz
  :name munsell-to-xyz-illum-c
  :documentation "Illuminant C.")

(declaim (inline munsell-to-xyz)
         (ftype (function * (values double-float double-float double-float &optional)) munsell-to-xyz))
(defun munsell-to-xyz (munsellspec)
  "Illuminant D65."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'mhvc-to-xyz (munsell-to-mhvc munsellspec)))

;;;
;;; For development
;;;

;; Not exported.
(defun mhvc-to-xyy (hue40 value chroma)
  "Illuminant D65."
  (multiple-value-call #'xyz-to-xyy (mhvc-to-xyz hue40 value chroma)))
(defun munsell-to-xyy (munsellspec)
  "Illuminant D65."
  (multiple-value-call #'xyz-to-xyy (munsell-to-xyz munsellspec)))

;; (defun munsell-to-qrgb (munsellspec &key (rgbspace +srgb+) (clamp t))
;;   "Illuminant D65.
;; The illuminant of RGBSPACE must also be D65."
;;   (declare (optimize (speed 3) (safety 1)))
;;   (multiple-value-call #'xyz-to-qrgb
;;     (munsell-to-xyz munsellspec)
;;     :rgbspace rgbspace
;;     :clamp clamp))

(defun max-chroma-lchab (hue40 value &key (use-dark t))
  "For devel. Returns the LCh(ab) value of the color on the max-chroma
boundary in the MRD."
  (mhvc-to-lchab-illum-c hue40
                         value
                         (max-chroma-in-mrd hue40 value :use-dark use-dark)))

(defun calc-isochroma-ovoid-integer-case (value chroma/2)
  "Value is integer."
  (let ((ovoid (make-array '(40 2) :element-type 'double-float))) ; (C*ab hab)
    (dotimes (hue40 40 ovoid)
      (setf (aref ovoid hue40 0) (aref +mrd-table-ch+ hue40 value chroma/2 0))
      (setf (aref ovoid hue40 1) (aref +mrd-table-ch+ hue40 value chroma/2 1)))))

(defun calc-isochroma-ovoid (value chroma/2)
  (declare (optimize (speed 3) (safety 1))
           ((double-float 0d0 10d0) value)
           (fixnum chroma/2))
  (let* ((ovoid (make-array '(40 2) :element-type 'double-float))
         (value1 (floor value))
         (value2 (ceiling value))
         (r (- value value1)))
    (declare ((double-float 0d0 1d0) r))
    (if (= value1 value2)
        (calc-isochroma-ovoid-integer-case value1 chroma/2)
        (dotimes (hue40 40 ovoid)
          (setf (aref ovoid hue40 0)
                (lerp r
                      (aref +mrd-table-ch+ hue40 value1 chroma/2 0)
                      (aref +mrd-table-ch+ hue40 value2 chroma/2 0)))
          (setf (aref ovoid hue40 1)
                (circular-lerp r
                               (aref +mrd-table-ch+ hue40 value1 chroma/2 1)
                               (aref +mrd-table-ch+ hue40 value2 chroma/2 1)
                               360d0))))))
