;;;
;;; This file contains several definitions for MHVC (3-number
;;; specification of Munsell Color) and MUNSELL (Standard string
;;; specification of Munsell Color), utilities for users, tools for
;;; development etc.
;;; 

(uiop:define-package :dufy/munsell/fundamental
  (:use :cl :alexandria :cl-ppcre :dufy/internal/* :dufy/core/* :dufy/munsell/y-to-value-data :dufy/munsell/renotation-data)
  (:export #:mhvc #:munsell
           #:invalid-mhvc-error
           #:munsellspec-parse-error
           
           #:*most-positive-non-large-double-float*
           #:non-negative-non-large-double-float
           #:max-chroma-in-mrd
           #:mhvc-out-of-mrd-p
           #:munsell-out-of-mrd-p
           
           #:munsell-value-to-y
           #:y-to-munsell-value
           #:munsell-value-to-lstar
           #:lstar-to-munsell-value

           #:munsell-to-mhvc
           #:mhvc-to-munsell))

(in-package :dufy/munsell/fundamental)

(define-colorspace mhvc (hue40 value chroma)
  :arg-types (real real real)
  :return-types ((double-float 0d0 40d0) double-float double-float)
  :documentation "Is a three-number specification of Munsell Color. HUE40 is in the circle group R/40Z. The nominal range of VALUE is [0, 10].")

(define-colorspace munsell (munsellspec)
  :arg-types (string)
  :return-types (string)
  :documentation "Is the standard string specification of Munsell color.")

;; The bradford transformations between D65 and C are frequently used here.
(define-cat-function c-to-d65
  +illum-c+ +illum-d65+ :cat +bradford+)
(define-cat-function d65-to-c
  +illum-d65+ +illum-c+ :cat +bradford+)

;; FIXME: Below are (not so good) workaround for optimization.
(declaim (double-float *most-positive-non-large-double-float*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bit-length-of-most-positive-fixnum*
    (floor (log most-positive-fixnum 2)))
  (defparameter *most-positive-non-large-double-float*
    #+(and sbcl 64-bit) (float (expt 2 (- *bit-length-of-most-positive-fixnum* 1)) 1d0)
    #-(and sbcl 64-bit) most-positive-double-float))

(deftype non-negative-non-large-double-float ()
  "The double-float that the Munsell converters accepts. It is in some
cases less than MOST-POSITIVE-DOUBLE-FLOAT because of efficiency:
e.g. on SBCL (64-bit) it is desirable that a float F
fulfills (TYPEP (ROUND F) '(SIGNED-BYTE 64))"
  `(double-float 0d0 ,*most-positive-non-large-double-float*))

(declaim (ftype (function * (integer 0 50)) max-chroma-in-mrd))
(defun max-chroma-in-mrd (hue40 value &key (use-dark t))
  (declare (optimize (speed 3) (safety 1)))
  "Returns the largest chroma in the Munsell Renotation Data (all.dat)
for a given hue and value. If you want to ignore the data for value =
0.2, 0.4, 0.6, or 0.8, give NIL to USE-DARK(, though it is maybe only
for development). "
  (with-ensuring-type double-float (hue40 value)
    (let* ((hue (mod hue40 40d0))
           (hue1 (floor hue))
           (hue2 (mod (ceiling hue) 40)))
      (if (or (>= value 1)
              (not use-dark))
          (let ((val1 (floor value))
                (val2 (ceiling value)))
            (min (aref +max-chroma-table+ hue1 val1)
                 (aref +max-chroma-table+ hue1 val2)
                 (aref +max-chroma-table+ hue2 val1)
                 (aref +max-chroma-table+ hue2 val2)))
          (let* ((dark-value (* value 5d0))
                 (dark-val1 (floor dark-value))
                 (dark-val2 (ceiling dark-value)))
            (min (aref +max-chroma-table-dark+ hue1 dark-val1)
                 (aref +max-chroma-table-dark+ hue1 dark-val2)
                 (aref +max-chroma-table-dark+ hue2 dark-val1)
                 (aref +max-chroma-table-dark+ hue2 dark-val2)))))))

(declaim (inline munsell-value-to-y)
         (ftype (function * (values double-float &optional)) munsell-value-to-y))
(defun munsell-value-to-y (v)
  (declare (optimize (speed 3) (safety 1)))
  "Converts Munsell value to Y, whose nominal range is [0, 1]. The
formula is based on ASTM D1535-08e1. Note that this function does no
clamping even if V is outside the interval [0, 10]."
  (with-ensuring-type double-float (v)
    (* v (+ 1.1914d0 (* v (+ -0.22533d0 (* v (+ 0.23352d0 (* v (+ -0.020484d0 (* v 0.00081939d0)))))))) 0.01d0)))

(declaim (inline munsell-value-to-lstar)
         (ftype (function * (values double-float &optional)) munsell-value-to-lstar))
(defun munsell-value-to-lstar (v)
  "Converts Munsell value to L*, whose nominal range is [0, 100]."
  (- (* 116d0 (dufy/core/lab-and-luv::function-f (munsell-value-to-y v))) 16d0))

(declaim (inline y-to-munsell-value))
(defun y-to-munsell-value (y)
  "Interpolates the inversion table of MUNSELL-VALUE-TO-Y linearly,
whose band width is 1e-3. It is guaranteed that the round-trip
error, (abs (- (y (munsell-value-to-y (y-to-munsell-value y))))), is
smaller than 1e-5."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((y1000 (* (clamp (float y 1d0) 0d0 1d0) 1000))
         (y1 (floor y1000))
         (y2 (ceiling y1000)))
    (if (= y1 y2)
        (aref +y-to-munsell-value-table+ y1)
        (let ((r (- y1000 y1)))
          (+ (* (- 1 r) (aref +y-to-munsell-value-table+ y1))
             (* r (aref +y-to-munsell-value-table+ y2)))))))

(defun evaluate-error-of-y-to-munsell-value (&optional (num 100000000))
  "For devel. Returns the maximal error and the corresponding Y."
  (declare (optimize (speed 3) (safety 1))
           (fixnum num))
  (let ((max-error 0d0)
        (worst-y 0d0))
    (dotimes (n num)
      (let* ((y (random 1d0))
             (delta (abs (- (munsell-value-to-y (y-to-munsell-value y)) y))))
        (when (>= delta max-error)
          (setf worst-y y
                max-error delta))))
    (values max-error worst-y)))

(declaim (inline lstar-to-munsell-value))
(defun lstar-to-munsell-value (lstar)
  (declare (optimize (speed 3) (safety 1)))
  (y-to-munsell-value (lstar-to-y (float lstar 1d0))))

(defun qrgb-to-munsell-value (r g b &optional (rgbspace +srgb+))
  "For devel."
  (y-to-munsell-value (nth-value 1 (qrgb-to-xyz r g b :rgbspace rgbspace))))

(define-condition invalid-mhvc-error (simple-error)
  ((value :initarg :value
          :initform 0d0
          :accessor cond-value)
   (chroma :initarg :chroma
           :initform 0d0
           :accessor cond-chroma))
  (:report (lambda (condition stream)
             (format stream "Value and chroma must be within [0, 10] and [0, ~A), respectively: (V C) = (~A ~A)"
                     *most-positive-non-large-double-float*
                     (cond-value condition)
                     (cond-chroma condition)))))

(defun mhvc-out-of-mrd-p (hue40 value chroma)
  "Checks if Munsell HVC is out of the Munsell Renotation data."
  (or (< value 0) (> value 10)
      (< chroma 0)
      (> chroma (max-chroma-in-mrd hue40 value))))

(defun mhvc-invalid-p (hue40 value chroma)
  "Checks if Munsell HVC values are out of the valid range."
  (declare (ignore hue40))
  (or (< value 0) (> value 10)
      (< chroma 0) (> chroma *most-positive-non-large-double-float*)))

(define-condition munsellspec-parse-error (parse-error)
  ((spec :initarg :spec
         :initform nil
         :accessor cond-spec))
  (:report (lambda (condition stream)
             (format stream "Invalid Munsell specification: ~A"
                     (cond-spec condition)))))

(define-primary-converter (munsell mhvc) (munsellspec)
  (declare (optimize (speed 3) (safety 1)))
  "Usage Example:
 (dufy:munsell-to-mhvc \"0.02RP 0.9/3.5\")
;; => 36.008d0
;; 0.9d0
;; 3.5d0

Many other notations of numbers are acceptable; an ugly specification
as follows are also available:

 (dufy:munsell-to-mhvc \"2d-2RP .9/ #x0ffffff\")
;; => 36.008d0
;; 0.9d0
;; 1.6777215d7

However, the capital letters and  '/' are reserved:

 (dufy:munsell-to-mhvc \"2D-2RP 9/10 / #X0FFFFFF\")
;; => ERROR,
"
  (let* ((lst (let ((*read-default-float-format* 'double-float))
                (mapcar (compose (rcurry #'coerce 'double-float)
                                 #'read-from-string)
                        (remove "" (ppcre:split "[^0-9.a-z\-#]+" munsellspec)
                                :test #'string=))))
         (hue-name (the string (ppcre:scan-to-strings "[A-Z]+" munsellspec)))
         (hue-number
           (switch (hue-name :test #'string=)
             ("R" 0) ("YR" 1) ("Y" 2) ("GY" 3) ("G" 4)
             ("BG" 5) ("B" 6) ("PB" 7) ("P" 8) ("RP" 9) ("N" -1)
             (t (error (make-condition 'munsellspec-parse-error
                                       :spec (format nil "Invalid hue designator: ~A" hue-name)))))))
    (cond ((= hue-number -1) ; achromatic
           (values 0d0 (car lst) 0d0))
          ((/= (length lst) 3)
           (error (make-condition 'munsellspec-parse-error
                                  :spec (format nil "Contains more than 3 numbers: ~A" lst))))
          (t (setf (car lst)
                   (mod (+ (* hue-number 4)
                           (* (the double-float (car lst)) 0.4d0))
                        40d0))
             (values-list lst)))))

(define-primary-converter (mhvc munsell) (hue40 value chroma &key (digits 2))
  (let ((directive (concatenate 'string "~," (write-to-string digits) "F")))
    (if (< chroma (* 0.5d0 (expt 0.1d0 digits))) ; if achromatic
        (format nil (format nil "N ~A" directive) value)
        (let* ((hue40$ (mod hue40 40d0))
               (hue-number (floor (/ hue40$ 4)))
               (hue-prefix (* (mod hue40$ 4) 2.5d0))
               (hue-name (aref #("R" "YR" "Y" "GY" "G"
                                 "BG" "B" "PB" "P" "RP")
                               hue-number)))
          (format nil (concatenate 'string directive "~A " directive "/" directive)
                  hue-prefix hue-name value chroma)))))

(defun munsell-out-of-mrd-p (munsellspec)
  (multiple-value-call #'mhvc-out-of-mrd-p (munsell-to-mhvc munsellspec)))
