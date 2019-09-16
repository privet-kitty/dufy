;; -*- coding: utf-8 -*-
;;;
;;; -to-Munsell converters
;;;
;;; The primary converter is LCHAB-TO-MHVC-ILLUM-C.
;;;
;;; TODO: LCHAB-TO-MHVC-ILLUM-C should call PREDICT-LCHAB-TO-MHVC
;;; (that calls LCHAB-TO-MHVC-GENERAL-CASE that calls
;;; LCHAB-TO-MHVC-L-INTEGER-CASE that calls
;;; LCHAB-TO-MHVC-L-C-INTEGER-CASE that calls
;;; LCHAB-TO-MHVC-ALL-INTEGER-CASE) for the first approximation and
;;; passes the returned HVC to INVERT-MHVC-TO-LCHAB to compute a more
;;; accurate HVC.
;;;

(in-package :dufy/munsell)

;; Below are converters between Munsell HVC and corresponding
;; cartesian coordinates. (Used only internally.)
(declaim (inline cartesian-to-mhvc))
(defun cartesian-to-mhvc (x y value)
  (values (mod (* (atan y x) #.(/ 40 TWO-PI)) 40d0)
          value
          ;; A unit value is equivalent to two units chroma.
          (* 2 (sqrt (+ (* x x) (* y y))))))

(declaim (inline mhvc-to-cartesian))
(defun mhvc-to-cartesian (hue40 value chroma)
  (let ((rad (* hue40 #.(/ TWO-PI 40)))
        (chroma/2 (/ chroma 2)))
    (values (* chroma/2 (cos rad))
            (* chroma/2 (sin rad))
            value)))

(declaim (ftype (function * (values (double-float 0d0 40d0) double-float double-float &optional))
                lchab-to-mhvc-all-integer-case
                lchab-to-mhvc-l-c-integer-case
                lchab-to-mhvc-l-integer-case
                lchab-to-mhvc-general-case))

;; In the following functions, the actual L* equals 10*LSTAR/10; the
;; actual C*ab equals 20*CSTARAB/20; the actual hab equals 9*HAB/9.
(declaim (inline lchab-to-mhvc-all-integer-case))
(defun lchab-to-mhvc-all-integer-case (lstar/10 cstarab/20 hab/9)
  (declare (optimize (speed 3) (safety 1))
           (fixnum lstar/10 cstarab/20 hab/9))
  "All integer case. Does no type checks: e.g. HAB/9 must be in {0, 1,
...., 39}."
  (declare (fixnum lstar/10 cstarab/20 hab/9))
  (if (<= cstarab/20 25)
      (values (aref +inversed-mrd-table-hc+ lstar/10 cstarab/20 hab/9 0)
              (aref +inversed-mrd-table-v+ lstar/10)
              (aref +inversed-mrd-table-hc+ lstar/10 cstarab/20 hab/9 1))
      ;; If C*ab > 500, the chroma is linearly extrapolated.
      (let ((chroma-at-boundary (aref +inversed-mrd-table-hc+ lstar/10 25 hab/9 1))
            (factor (* cstarab/20 #.(float 1/25 1d0))))
        (values (aref +inversed-mrd-table-hc+ lstar/10 25 hab/9 0)
                (aref +inversed-mrd-table-v+ lstar/10)
                (* chroma-at-boundary factor)))))

(declaim (inline lchab-to-mhvc-l-c-integer-case))
(defun lchab-to-mhvc-l-c-integer-case (lstar/10 cstarab/20 hab/9)
  (declare (optimize (speed 3) (safety 1))
           (fixnum lstar/10 cstarab/20)
           ((double-float 0d0 40d0) hab/9))
  (let ((hab1 (floor hab/9))
        (hab2 (mod (ceiling hab/9) 40)))
    (multiple-value-bind (hue1 value chroma1)
        (lchab-to-mhvc-all-integer-case lstar/10 cstarab/20 hab1)
      (if (= hab1 hab2)
          (values hue1 value chroma1)
          (multiple-value-bind (hue2 _ chroma2)
              (lchab-to-mhvc-all-integer-case lstar/10 cstarab/20 hab2)
            (declare (ignore _)
                     ((double-float 0d0 40d0) hue1 hue2))
            (if (= hue1 hue2)
                (values hue1 value chroma1)
                (let* ((hue40 (circular-lerp (- hab/9 hab1) hue1 hue2 40d0))
                       (chroma (+ (* chroma1 (/ (mod (- hue2 hue40) 40d0)
                                                (mod (- hue2 hue1) 40d0)))
                                  (* chroma2 (/ (mod (- hue40 hue1) 40d0)
                                                (mod (- hue2 hue1) 40d0))))))
                  (declare ((double-float 0d0 40d0) hue40))
                  (values hue40 value chroma))))))))

(defun lchab-to-mhvc-l-integer-case (lstar/10 cstarab/20 hab/9)
  (declare (optimize (speed 3) (safety 1))
           (fixnum lstar/10)
           (non-negative-non-large-double-float cstarab/20)
           ((double-float 0d0 40d0) hab/9))
  (let ((cstarab1 (floor cstarab/20))
        (cstarab2 (ceiling cstarab/20)))
    (if (= cstarab1 cstarab2)
        (lchab-to-mhvc-l-c-integer-case lstar/10 cstarab1 hab/9)
        (multiple-value-bind (x1 y1 value)
            (multiple-value-call #'mhvc-to-cartesian
              (lchab-to-mhvc-l-c-integer-case lstar/10 cstarab1 hab/9))
          (multiple-value-bind (x2 y2 _)
              (multiple-value-call #'mhvc-to-cartesian
                (lchab-to-mhvc-l-c-integer-case lstar/10 cstarab2 hab/9))
            (declare (ignore _)
                     (double-float value x1 y1 x2 y2))
            (let ((x (+ (* x1 (- cstarab2 cstarab/20))
                        (* x2 (- cstarab/20 cstarab1))))
                  (y (+ (* y1 (- cstarab2 cstarab/20))
                        (* y2 (- cstarab/20 cstarab1)))))
              (cartesian-to-mhvc x y value)))))))

(defun lchab-to-mhvc-general-case (lstar/10 cstarab/20 hab/9)
  (declare (optimize (speed 3) (safety 1))
           ((double-float 0d0 10d0) lstar/10)
           (non-negative-non-large-double-float cstarab/20)
           ((double-float 0d0 40d0) hab/9))
  (let  ((lstar1 (floor lstar/10))
         (lstar2 (ceiling lstar/10))
         (value (lstar-to-munsell-value (* lstar/10 10))))
    (if (= lstar1 lstar2)
        (lchab-to-mhvc-l-integer-case lstar1 cstarab/20 hab/9)
        ;; If the given color is so dark that it is out of the table,
        ;; we use the fact that the chroma and hue of LCh(ab)
        ;; corresponds roughly to that of Munsell.
        (if (zerop lstar1)
            (multiple-value-bind (hue40 _ chroma)
                (lchab-to-mhvc-l-integer-case 1 cstarab/20 hab/9)
              (declare (ignore _))
              (values hue40 value chroma))
            (multiple-value-bind (x1 y1 value1)
                (multiple-value-call #'mhvc-to-cartesian
                  (lchab-to-mhvc-l-integer-case lstar1 cstarab/20 hab/9))
              (multiple-value-bind (x2 y2 value2)
                  (multiple-value-call #'mhvc-to-cartesian
                    (lchab-to-mhvc-l-integer-case lstar2 cstarab/20 hab/9))
                (declare (double-float x1 y1 value1 x2 y2 value2))
                (let ((x (+ (* x1 (/ (- value2 value) (- value2 value1)))
                            (* x2 (/ (- value value1) (- value2 value1)))))
                      (y (+ (* y1 (/ (- value2 value) (- value2 value1)))
                            (* y2 (/ (- value value1) (- value2 value1))))))
                  (cartesian-to-mhvc x y value))))))))

(defun predict-lchab-to-mhvc (lstar cstarab hab)
  (declare (optimize (speed 3) (safety 1)))
  "Illuminant C."
  (with-ensuring-type double-float (lstar cstarab hab)
    (let ((lstar/10 (clamp (* lstar 0.1d0) 0d0 10d0))
          (cstarab/20 (clamp (* cstarab #.(float 1/20 1d0))
                             0d0
                             *most-positive-non-large-double-float*))
          (hab/9 (mod (* hab #.(float 40/360 1d0)) 40d0)))
      (lchab-to-mhvc-general-case lstar/10 cstarab/20 hab/9))))

(declaim (inline circular-delta))
(defun circular-delta (theta1 theta2)
  "Is called in INVERT-LCHAB-TO-MHVC."
  (let ((z (mod (- theta1 theta2) 360d0)))
    (if (<= z 180d0)
        z
        (- z 360d0))))

(define-condition large-approximation-error (arithmetic-error)
  ((message :initarg :message
            :initform "Couldn't achieve the required accuracy."
            :accessor cond-message))
  (:report (lambda (condition stream)
             (format stream "~A" (cond-message condition)))))

;; called by LCHAB-TO-MHVC-ILLUM-C
(declaim (inline invert-mhvc-to-lchab))
(defun invert-mhvc-to-lchab (lstar cstarab hab init-hue40 init-chroma &key (max-iteration 200) (if-reach-max :error) (factor 0.5d0) (threshold 1d-6))
  (declare (double-float lstar cstarab hab factor threshold)
           (fixnum max-iteration))
  "Illuminant C."
  (let ((hue40 init-hue40)
        (value (lstar-to-munsell-value lstar))
        (chroma init-chroma))
    (declare (double-float hue40 chroma value))
    (if (or (<= value threshold) (<= init-chroma threshold))
        (values init-hue40 value init-chroma)
        (dotimes (i max-iteration
                    (ecase if-reach-max
                      (:error
                       (error 'large-approximation-error
                              :message "INVERT-MHVC-TO-LCHAB reached MAX-ITERATION without achieving sufficient accuracy."))
                      (:return40 (values 40d0 40d0 40d0))
                      (:raw (values (mod hue40 40d0) value chroma))))
          (multiple-value-bind (_ tmp-cstarab tmp-hab)
              (mhvc-to-lchab-illum-c hue40 value chroma)
            (declare (ignore _))
            (let* ((delta-cstarab (- cstarab tmp-cstarab))
                   (delta-hab (circular-delta hab tmp-hab))
                   (delta-hue40 (* delta-hab #.(float 40/360 1d0)))
                   (delta-chroma (* delta-cstarab #.(/ 5.5d0))))
              (if (and (<= (abs delta-hue40) threshold)
                       (<= (abs delta-chroma) threshold))
                  (return-from invert-mhvc-to-lchab
                    (values (mod hue40 40d0) value chroma))
                  (setf hue40 (+ hue40 (* factor delta-hue40))
                        chroma (max 0d0 (+ chroma (* factor delta-chroma)))))))))))


(define-primary-converter (lchab mhvc :name lchab-to-mhvc-illum-c)
    (lstar cstarab hab &key (max-iteration 200) (if-reach-max :error) (factor 0.5d0) (threshold 1d-6) &aux (illuminant +illum-c+))
  "Is an inverter of MHVC-TO-LCHAB-ILLUM-C with a simple iteration algorithm,
which is almost same as the one in \"An Open-Source Inversion Algorithm for the
Munsell Renotation\" by Paul Centore, 2011:

V := LSTAR-TO-MUNSELL-VALUE(L*);
C_0 := C*_ab / 5.5;
H_0 := h_ab / 9;
C_(n+1) :=  C_n + FACTOR * ΔC_n;
H_(n+1) := H_n + FACTOR * ΔH_n;

Δ(H_n) and Δ(C_n) are internally calculated at every step. This function
returns Munsell HVC values if C_0 <= THRESHOLD or if V <= THRESHOLD or when
max(Δ(H_n), Δ(C_n)) falls below THRESHOLD.

IF-REACH-MAX specifies the action to be taken if the loop reaches the
MAX-ITERATION as follows:

:error: Error of type DUFY:LARGE-APPROXIMATION-ERROR is signaled.
:return40: Three 40d0s are returned.
:raw: Just returns HVC as it is.
"
  (declare (optimize (speed 3) (safety 1))
           (ignorable illuminant)
           (fixnum max-iteration))
  (with-ensuring-type double-float (lstar cstarab hab factor threshold)
    (let ((init-hue40 (* hab #.(float 40/360 1d0)))
          (init-chroma (* cstarab #.(/ 5.5d0))))
      (invert-mhvc-to-lchab lstar cstarab hab
                            init-hue40 init-chroma
                            :max-iteration max-iteration
                            :if-reach-max if-reach-max
                            :factor factor
                            :threshold threshold))))

(defconverter lchab munsell
  :name lchab-to-munsell-illum-c
  :documentation "Illuminant C")

(defconverter xyz mhvc
  :name xyz-to-mhvc-illum-c
  :documentation "Illuminant C.")

;; The Bradford transformation from D65 to C
(define-cat-function d65-to-c
  +illum-d65+ +illum-c+ :cat +bradford+)

(declaim (inline xyz-to-mhvc))
(defun xyz-to-mhvc (x y z &key (max-iteration 200) (if-reach-max :error) (factor 0.5d0) (threshold 1d-6))
  "Illuminant D65.
This converter involves the Bradford transformation from illuminant
D65 to illuminant C."
  (multiple-value-call #'lchab-to-mhvc-illum-c
    (multiple-value-call #'xyz-to-lchab
      (d65-to-c x y z)
      :illuminant +illum-c+)
    :max-iteration max-iteration
    :if-reach-max if-reach-max
    :factor factor
    :threshold threshold))

(defconverter xyz munsell
  :name xyz-to-munsell-illum-c
  :documentation "Illuminant C.")

(declaim (inline xyz-to-munsell))
(defun xyz-to-munsell (x y z &key (max-iteration 200) (if-reach-max :error) (factor 0.5d0) (threshold 1d-6) (digits 2))
  "Illuminant D65.
This converter involves the Bradford transformation from illuminant
D65 to illuminant C."
  (multiple-value-call #'mhvc-to-munsell
    (xyz-to-mhvc x y z
                 :max-iteration max-iteration
                 :if-reach-max if-reach-max
                 :factor factor
                 :threshold threshold)
    :digits digits))
