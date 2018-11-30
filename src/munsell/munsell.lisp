;; -*- coding: utf-8 -*-

(in-package :dufy/munsell)

;;;
;;; Munsell-to- converters
;;; The primary converter is MHVC-TO-LCHAB-ILLUM-C
;;;

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
  (macrolet ((gen-body (arr-l arr-c-h)
               `(if (<= half-chroma 25)
                    (values (aref ,arr-l scaled-value)
                            (aref ,arr-c-h hue40 scaled-value half-chroma 0)
                            (aref ,arr-c-h hue40 scaled-value half-chroma 1))
                    ;; If chroma > 50, the C*ab is linearly extrapolated.
                    (let ((cstarab (aref ,arr-c-h hue40 scaled-value 25 0))
                          (factor (* half-chroma #.(float 1/25 1d0))))
                      (values (aref ,arr-l scaled-value)
                              (* cstarab factor)
                              (aref ,arr-c-h hue40 scaled-value 25 1))))))
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

(declaim (inline mhvc-to-qrgb)
         (ftype (function * (values fixnum fixnum fixnum &optional)) mhvc-to-qrgb))
(defun mhvc-to-qrgb (hue40 value chroma &key (rgbspace +srgb+) (clamp t))
  "Illuminant D65.
The illuminant of RGBSPACE must also be D65."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'xyz-to-qrgb
    (mhvc-to-xyz hue40 value chroma)
    :rgbspace rgbspace
    :clamp clamp))

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

;; For development. Not exported.
(defun mhvc-to-xyy (hue40 value chroma)
  "Illuminant D65."
  (multiple-value-call #'xyz-to-xyy (mhvc-to-xyz hue40 value chroma)))
(defun munsell-to-xyy (munsellspec)
  "Illuminant D65."
  (multiple-value-call #'xyz-to-xyy (munsell-to-xyz munsellspec)))

(defun munsell-to-qrgb (munsellspec &key (rgbspace +srgb+) (clamp t))
  "Illuminant D65.
The illuminant of RGBSPACE must also be D65."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'xyz-to-qrgb
    (munsell-to-xyz munsellspec)
    :rgbspace rgbspace
    :clamp clamp))

(defun max-chroma-lchab (hue40 value &key (use-dark t))
  "For devel. Returns the LCh(ab) value of the color on the max-chroma
boundary in the MRD."
  (mhvc-to-lchab-illum-c hue40
                         value
                         (max-chroma-in-mrd hue40 value :use-dark use-dark)))


;;;
;;; -to-Munsell converters
;;;
;;; The primary converter is LCHAB-TO-MHVC-ILLUM-C. This function
;;; calls PREDICT-LCHAB-TO-MHVC (that calls LCHAB-TO-MHVC-GENERAL-CASE
;;; that calls LCHAB-TO-MHVC-L-INTEGER-CASE that calls
;;; LCHAB-TO-MHVC-L-C-INTEGER-CASE that calls
;;; LCHAB-TO-MHVC-ALL-INTEGER-CASE) for the first approximation and
;;; passes the returned HVC to INVERT-MHVC-TO-LCHAB to compute a more
;;; accurate HVC.
;;;

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
                       (error (make-condition 'large-approximation-error
                                              :message "INVERT-MHVC-TO-LCHAB reached MAX-ITERATION without achieving sufficient accuracy.")))
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
  (declare (optimize (speed 3) (safety 1))
           (ignorable illuminant)
           (fixnum max-iteration))
  "Is an inverter of MHVC-TO-LCHAB-ILLUM-C with a simple iteration
algorithm, which is almost same as the one in \"An Open-Source
Inversion Algorithm for the Munsell Renotation\" by Paul Centore,
2011:

V := LSTAR-TO-MUNSELL-VALUE(L*);
C_0 := C*ab / 5.5;
H_0 := Hab / 9;
C_(n+1) :=  C_n + factor * delta(C_n);
H_(n+1) := H_n + factor * delta(H_n);

delta(H_n) and delta(C_n) are internally calculated at every
step. This function returns Munsell HVC values if C_0 <= THRESHOLD or
if V <= THRESHOLD or when max(delta(H_n), delta(C_n)) falls below
THRESHOLD.

IF-REACH-MAX specifies the action to be taken if the loop reaches the
MAX-ITERATION:
:error: Error of type DUFY:LARGE-APPROXIMATION-ERROR is signaled.
:return40: Three 40d0s are returned.
:raw: Just returns HVC as it is.
"
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

;;;
;;; For development
;;;

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
