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
           ((double-float 0d0 #.*maximum-chroma*) half-chroma)
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
           ((double-float 0d0 #.*maximum-chroma*) half-chroma))
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
        (chroma (clamp (float chroma 1d0) 0d0 *maximum-chroma*)))
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

(declaim (inline lstar-to-munsell-value))
(defun lstar-to-munsell-value (lstar)
  (declare (optimize (speed 3) (safety 1)))
  (y-to-munsell-value (lstar-to-y (float lstar 1d0))))

(defun rough-lchab-to-mhvc (lstar cstarab hab)
  "For devel. Does rough conversion from LCHab to munsell HVC"
  (declare (optimize (speed 3) (safety 0))
           (double-float lstar cstarab hab))
  (values (* hab #.(float 40/360 1d0))
          (lstar-to-munsell-value lstar)
          (* cstarab #.(/ 5.5d0))))

;; In the following functions, the actual L* equals 10*LSTAR/10; the
;; actual C*ab equals 20*CSTARAB/20; the actual hab equals 9*HAB/9.
(defun lchab-to-mhvc-all-integer-case (lstar/10 cstarab/20 hab/9)
  (declare (fixnum lstar/10 cstarab/20 hab/9))
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
  (let ((tmp-hue40 init-hue40)
        (v (lstar-to-munsell-value lstar))
        (tmp-chroma init-chroma))
    (declare (double-float tmp-hue40 tmp-chroma v))
    (if (or (<= v threshold) (<= init-chroma threshold))
        (values init-hue40 v init-chroma)
        (dotimes (i max-iteration
                    (ecase if-reach-max
                      (:error
                       (error (make-condition 'large-approximation-error
                                              :message "INVERT-MHVC-TO-LCHAB reached MAX-ITERATION without achieving sufficient accuracy.")))
                      (:return40 (values 40d0 40d0 40d0))
                      (:raw (values (mod tmp-hue40 40d0) v tmp-chroma))))
          (multiple-value-bind (_ tmp-cstarab tmp-hab)
              (mhvc-to-lchab-illum-c tmp-hue40 v tmp-chroma)
            (declare (ignore _))
            (let* ((delta-cstarab (- cstarab tmp-cstarab))
                   (delta-hab (circular-delta hab tmp-hab))
                   (delta-hue40 (* delta-hab #.(float 40/360 1d0)))
                   (delta-chroma (* delta-cstarab #.(/ 5.5d0))))
              (if (and (<= (abs delta-hue40) threshold)
                       (<= (abs delta-chroma) threshold))
                  (return-from invert-mhvc-to-lchab
                    (values (mod tmp-hue40 40d0) v tmp-chroma))
                  (setf tmp-hue40 (+ tmp-hue40
                                     (* factor delta-hue40))
                        tmp-chroma (max 0d0 (+ tmp-chroma
                                               (* factor delta-chroma)))))))))))


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



(defun test-inverter ()
  "For development."
  (let ((max-iteration 300))
    (do ((lstar 0 (+ lstar 10)))
        ((> lstar 100) 'done)
      (do ((hab 0 (+ hab 9)))
          ((= hab 360))
        (do ((cstarab 0 (+ cstarab 10)))
            ((= cstarab 200) nil)
          (let ((result (lchab-to-mhvc-illum-c lstar cstarab hab
                                               :threshold 1d-6
                                               :if-reach-max :return40
                                               :max-iteration max-iteration)))
            (when (= result 40d0)
              (format t "failed at L*=~A C*ab=~A Hab=~A.~%" lstar cstarab hab))))))))

(defun test-inverter2 (&optional (num-loop 10000) (profile nil) (rgbspace +srgb+))
  "For development."
  (funcall
   #'call-with-profiling
   (when profile
     '("DUFY/MUNSELL" "DUFY/CORE" "DUFY/INTERNAL"))
   #'(lambda ()
       (let ((qmax+1 (1+ (rgbspace-qmax rgbspace)))
             (cat-func (gen-cat-function (rgbspace-illuminant rgbspace) +illum-c+))
             (sum 0))
         (dotimes (x num-loop (float (/ sum num-loop) 1d0))
           (let ((qr (random qmax+1))
                 (qg (random qmax+1))
                 (qb (random qmax+1)))
             (multiple-value-bind (lstar cstarab hab)
                 (multiple-value-call #'xyz-to-lchab
                   (multiple-value-call cat-func
                     (qrgb-to-xyz qr qg qb :rgbspace rgbspace))
                   :illuminant +illum-c+)
               (let ((result (lchab-to-mhvc-illum-c lstar cstarab hab
                                                    :max-iteration 300
                                                    :if-reach-max :return40
                                                    :factor 0.5d0)))
                 (when (= result 40d0)
                   (incf sum)
                   (format t "~A ~A ~A, (~a ~a ~a)~%" lstar cstarab hab qr qg qb))))))))))

;; Doesn't converge at:
;; LCH = 90.25015693115249d0 194.95626408656423d0 115.6958104971207d0
;; in ProPhoto space, 16-bit

(defun test-inverter3 (&optional (rgbspace +srgb+))
  "For development."
  (declare (optimize (speed 3) (safety 1)))
  (let ((sum 0))
    (dotimes (qr 256)
      (print qr)
      (dotimes (qg 256)
        (dotimes (qb 256)
          (let ((result (multiple-value-call #'xyz-to-mhvc 
                          (qrgb-to-xyz qr qg qb :rgbspace rgbspace)
                          :if-reach-max :return40
                          :max-iteration 200
                          :threshold 1.0d-3)))
            (when (= result 40d0)
              (incf sum)
              (format t "(~a ~a ~a)~%" qr qg qb))))))
    (float (/ sum (* 256 256 256)) 1d0)))
