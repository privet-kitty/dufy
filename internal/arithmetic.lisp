(uiop:define-package :dufy/internal/arithmetic
  (:use :cl :alexandria)
  (:export #:TWO-PI
           #:+TWO-PI/360+
           #:+360/TWO-PI+
           #:degree-to-radian
           #:radian-to-degree
           #:nearly=
           #:nearly<=
           #:nearly-equal
           #:nearly-equal-values
           #:circular-nearer
           #:circular-clamp
           #:circular-lerp-loose
           #:circular-lerp
           #:circular-member
           #:square
           #:pow))

(in-package :dufy/internal/arithmetic)

;;
;; Degree and radian
;;

(define-constant TWO-PI (float (+ PI PI) 1d0))
(define-constant +TWO-PI/360+ (/ TWO-PI 360))
(define-constant +360/TWO-PI+ (/ 360 TWO-PI))

(declaim (inline degree-to-radian))
(defun degree-to-radian (degree)
  (* degree +TWO-PI/360+))
(declaim (inline radian-to-degree))
(defun radian-to-degree (radian)
  (* radian +360/TWO-PI+))

;;
;; Approximate comparison operators
;; (used mainly for test)
;;

(defun nearly= (threshold number &rest more-numbers)
  "THRESHOLD is acceptable absolute error."
  (if (null more-numbers)
      t
      (and (<= (abs (- number (car (the cons more-numbers)))) threshold)
           (apply #'nearly= threshold more-numbers))))

(defun nearly-equal (threshold lst1 &rest lsts)
  "THRESHOLD is acceptable absolute error."
  (if (null lst1)
      t
      (and (apply #'nearly= threshold
                  (car lst1)
                  (mapcar #'car lsts))
           (apply #'nearly-equal threshold
                  (cdr lst1)
                  (mapcar #'cdr lsts)))))

(defmacro nearly-equal-values (threshold &rest forms)
  `(nearly-equal ,threshold
                 ,@(mapcar #'(lambda (x) `(multiple-value-list ,x))
                           forms)))

(defun nearly<= (threshold number &rest more-numbers)
  (if (null more-numbers)
      t
      (and (<= (- number (car (the cons more-numbers))) threshold)
           (apply #'nearly<= threshold more-numbers))))

;;
;; Some arithmetic in a circle group.
;;
;; Note that these functions don't take `multiple laps' into
;; consideration: i.e. the length of the interval [-360, 360] in
;; R/360Z is zero.
;;

(declaim (inline circular-nearer))
(defun circular-nearer (theta1 x theta2 &optional (perimeter TWO-PI))
  "Compares the counterclockwise distances between THETA1 and X and
between X and THETA2, and returns THETA1 or THETA2 whichever is
nearer."
  (if (<= (mod (- x theta1) perimeter)
          (mod (- theta2 x) perimeter))
      theta1
      theta2))

(declaim (inline circular-clamp))
(defun circular-clamp (number min max &optional (perimeter TWO-PI))
  "A clamp function in a circle group. If NUMBER is not in
the (counterclockwise) closed interval [MIN, MAX], CIRCULAR-CLAMP
returns MIN or MAX whichever is nearer to NUMBER."
  (let ((mnumber (mod number perimeter))
        (mmin (mod min perimeter))
        (mmax (mod max perimeter)))
    (if (<= mmin mmax)
        (if (<= mmin mnumber mmax)
            number ; min <= number <= max
            (circular-nearer max number min)) ; min <= max < number or number < min <= max
        (if (or (<= mnumber mmax)  (<= mmin mnumber))
            number ; number <= max <= min or max <= min <= number
            (circular-nearer max number min))))) ; max < number < min

(declaim (inline circular-lerp))
(defun circular-lerp (coef theta1 theta2 &optional (perimeter TWO-PI))
  "Counterclockwise linear interpolation from THETA1 to THETA2 in a
circle group. It is guaranteed that the returned value doesn't exceed
the given interval from THETA1 to THETA2 if COEF is in [0, 1]. It is,
however, slower than CIRCULAR-LERP-LOOSE."
  (let ((length (mod (- theta2 theta1) perimeter)))
    (circular-clamp (+ theta1 (* length coef))
                    theta1
                    theta2
                    perimeter)))

(declaim (inline circular-lerp-loose))
(defun circular-lerp-loose (coef theta1 theta2 &optional (perimeter TWO-PI))
  "Counterclockwise linear interpolation from THETA1 to THETA2 in a
circle group. There is a possibility that the returned value slightly
exceeds the interval [THETA1, THETA2] due to floating-point error. If
that is incovenient, you should use CIRCULAR-LERP instead."
  (let ((length (mod (- theta2 theta1) perimeter)))
    (mod (+ theta1 (* length coef)) perimeter)))

(declaim (inline circular-member))
(defun circular-member (number theta1 theta2 &optional (perimeter TWO-PI))
  "Returns true if NUMBER is within the counterclockwise closed interval [THETA1,
THETA2] in a circle group."
  (let ((mnumber (mod number perimeter))
        (mtheta1 (mod theta1 perimeter))
        (mtheta2 (mod theta2 perimeter)))
    (if (<= mtheta1 mtheta2)
        (and (<= mtheta1 mnumber)
             (<= mnumber theta2))
        (or (<= mtheta1 mnumber)
            (<= mnumber theta2)))))

;;
;; Miscellaneous arithmetic
;;

(defmacro pow (base power)
  "Does fast exponentiation by squaring. POWER must be a literal of
type (integer 1)."
  (assert (constantp power))
  (check-type power (integer 1))
  (labels ((round-off-to-power-of-2 (num)
             (let* ((approx (log num 2))
                    (floor (expt 2 (floor approx)))
                    (ceil (expt 2 (ceiling approx))))
               (if (<= ceil num) ceil floor)))
           (decompose-to-sum-of-powers-of-2 (num &optional result)
             (if (zerop num)
                 result
                 (let ((k (round-off-to-power-of-2 num)))
                   (decompose-to-sum-of-powers-of-2 (- num k) (cons k result))))))
    (let* ((components (decompose-to-sum-of-powers-of-2 power))
           (max (apply #'max (cons 1 components)))
           (vars (apply #'vector
                        (loop for i from 0 to (round (log max 2))
                              collect (gensym (format nil "POW~A-" (expt 2 i)))))))
      `(let* ((,(aref vars 0) ,base) 
              ,@(loop for i from 1
                      for num = (expt 2 i)
                      until (> num max)
                      collect `(,(aref vars i) (* ,(aref vars (- i 1))
                                                  ,(aref vars (- i 1))))))
         ,(if (= 1 (length components))
              (last-elt vars)
              `(* ,@(mapcar #'(lambda (num) (aref vars (round (log num 2))))
                            components)))))))

(declaim (inline square))
(defun square (x) (* x x))
