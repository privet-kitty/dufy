(in-package :dufy/internal)

;;
;; Constants
;;

(define-constant TWO-PI (float (+ PI PI) 1d0))
(define-constant +TWO-PI/360+ (/ TWO-PI 360))
(define-constant +360/TWO-PI+ (/ 360 TWO-PI))


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
  (let ((number$ (mod number perimeter))
        (min$ (mod min perimeter))
        (max$ (mod max perimeter)))
    (if (<= min$ max$)
        (if (<= min$ number$ max$)
            number ; [min, number, max]
            (circular-nearer max number min)) ; [min, max, number] or [number, min, max]
        (if (or (<= number$ max$)  (<= min$ number$))
            number ;[number, max, min] or [max, min, number]
            (circular-nearer max number min))))) ; [max, number, min]

(declaim (inline circular-lerp))
(defun circular-lerp (coef theta1 theta2 &optional (perimeter TWO-PI))
  "Counterclockwise linear interpolation from THETA1 to THETA2 in a
circle group. It is guaranteed that the return value doesn't exceed
the given interval from THETA1 to THETA2 if COEF is in [0, 1]. It is,
however, slower than CIRCULAR-LERP-LOOSE."
  (let ((dtheta (mod (- theta2 theta1) perimeter)))
    (circular-clamp (+ theta1 (* dtheta coef))
                    theta1
                    theta2
                    perimeter)))

(declaim (inline circular-lerp-loose))
(defun circular-lerp-loose (coef theta1 theta2 &optional (perimeter TWO-PI))
  "Counterclockwise linear interpolation from THETA1 to THETA2 in a
circle group. There is a possibility that the return value slightly
exceeds the interval [THETA1, THETA2] due to floating-point error. If
that is incovenient, you should use CIRCULAR-LERP instead."
  (let ((dtheta (mod (- theta2 theta1) perimeter)))
    (mod (+ theta1 (* dtheta coef)) perimeter)))

(declaim (inline circular-member))
(defun circular-member (x theta1 theta2 &optional (perimeter TWO-PI))
  "Returns true if X is within the counterclockwise closed interval [THETA1,
THETA2] in a circle group."
  (let ((x-m (mod x perimeter))
        (theta1-m (mod theta1 perimeter))
        (theta2-m (mod theta2 perimeter)))
    (if (<= theta1-m theta2-m)
        (and (<= theta1-m x-m)
             (<= x-m theta2))
        (or (<= theta1-m x-m)
            (<= x-m theta2)))))


;;
;; Miscellaneous arithmetic
;;

(defmacro fast-expt (base power)
  "Exponentiation by squaring. POWER must be a literal of
type (integer 1)."
  (assert (constantp power))
  (check-type power (integer 1))
  (labels ((round-off-to-power-of-2 (num)
             (let* ((approx (log num 2))
                    (flo (expt 2 (floor approx)))
                    (ceil (expt 2 (ceiling approx))))
               (if (<= ceil num) ceil flo)))
           (decompose-to-sum-of-powers-of-2 (num result)
             (if (zerop num)
                 result
                 (let ((k (round-off-to-power-of-2 num)))
                   (decompose-to-sum-of-powers-of-2 (- num k) (cons k result))))))
    (let* ((parts (decompose-to-sum-of-powers-of-2 power nil))
           (m (apply #'max (cons 1 parts)))
           (vars (apply #'vector
                        (loop for i from 0 to (round (log m 2))
                              collect (gensym (format nil "POW~A-" (expt 2 i)))))))
      `(let* ((,(aref vars 0) ,base) 
              ,@(loop for i from 1
                      for num = (expt 2 i)
                      until (> num m)
                      collect `(,(aref vars i) (* ,(aref vars (- i 1))
                                                  ,(aref vars (- i 1))))))
         ,(if (= 1 (length parts))
              (last-elt vars)
              `(* ,@(loop for num in parts
                          collect (aref vars (round (log num 2))))))))))

(declaim (inline square))
(defun square (x) (* x x))
