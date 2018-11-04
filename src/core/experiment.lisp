(in-package :dufy-core)

;;;
;;; This file is not a part of this library but an experimental code
;;; in development. It is for the comparison between list and multiple
;;; values.
;;;

(deftype %list (&rest rest)
  "Analogy of the type specifier `cons' and the function
`list'. Type (%list a b c) is equivalent to the type (cons a (cons
b (cons c)))."
  (reduce #'(lambda (x y) (if (null y) `(cons ,x) `(cons ,x ,y)))
          rest
          :from-end t
          :initial-value nil))

(defmacro typed-destructuring-bind (var-list expression &body body)
  "Typed `destructuring-bind'. The difference from the standard
`destructuring-bind' is as follows:

1. The structure can only be a non-nested proper-list without
lambda-list keywords (e.g. &optional).

2. The variables can be typed:
e.g. (destructuring-bind ((a (double-float 0d0 360d0)) (b function))
...). Then the type declaration for these variables are added. A
non-typed variable is assumed to have the type T.

3. The returned list of `expression' is declared to have the
type (CONS [type1] (CONS [type2] ... (CONS [typeN]))) and to be
dynamic extent."
  (assert (proper-list-p var-list))
  (let* ((typed-var-list (mapcar #'(lambda (x)
                                     (if (listp x)
                                         x
                                         (list x t)))
                                 var-list))
         (var-names (mapcar #'first typed-var-list))
         (var-types (mapcar #'second typed-var-list)))
    (labels ((expand (var-names var-types form-returning-list dynamic-extent-p)
               (if (null var-names)
                   `(progn ,@body)
                   (let ((tmp-list (gensym)))
                     `(let* ((,tmp-list ,form-returning-list)
                             (,(car var-names) (car ,tmp-list)))
                        (declare ,@(when dynamic-extent-p
                                     `((dynamic-extent ,tmp-list)))
                                 (type (%list ,@var-types) ,tmp-list)
                                 (type ,(car var-types) ,(car var-names)))
                        ,(expand (cdr var-names)
                                 (cdr var-types)
                                 `(cdr ,tmp-list)
                                 ;; Only has to stack-allocate the beginning list.
                                 nil))))))
      (expand var-names var-types expression t))))


(defmacro typed-destructuring-bind2 (var-list expression &body body)
  "Works on SBCL but is probably illegal, because `returned-list' is
popped against its type declaration."
  (assert (proper-list-p var-list))
  (let* ((typed-var-list (mapcar #'(lambda (x)
                                     (if (listp x)
                                         x
                                         (list x t)))
                                 var-list))
         (var-names (mapcar #'first typed-var-list))
         (var-types (mapcar #'second typed-var-list))
         (returned-list (gensym)))
    `(let* ((,returned-list ,expression)
            ,@(loop for name in var-names
                    collect `(,name (pop ,returned-list))))
       (declare (dynamic-extent ,returned-list)
                (type (%list ,@var-types) ,returned-list)
                ,@(loop for type in var-types
                        for name in var-names
                        collect `(type ,type ,name)))
       ,@body)))

(declaim (inline list-lchab-to-lab)
         (ftype (function * (values (%list double-float double-float double-float) &optional)) list-lchab-to-lab))
(defun list-lchab-to-lab (lstar cstarab hab)
  (declare (real lstar) (real cstarab) (real hab))
  (declare (optimize (speed 3) (safety 1)))
  (with-ensuring-type double-float (lstar cstarab hab)
    (let ((hue-two-pi (* hab +two-pi/360+)))
      (list lstar
            (* cstarab (cos hue-two-pi))
            (* cstarab (sin hue-two-pi))))))

(declaim (inline list-lab-to-xyz)
         (ftype (function * (values (%list double-float double-float double-float) &optional))
                list-lab-to-xyz))
(defun list-lab-to-xyz (lstar astar bstar &key (illuminant +illum-d65+))
  (declare (real lstar) (real astar) (real bstar))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((fy (* (+ (float lstar 1.0d0) 16.0d0) 1/116))
         (fx (+ fy (* (float astar 1.0d0) 0.002d0)))
         (fz (- fy (* (float bstar 1.0d0) 0.005d0))))
    (list
     (if (> fx 0.20689655172413793d0)
         (* (illuminant-x illuminant) fx fx fx)
         (* (- fx 0.13793103448275862d0) 0.12841854934601665d0
            (illuminant-x illuminant)))
     (if (> fy 0.20689655172413793d0)
         (* fy fy fy)
         (* (- fy 0.13793103448275862d0) 0.12841854934601665d0))
     (if (> fz 0.20689655172413793d0)
         (* (illuminant-z illuminant) fz fz fz)
         (* (- fz 0.13793103448275862d0) 0.12841854934601665d0
            (illuminant-z illuminant))))))

(declaim (inline list-multiply-mat-vec)
         (ftype (function * (values (%list double-float double-float double-float) &optional))
                list-multiply-mat-vec))
(defun list-multiply-mat-vec (matrix x y z)
  (declare (optimize (speed 3) (safety 1))
  	   (matrix33 matrix))
  (with-ensuring-type double-float (x y z)
    (list (+ (* x (aref matrix 0 0))
	     (* y (aref matrix 0 1))
	     (* z (aref matrix 0 2)))
	  (+ (* x (aref matrix 1 0))
	     (* y (aref matrix 1 1))
	     (* z (aref matrix 1 2)))
	  (+ (* x (aref matrix 2 0))
	     (* y (aref matrix 2 1))
	     (* z (aref matrix 2 2))))))

(declaim (inline list-xyz-to-lrgb)
         (ftype (function * (values (cons double-float (cons double-float (cons double-float))) &optional))
                list-xyz-to-lrgb))
(defun list-xyz-to-lrgb
    (x y z &key (rgbspace +srgb+) &aux (illuminant (rgbspace-illuminant rgbspace)))
  (declare (real x) (real y) (real z))
  (declare (optimize (speed 3) (safety 1))
           (ignorable illuminant))
  (list-multiply-mat-vec (rgbspace-from-xyz-matrix rgbspace)
                         (float x 1.0d0)
                         (float y 1.0d0)
                         (float z 1.0d0)))

(declaim (inline lchab-to-lrgb)
         (ftype (function * (values double-float double-float double-float &optional))
                lchab-to-lrgb))
(defun lchab-to-lrgb (lstar cstarab hab &key (rgbspace +srgb+)
                      &aux (illuminant (rgbspace-illuminant rgbspace)))
  (declare (optimize (speed 3) (safety 1))
           (type real lstar cstarab hab))
  "Current implementation. (automatically generated by
`dufy-core::defconverter')"
  (multiple-value-call #'xyz-to-lrgb
    (multiple-value-call #'lab-to-xyz
      (lchab-to-lab lstar cstarab hab)
      :illuminant
      illuminant)
    :rgbspace
    rgbspace))

(declaim (inline list-lchab-to-lrgb)
         (ftype (function * (values (%list double-float double-float double-float) &optional))
                list-lchab-to-lrgb))
(defun list-lchab-to-lrgb
    (lstar cstarab hab &key (rgbspace +srgb+) &aux (illuminant (rgbspace-illuminant rgbspace)))
  (declare (optimize (speed 3) (safety 1))
           (type real lstar cstarab hab))
  (typed-destructuring-bind ((lstar double-float) (astar double-float) (bstar double-float))
      (list-lchab-to-lab lstar cstarab hab)
    (typed-destructuring-bind ((x double-float) (y double-float) (z double-float))
        (list-lab-to-xyz lstar astar bstar :illuminant illuminant)
      (list-xyz-to-lrgb x y z :rgbspace rgbspace))))

(defun bench-mv-version (num &optional (sample 10))
  (let ((state (sb-ext:seed-random-state 1)))
    (format t "~&~F sec."
            (time-median sample
              (print (loop repeat num
                           sum (multiple-value-call
                                   #'(lambda (x y z)
                                       (declare (double-float x y z))
                                       (+ x y z))
                                   (lchab-to-lrgb (random 100d0 state)
                                                  (- (random 128d0 state) 256d0)
                                                  (- (random 128d0 state) 256d0)))))))))

(defun bench-list-version (num &optional (sample 10))
  (let ((state (sb-ext:seed-random-state 1)))
    (format t "~&~F sec."
            (time-median sample
              (print (loop repeat num
                           sum (destructuring-bind (x y z)
                                   (list-lchab-to-lrgb (random 100d0 state)
                                                       (- (random 128d0 state) 256d0)
                                                       (- (random 128d0 state) 256d0))
                                 (declare (double-float x y z))
                                 (+ x y z))))))))
