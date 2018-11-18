;;;
;;; Miscellaneous definitions, functions and macros
;;;

(in-package :dufy/internal)

;;
;; Utilities for preprocessing of data file
;;

(defparameter *dat-dir-path* (asdf:component-pathname (asdf:find-component "dufy" "dat")))

(defun array-to-list (array)
  "array -> list coercion"
  (let* ((dimensions (array-dimensions array))
         (indices (make-list (length dimensions) :initial-element 0)))
    (labels ((traverse (dimensions-rest indices-rest)
               (loop for idx of-type fixnum below (car dimensions-rest)
                     do (setf (car indices-rest) idx)
                     collect (if (null (cdr dimensions-rest))
                                 (apply #'aref array indices)
                                 (traverse (cdr dimensions-rest)
                                           (cdr indices-rest))))))
      (traverse dimensions indices))))

(defun print-make-array (var-name array &optional (stream t) (declaration t) (load-time-value nil))
  "Prints a form like (defparameter VAR-NAME (make-array ...))."
  (labels ((wrap-with-load-time-value (form)
             `(load-time-value ,form t)))
    (let ((typ (array-element-type array))
          (dims (array-dimensions array)))
      (when declaration
        (format stream "~S~%"
                `(declaim (type (simple-array ,typ ,dims)
                                ,(intern (string-upcase var-name))))))
      (format stream "(DEFPARAMETER ~A~% ~S)~%"
              (string-upcase var-name)
              (funcall (if load-time-value
                           #'wrap-with-load-time-value
                           #'identity)
                       `(make-array ',dims
                                    :element-type ',typ
                                    :initial-contents ',(array-to-list array)))))))

;;
;; For benchmark
;;

(defmacro internal-real-time (&body body)
  "For development. Returns elapsed (internal real) time."
  (let ((start (gensym)))
    `(let ((,start (get-internal-real-time)))
       ,@body
       (/ (float (- (get-internal-real-time) ,start) 1d0)
          internal-time-units-per-second))))

(defmacro time-after-gc (&body body)
  "TIME macro after GC"
  `(progn
     #+sbcl(sb-ext:gc :full t)
     #+ccl(ccl:gc)
     (time ,@body)))

(defmacro internal-real-time-after-gc (&body body)
  "INTERNAL-REAL-TIME macro after GC"
  `(progn
     #+sbcl(sb-ext:gc :full t)
     #+ccl(ccl:gc)
     (internal-real-time ,@body)))

(defmacro time-median (num &body body)
  "Repeats BODY NUM times and returns the median of elapsed (internal
real) times."
  (let ((i (gensym)))
    `(alexandria:median
      (loop for ,i below ,num
            collect (internal-real-time-after-gc ,@body)))))

(defun call-with-profiling (names func)
  "Works only on SBCL."
  (declare (ignorable names))
  #+sbcl (if (null names)
             (funcall func)
             (unwind-protect
                  (progn (eval `(sb-profile:profile ,@(ensure-list names)))
                         (funcall func)
                         (sb-profile:report :print-no-call-list nil))
               (eval `(sb-profile:unprofile ,@(ensure-list names)))))
  #-sbcl (funcall func))

(defmacro with-profiling (names &body body)
  "Works only on SBCL."
  (declare (ignorable names))
  #+sbcl `(unwind-protect
               (progn (sb-profile:profile ,@(ensure-list names))
                      ,@body
                      (sb-profile:report :print-no-call-list nil))
            (sb-profile:unprofile ,@(ensure-list names)))
  #-sbcl `(progn ,@body))

;;
;; Unclassified
;;

(defmacro dotimes-unroll ((var count &optional result) &body body)
  `(block nil
     ,@(loop for i from 0 below count
             collect `(let ((,var ,i)) ,@body))
     ,result))

(defmacro with-ensuring-type (type vars &body body)
  "Ensures and declares that the type of variables are TYPE."
  (labels ((expand (var-lst)
             (if (null var-lst)
                 nil
                 (cons `(,(car var-lst) (coerce ,(car var-lst) ',type))
                       (expand (cdr var-lst))))))
    `(let ,(expand vars)
       (declare (type ,type ,@vars))
       ,@body)))

(deftype tuple (&rest rest)
  "Analogy of the type specifier `cons' and the function `list':
type (TUPLE A B C) is equivalent to the type (CONS A (CONS B (CONS
C))); the type TUPLE (or (TUPLE)) is equivalent to the type NULL."
  (if (null rest)
      'null
      (reduce #'(lambda (x y) (if (null y) `(cons ,x) `(cons ,x ,y)))
              rest
              :from-end t
              :initial-value nil)))
