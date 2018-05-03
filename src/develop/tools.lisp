(cl:in-package :cl-user)

(defpackage dufy.develop
  (:use :cl)
  (:export :print-make-array))

(in-package :dufy.develop)

(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (indices (make-list (length dimensions) :initial-element 0)))
    (labels ((recurse (dimensions-rest indices-rest)
               (loop for idx of-type fixnum below (car dimensions-rest)
                  do (setf (car indices-rest) idx)
                  collect (if (null (cdr dimensions-rest))
                              (apply #'aref array indices)
                              (recurse (cdr dimensions-rest)
                                       (cdr indices-rest))))))
      (recurse dimensions indices))))


(defun print-make-array (var-name array &optional (stream t) (declaration t) (load-time-value nil))
  "Prints a code like (defparameter VAR-NAME (make-array ...))"
  (let ((typ (array-element-type array))
	(dims (array-dimensions array)))
    (when declaration
      (format stream "~S~%"
              `(declaim (type (simple-array ,typ ,dims)
                              ,(intern (string-upcase var-name))))))
    (format stream "(defparameter ~A ~A~% #.~S~A)~%"
            var-name
            (if load-time-value "(load-time-value" "")
            `(make-array (quote ,dims)
                         :element-type (quote ,typ)
                         :initial-contents (quote ,(array-to-list array)))
            (if load-time-value " t)" ""))))
