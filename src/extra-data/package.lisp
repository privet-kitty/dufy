(cl:in-package :cl-user)

(defpackage dufy-extra-data
  (:use :common-lisp :dufy-core)
  #.`(:export
      :+illum-d55+
      :+illum-d75+
      ,@(loop for i from 1 to 12
              collect (intern (format nil "+ILLUM-F~A+" i) :keyword))
      ,@(loop for i from 1 to 15
              collect (intern (format nil "+ILLUM-F3.~A+" i) :keyword))))
