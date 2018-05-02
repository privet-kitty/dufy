(cl:in-package :cl-user)

(defpackage dufy.package
  (:use :cl))
(in-package :dufy.package)

(uiop:define-package dufy
    (:use-reexport :dufy.core :dufy.munsell))
