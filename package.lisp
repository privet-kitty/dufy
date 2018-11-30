(cl:in-package :cl-user)

(uiop:define-package dufy
    (:use-reexport :dufy/core :dufy/munsell)
  (:unintern
   #:colorspace
   #:define-colorspace
   #:primary-converter
   #:define-primary-converter
   #:defconverter
   #:defconverters
   #:functional
   #:define-functional
   #:extend-functional))
