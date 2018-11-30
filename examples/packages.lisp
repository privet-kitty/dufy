(cl:in-package :cl-user)

(defpackage #:dufy/examples
  (:nicknames #:dufy-examples)
  (:use #:common-lisp #:alexandria #:dufy)
  (:export #:draw-srgb-in-munsell))
