;;;; clcltool.asd -*- Mode: Lisp;-*-

(in-package :cl-user)

(asdf:defsystem :clcltool
  :description "CLCL examples"
  :author "privet-kitty"
  :license "MIT"
  :serial t
  :depends-on (:clcl :nibbles :alexandria)
  :components ((:file "munselltool"))
)
