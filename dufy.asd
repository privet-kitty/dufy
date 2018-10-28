;;;; -*- Mode: Lisp;-*-

(defsystem "dufy"
  :version "0.2.9"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :depends-on ("dufy/core" "dufy/munsell")
  :components ((:module "dat"
                :components ((:static-file "ciede2000-test-data.csv")))
               (:module "src"
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op "dufy/test"))))

(defsystem "dufy/internal"
  :description "Common definitions that are not exported in the main package"
  :pathname "src"
  :serial t
  :depends-on ("alexandria")
  :components ((:module "internal"
                :components
                ((:file "package")
                 (:file "utilities")
                 (:file "matrix")))))

(defsystem "dufy/core"
  :pathname "src"
  :serial t
  :depends-on ("alexandria" "dufy/internal")
  :components ((:module "core"
		:components
		((:file "package")
                 (:file "colorspace")
		 (:file "cmf-data")
		 (:file "xyz")
                 (:file "illuminants-data")
		 (:file "rgb")
		 (:file "lab-and-luv")
		 (:file "cat")
                 (:file "rgbspaces-data")
		 (:file "deltae")))))

(defsystem "dufy/munsell"
  :pathname "src"
  :serial t
  :depends-on ("dufy/core" "cl-ppcre")
  :components ((:module "munsell"
		:components
		((:file "package")
                 (:file "y-to-value-data")
                 (:file "munsell-renotation-data")
		 (:file "munsell")))))

(defsystem "dufy/extra-data"
  :pathname "src"
  :serial t
  :depends-on ("dufy/core")
  :components ((:module "extra-data"
		:components
		((:file "package")
                 (:file "illuminants-data")
                 (:file "illuminants-f3-series")
                 (:file "illuminants-gas-discharge-lamps")))))

(defsystem "dufy/examples"
  :description "Examples of dufy"
  :pathname "src"
  :serial t
  :depends-on ("dufy" "lispbuilder-sdl" "iterate" "alexandria" "lparallel")
  :components ((:module "examples"
                :components ((:file "packages")
                             (:file "show-munsell-space")))))

(defsystem "dufy/test"
  :description "Test system for dufy"
  :depends-on ("dufy"
               "fiveam"
               "cl-csv"
               "parse-float"
               ;; There's no specific test for dufy-extra-data and dufy-examples;
               ;; the following is just for checking if load-op succeeds.
               "dufy/extra-data"
               (:feature (:and (:or :sbcl :ccl) :x86-64) "dufy/examples"))
  :components ((:file "dufy-test"))
  :perform (test-op (o s)
		    (uiop:eval-thunk "(fiveam:run! 'dufy/test:dufy-suite)")))
