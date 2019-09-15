;;;; -*- Mode: Lisp;-*-

(defsystem "dufy"
  :version "0.4.0"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :depends-on ("dufy/core" "dufy/munsell" "dufy/hsluv")
  :components ((:module "dat"
                :components ((:static-file "ciede2000-test-data.csv")
                             (:static-file "FL3.x.tsv")))
               (:module "src"
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op "dufy/test"))))

(defsystem "dufy/internal"
  :description "Common definitions not exported in the main package"
  :pathname "src"
  :serial t
  :depends-on ("alexandria" "trivia")
  :components ((:module "internal"
                :components
                ((:file "package")
                 (:file "utilities")
                 (:file "arithmetic")
                 (:file "matrix")
                 (:file "colorspace")
                 (:file "mb-line")))))

(defsystem "dufy/core"
  :pathname "src"
  :serial t
  :depends-on ("alexandria" "dufy/internal")
  :components ((:module "core"
                :components
                ((:file "package")
                 (:file "cmf-data")
                 (:file "spectrum")
                 (:file "illuminants-data")
                 (:file "xyz")
                 (:file "rgb")
                 (:file "lab-and-luv")
                 (:file "cat")
                 (:file "builtin-rgbspaces")
                 (:file "deltae")))))

(defsystem "dufy/hsluv"
  :description "HSLuv and HPLuv color spaces"
  :pathname "src"
  :serial t
  :depends-on ("dufy/core")
  :components ((:module "hsluv"
                :components
                ((:file "package")
                 (:file "hsluv")))))

(defsystem "dufy/munsell"
  :description "Munsell Color System"
  :pathname "src"
  :serial t
  :depends-on ("dufy/core" "cl-ppcre")
  :components ((:module "munsell"
                :components
                ((:file "package")
                 (:file "y-to-value-data")
                 (:file "renotation-data")
                 (:file "inversed-renotation-data")
                 (:file "fundamental")
                 (:file "convert")
                 (:file "invert")))))

(defsystem "dufy/extra-data"
  :pathname "src"
  :serial t
  :depends-on ("dufy/core")
  :components ((:module "extra-data"
                :components
                ((:file "package")
                 (:file "illuminants")
                 (:file "illuminants-f3-series")
                 (:file "illuminants-lamps")))))

(defsystem "dufy/examples"
  :pathname "src"
  :serial t
  :depends-on ("dufy" "lispbuilder-sdl" "iterate" "alexandria" "lparallel")
  :components ((:module "examples"
                :components ((:file "package")
                             (:file "visualize-munsell")))))

(defsystem "dufy/test"
  :pathname "test"
  :serial t
  :depends-on ("dufy"
               "fiveam"
               "cl-csv"
               "parse-float"
               ;; There's no specific test for dufy/extra-data and dufy/examples;
               ;; the following is just for checking if load-op succeeds.
               "dufy/extra-data"
               (:feature (:and (:or :sbcl :ccl) :x86-64) "dufy/examples"))
  :components ((:file "package")
               (:file "core")
               (:file "hsluv")
               (:file "munsell"))
  :perform (test-op (o s)
                    (uiop:eval-input "(fiveam:run! 'dufy/test:main-suite)")))
