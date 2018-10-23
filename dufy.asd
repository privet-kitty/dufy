;;;; -*- Mode: Lisp;-*-

(defsystem "dufy"
  :version "0.2.9"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :serial t
  :depends-on ("dufy-core" "dufy-munsell")
  :components ((:module "dat"
                :components ((:static-file "ciede2000-test-data.csv")))
               (:module "src"
                :components ((:file "package"))))
  :in-order-to ((test-op (test-op "dufy/test"))))

(defsystem "dufy/test"
  :description "Test system for Dufy"
  :author "Hugo I."
  :license "MIT"
  :depends-on ("dufy"
               "fiveam"
               "cl-csv"
               "parse-float"
               ;; There's no specific test for dufy-extra-data and dufy-examples;
               ;; the following is just for checking if load-op succeeds.
               "dufy-extra-data"
               (:feature (:and (:or :sbcl :ccl) :x86-64) "dufy-examples"))
  :components ((:file "dufy-test"))
  :perform (test-op (o s)
		    (uiop:eval-thunk "(fiveam:run! :dufy-suite)")))
