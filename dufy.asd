;;;; -*- Mode: Lisp;-*-

(defsystem "dufy"
  :version "0.3.1"
  :description "Color library for Common Lisp"
  :author "Hugo I."
  :license "MIT"
  :defsystem-depends-on ("wild-package-inferred-system")
  :class "wpis:wild-package-inferred-system"
  :add-non-wild-nickname t
  :depends-on ("dufy/core/*" "dufy/munsell/*")
  :components ((:module "dat"
                :components ((:static-file "ciede2000-test-data.csv"))))
  :in-order-to ((test-op (test-op "dufy/test"))))

(defsystem "dufy/test"
  :depends-on ("dufy/test/*"
               ;; There's no specific test for dufy/extra-data and dufy/examples;
               ;; the following is just for checking if load-op succeeds.
               "dufy/extra-data/*"
               (:feature (:and (:or :sbcl :ccl) :x86-64) "dufy/examples/*"))
  :perform (test-op (o s)
                    (uiop:eval-input "(fiveam:run! :dufy-suite)")))
