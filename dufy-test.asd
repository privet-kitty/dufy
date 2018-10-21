;;;; -*- Mode: Lisp;-*-

(defsystem "dufy-test"
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
               (:feature (:or :sbcl :ccl) "dufy-examples"))
  :components ((:file "dufy-test"))
  :perform (test-op (o s)
		    (uiop:eval-thunk "(fiveam:run! :dufy-suite)")))
