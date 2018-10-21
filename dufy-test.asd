;;;; -*- Mode: Lisp;-*-

(defsystem "dufy-test"
  :description "Test system for Dufy"
  :author "Hugo I."
  :license "MIT"
  :depends-on ("dufy" "dufy-extra-data" "dufy-examples" "fiveam" "cl-csv" "parse-float")
  :components ((:file "dufy-test"))
  :perform (test-op (o s)
		    (uiop:eval-thunk "(fiveam:run! :dufy-suite)")))
