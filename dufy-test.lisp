(in-package :cl-user)

(defpackage dufy-test
  (:use :cl :fiveam))
(in-package :dufy-test)

(def-suite :dufy-suite)
(in-suite :dufy-suite)

(test test-core
  (is (equal '(50 100 150)
	     (apply #'dufy:xyz-to-rgb255 (dufy:rgb255-to-xyz 50 100 150)))))
