(in-package :cl-user)

(defpackage dufy-test
  (:use :cl :fiveam :dufy))
(in-package :dufy-test)

(def-suite :dufy-suite)
(in-suite :dufy-suite)

(let ((*read-default-float-format* 'double-float))
  (test test-core
    (is (equal '(50 100 150)
	       (apply #'dufy:xyz-to-qrgb (dufy:qrgb-to-xyz 50 100 150))))
    (is (nearly= 1d-3
		 (deltae00 63.0109 -31.0961 -5.8663
			   62.8187 -29.7946 -4.0864)
		 1.2630))))
