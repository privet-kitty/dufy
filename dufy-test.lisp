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
		 1.2630))
    (is (equal '(200 2000 3000)
	       (apply (alexandria:rcurry #'dufy:xyz-to-qrgb :rgbspace dufy:+bg-srgb-12+)
		      (dufy:qrgb-to-xyz 200 2000 3000 dufy:+bg-srgb-12+))))
    (is (equal '(200 2000 3000)
	       (apply (alexandria:rcurry #'dufy:xyz-to-qrgb :rgbspace dufy:+scrgb-nl+)
		      (dufy:qrgb-to-xyz 200 2000 3000 dufy:+scrgb-nl+))))
    (is (= 3363638200
	   (apply (alexandria:rcurry #'dufy:xyz-to-hex :rgbspace dufy:+bg-srgb-12+)
		  (dufy:hex-to-xyz 3363638200 dufy:+bg-srgb-12+))))
    (is (= 3363638200
	   (apply (alexandria:rcurry #'dufy:xyz-to-hex :rgbspace dufy:+scrgb-nl+)
		  (dufy:hex-to-xyz 3363638200 dufy:+scrgb-nl+))))))
