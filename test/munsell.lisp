(in-package :dufy/test)

(def-suite munsell-suite :in dufy-suite)
(in-suite munsell-suite)

(test test-munsell
  (is (nearly-equal 1d-4
                    '(0 0 0)
                    (multiple-value-list (dufy:munsell-to-lchab-illum-c "3RP 0/200"))))
  (is (nearly-equal 1d-4
                    '(100 0 0)
                    (multiple-value-list (dufy:munsell-to-lchab-illum-c "N 10"))))
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (multiple-value-call #'munsell-to-xyz
			 (nth-value 0 (apply (rcurry #'xyz-to-munsell :digits 6)
					     xyz)))))))
  (is (nearly-equal 1d-4
                    '(0.6355467107666922d0 0.06136344507966737d0 0.004921707437403791d0)
                    (multiple-value-list (munsell-to-xyz "2.3R 3.1/50.1"))))
  (is (nearly-equal 1d-4
                    '(0.09212673811227157d0 0.06799167763718869d0 0.05724974585563483d0)
                    (multiple-value-list (munsell-to-xyz "2.3R 3.1/5.1"))))
  (is (nearly-equal 1d-4
                    '(0.0014506484808043598d0 0.0011663458924852145d0 0.0014582198016197035d0)
                    (multiple-value-list (munsell-to-xyz "2.3R 0.1/0.2"))))
  (is (nearly-equal 1d-9
                    '(0.31006249258289614d0 0.31615894048704557d0 0.06827557003978901d0)
                    (multiple-value-list
                     (multiple-value-call #'dufy:xyz-to-xyy
                       (dufy:munsell-to-xyz-illum-c "N 3.1"))))))

