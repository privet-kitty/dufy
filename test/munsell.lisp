;;;
;;; Test for dufy/munsell
;;;

(in-package :dufy/test)

(def-suite munsell-suite :in dufy-suite)
(in-suite munsell-suite)

(defun ordered-in-circle-group-p (list &optional (perimeter 360d0))
  "Checks if for every triplet (X1 X2 X3) in LIST X2 is within
the (counterclockwise) interval [X1, X3] in a circle group. The last
element of LIST is regarded as followed by the first element."
  (loop for (x1 x2 x3) on (append list (subseq list 0 2))
        until (null x3)
        always (circular-member x2 x1 x3 perimeter)))

(test no-hue-reversal-exists
  (is (loop for value in '(0.2 0.4 0.6 0.8 1 2 3 4 5 6 7 8 9 10)
            always (loop for chroma from 2 to 50 by 2
                         always (ordered-in-circle-group-p
                                 (loop for hue40 from 0 below 40
                                       collect (nth-value 2 (mhvc-to-lchab-illum-c hue40 value chroma))))))))

(test round-trip-to-xyz
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (munsell-to-xyz
			(apply (rcurry #'xyz-to-munsell :digits 6) xyz)))))))

(test basic-set
  (is (nearly-equal 1d-4
                    '(0 0 0)
                    (multiple-value-list (dufy:munsell-to-lchab-illum-c "3RP 0/200"))))
  (is (nearly-equal 1d-4
                    '(100 0 0)
                    (multiple-value-list (dufy:munsell-to-lchab-illum-c "N 10"))))
  
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

