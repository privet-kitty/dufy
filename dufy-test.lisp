(in-package :cl-user)

(defpackage :dufy/test
  (:use #:cl #:fiveam #:dufy #:dufy/internal #:alexandria #:cl-csv #:parse-float)
  (:export #:dufy-suite))
(in-package :dufy/test)

(def-suite dufy-suite)
(in-suite dufy-suite)

;;;
;;; Test Data
;;;

(defparameter *xyz-set*
  '((0.6018278793248849d0 0.3103175002768938d0 0.028210681843353954d0)
    (4.846309924502165d-6 5.099078671483812d-6 5.552388656107547d-6)
    (0.9504285453771808d0 1.0000000000000202d0 1.0889003707981277d0)))

(defparameter *xyy-set*
  (mapcar #'(lambda (lst) (multiple-value-list (apply #'xyz-to-xyy lst)))
	  *xyz-set*))

(defparameter *qrgb16-set*
  '((65535 65534 65533) (0 1000 2000) (-1000 6000 70000)))

(defparameter *rgb-set*
  '((1d0 0d0 0d0) (0.1d0 0.2d0 0.3d0) (-0.5d0 0d0 0.5d0)))

(defparameter *illum-d55-10*
  (make-illuminant :spectrum (gen-illum-d-spectrum 5500 :rectify t)
                   :observer +obs-cie1964+))

(defparameter *illum-b*
  (make-illuminant :x 0.9909274174750896d0 :z 0.8531327231885476d0))

(defparameter *lchuv-set*
  '((20 30 40) (0.5 0.2 240) (99.9 0.1 359.9)))

(define-cat-function lchuv-d50-to-a +illum-d50+ +illum-a+
  :target :lchuv
  :cat +cmccat97+)
(define-cat-function lchuv-a-to-d50 +illum-a+ +illum-d50+
  :target :lchuv
  :cat +cmccat97+)
(define-cat-function xyy-c-to-e +illum-c+ +illum-e+
  :target :xyy)
(define-cat-function xyy-e-to-c +illum-e+ +illum-c+
  :target :xyy)

;; Workaround for ABCL. (alexanrdia:rcurry causes an error on ABCL.)
#+abcl
(defun rcurry (function &rest initial-args)
  (lambda (&rest args)
    (apply function (append args initial-args))))

(defparameter *ciede2000-set-path* (asdf:component-pathname (asdf:find-component "dufy" '("dat" "ciede2000-test-data.csv"))))
(defparameter *ciede2000-set*
  (loop for (row1 row2)
          on (read-csv *ciede2000-set-path*
                       :map-fn #'(lambda (row)
                                   (mapcar (rcurry #'parse-float
                                                   :junk-allowed t
                                                   :type 'double-float)
                                           row)))
        by #'cddr
        collect (nconc (subseq row1 0 3) (subseq row2 0 3) (last row1))))


;;;
;;; Test Code
;;;

(test test-circle-arithmetic
  (is (= 0 (circular-clamp 50 0 0)))
  (is (= -3 (circular-clamp -3 350 10 360)))
  (is (= 350 (circular-clamp -11 350 10 360)))
  (is (= 10 (circular-clamp 30 350 10 360)))
  ;; FIXME: Fails on Clozure CL due to a bug related to an inlined
  ;; function. See
  ;; https://github.com/Clozure/ccl/issues/166. Currently it is not a
  ;; serious problem because CIRCULAR-LERP only takes (implicitly
  ;; coerced) DOUBLE-FLOATs.
  #-ccl (is (= 1 (circular-lerp 1 0.2 1))))

(test test-spectrum
  (is (nearly-equal 1d-4
		    '(0.33411d0 0.34877)
		    (multiple-value-list (illuminant-xy *illum-d55-10*))))
  (is (nearly-equal 1d-3
		    '(0.95047d0 1d0 1.08883d0)
		    (multiple-value-list
		     (spectrum-to-xyz #'flat-spectrum :illuminant +illum-d65+
                                                      :begin-wl 370s0
                                                      :end-wl 825.5
                                                      :band 1/10))))
  (is (nearly-equal 1d-4
		    '(0.33411 0.34877 1.0d0)
		    (multiple-value-list
		     (multiple-value-call #'xyz-to-xyy
		       (spectrum-to-xyz #'flat-spectrum
                                        :illuminant *illum-d55-10*)))))
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (spectrum-to-xyz (approximate-spectrum
					 (apply (rcurry #'xyz-to-spectrum
                                                        :illuminant *illum-d55-10*)
						xyz)
					 :begin-wl 340d0
                                         :end-wl 850d0
                                         :band 0.23d0)
					:illuminant *illum-d55-10*)))))
  (is (equal '(0d0 0d0 0d0)
             (multiple-value-list (funcall (observer-cmf +obs-cie1964+) -1/2))))
  (signals no-spd-error
    (spectrum-to-xyz #'flat-spectrum :illuminant *illum-b*)))

(test test-xyy
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (multiple-value-call #'xyy-to-xyz
			 (apply #'xyz-to-xyy xyz))))))
  (is (equal '(0d0 1d0 0d0)
             (multiple-value-list (xyy-to-xyz 35 0 1))))
  (is (equal '(0d0 0d0 0.8d0)
             (multiple-value-list (xyz-to-xyy 0 0.8d0 -0.8d0)))))

(test test-cat
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (multiple-value-call #'lms-to-xyz
			 (apply (rcurry #'xyz-to-lms
					:illuminant *illum-d55-10*
					:cat +cmccat97+)
				xyz)
			 :illuminant *illum-d55-10*
			 :cat +cmccat97+)))))
  (let ((cat-func (gen-cat-function *illum-d55-10* +illum-a+ :target :xyy))
	(cat-func-rev (gen-cat-function +illum-a+ *illum-d55-10* :target :xyy)))
    (dolist (xyy *xyy-set*)
      (is (nearly-equal 1d-4
			xyy
			(multiple-value-list
			 (multiple-value-call cat-func-rev
			   (apply cat-func xyy)))))))
  (dolist (lchuv *lchuv-set*)
    (is (nearly-equal 1d-4
		      lchuv
		      (multiple-value-list
		       (multiple-value-call #'lchuv-a-to-d50
			 (apply #'lchuv-d50-to-a lchuv))))))
  (dolist (xyy *xyy-set*)
    (is (nearly-equal 1d-4
		      xyy
		      (multiple-value-list
		       (multiple-value-call #'xyy-c-to-e
			 (apply #'xyy-e-to-c xyy)))))))


(test test-rgb
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (multiple-value-call #'rgb-to-xyz
			 (apply (rcurry #'xyz-to-rgb :rgbspace +scrgb-nl+) xyz)
			 :rgbspace +scrgb-nl+)))))
  (dolist (rgbspace (list +bg-srgb-16+ +scrgb-nl+
                          (copy-rgbspace +scrgb-16+
                                         :illuminant +illum-a+
                                         :bit-per-channel 21)))
    (dolist (qrgb *qrgb16-set*)
      (is (equal qrgb
		 (multiple-value-list
		  (multiple-value-call (rcurry #'xyz-to-qrgb :clamp nil)
		    (apply (rcurry #'qrgb-to-xyz :rgbspace rgbspace)
			   qrgb)
		    :rgbspace rgbspace))))
      (is (equal qrgb
		 (multiple-value-list
		  (multiple-value-call (rcurry #'lrgb-to-qrgb :clamp nil)
		    (apply (rcurry #'qrgb-to-lrgb :rgbspace rgbspace)
			   qrgb)
		    :rgbspace rgbspace))))))
  (is (equal '(0 5001 65535)
	     (multiple-value-list
	      (rgbpack-to-qrgb (qrgb-to-rgbpack 0 5001 65535 :rgbspace +bg-srgb-16+)
			   :rgbspace +bg-srgb-16+))))
  (dolist (intrgb '(#x000011112222 #x5678abcdffff))
    (is (= intrgb
           (multiple-value-call #'xyz-to-rgbpack
             (rgbpack-to-xyz intrgb :rgbspace +bg-srgb-16+)
             :rgbspace +bg-srgb-16+)))
    (is (= intrgb
           (multiple-value-call #'rgb-to-rgbpack
             (rgbpack-to-rgb intrgb :rgbspace +bg-srgb-16+)
             :rgbspace +bg-srgb-16+)))
    (is (= intrgb
           (multiple-value-call #'lrgb-to-rgbpack
             (rgbpack-to-lrgb intrgb :rgbspace +bg-srgb-16+)
             :rgbspace +bg-srgb-16+))))
  ;; rgbspace changer
  (dolist (rgb *rgb-set*)
    (is (nearly-equal 1d-4
		      rgb
		      (multiple-value-list
		       (multiple-value-call (gen-rgbspace-changer +scrgb-nl+ +pal/secam+
                                                                  :target :rgb)
			 (apply (gen-rgbspace-changer +pal/secam+ +scrgb-nl+
                                                      :target :rgb)
				rgb)))))))

(test test-lab/luv
  (dolist (xyy *xyy-set*)
    (is (nearly-equal 1d-4
		      xyy
		      (multiple-value-list
		       (multiple-value-call #'dufy/core::lchab-to-xyy
			 (apply (rcurry #'dufy/core::xyy-to-lchab
                                        :illuminant *illum-d55-10*)
				xyy)
			 :illuminant *illum-d55-10*)))))
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (multiple-value-call #'lchuv-to-xyz
			 (apply (rcurry #'xyz-to-lchuv
                                        :illuminant *illum-d55-10*)
				xyz)
			 :illuminant *illum-d55-10*))))))

(test test-hsv/hsl
  (is (nearly-equal 1d-4
                    (multiple-value-list
                     (hsl-to-rgb 1234 0 1.2545778685270217d0))
                    (multiple-value-list
                     (hsv-to-rgb -1234 0 1.2545778685270217d0))
                    '(1.2545778685270217d0 1.2545778685270217d0 1.2545778685270217d0)))
  (loop for xyz-to-foo in '(xyz-to-hsv xyz-to-hsl)
     for foo-to-xyz in '(hsv-to-xyz hsl-to-xyz) do
       (dolist (xyz *xyz-set*)
	 (is (nearly-equal 1d-4
			   xyz
			   (multiple-value-list
			    (multiple-value-call foo-to-xyz
			      (apply (rcurry xyz-to-foo :rgbspace +bg-srgb-16+)
				     xyz)
			      :rgbspace +bg-srgb-16+))))))
  (loop for qrgb-to-foo in '(qrgb-to-hsv qrgb-to-hsl)
     for foo-to-qrgb in '(hsv-to-qrgb hsl-to-qrgb) do
       (dolist (rgbspace (list +bg-srgb-16+ +prophoto-16+))
	 (dolist (qrgb *qrgb16-set*)
	   (is (equal qrgb
		      (multiple-value-list
		       (multiple-value-call (rcurry foo-to-qrgb :clamp nil)
			 (apply (rcurry qrgb-to-foo :rgbspace rgbspace)
				qrgb)
			 :rgbspace rgbspace))))))))

(test test-deltae
  ;; By Bruce Lindbloom's calculator
  (is (nearly= 5d-3 66.228653d0 (qrgb-deltae94 10 20 30 200 100 0 :application :textiles)))
  (is (nearly= 5d-3 91.75d0 (qrgb-deltaeab 10 20 30 200 100 0)))
  (is (nearly= 1d-4 62.131436d0 (lab-deltaecmc 10 20 30 40 50 60 :l-factor 1d0 :c-factor 1d0)))
  (is (nearly= 1d-4 16.257543d0 (lab-deltaecmc 90 80 70 60 50 40)))
  (dolist (row *ciede2000-set*)
    (is (nearly= 1d-4
                 (nth 6 row)
                 (apply #'lab-deltae00
                        (append  (subseq row 0 3)
                                 (subseq row 3 6)))))))

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

