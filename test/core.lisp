;; -*- coding: utf-8 -*-
(in-package :dufy/test)

(in-suite main-suite)

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

;; Workaround for ABCL. (alexanrdia:rcurry causes an error on ABCL.)
#+abcl
(defun rcurry (function &rest initial-args)
  (lambda (&rest args)
    (apply function (append args initial-args))))

(defparameter *ciede2000-set-path* (asdf:component-pathname (asdf:find-component "dufy" '("dat" "ciede2000-test-data.csv"))))

;; Extract two sets of L*a*b* values and ΔE_00 from the CSV.
(defparameter *ciede2000-set*
  (loop for (row1 row2)
          on (read-csv *ciede2000-set-path*
                       :map-fn #'(lambda (row)
                                   (mapcar (rcurry #'parse-float
                                                   :junk-allowed t
                                                   :type 'double-float)
                                           row)))
        by #'cddr
        collect (append (subseq row1 0 3) (subseq row2 0 3) (last row1))))


;;;
;;; Test Code
;;;

(test circle-arithmetic
  (is (= 0 (circular-clamp 50 0 0)))
  (is (= -3 (circular-clamp -3 350 10 360)))
  (is (= 350 (circular-clamp -11 350 10 360)))
  (is (= 10 (circular-clamp 30 350 10 360)))
  ;; FIXME: Fails on Clozure CL due to a bug related to an inlined
  ;; function. See
  ;; https://github.com/Clozure/ccl/issues/166. Currently it is not a
  ;; serious problem because CIRCULAR-LERP only receives (explicitly
  ;; coerced) DOUBLE-FLOATs.
  #-ccl (is (= 1 (circular-lerp 1 0.2 1))))

(test spectrum
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

(test xyy
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

(test lms
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
		         :cat +cmccat97+))))))

(test make-cat
  (is (equalp +identity-matrix+ (cat-inv-matrix +xyz-scaling+)))
  (is (equalp +xyz-scaling+ (make-cat #((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 1d0)))))
  (is (equalp +xyz-scaling+ (make-cat '((1d0 0d0 0d0) (0d0 1d0 0d0) (0d0 0d0 1d0)))))
  (is (equalp +xyz-scaling+ (make-cat #2a((1f0 0e0 0l0) (0s0 1 0) (0 0/1 1l0)))))
  (signals type-error (make-cat 0d0)))

(define-cat-function d50-to-a +illum-d50+ +illum-a+ :cat +cmccat97+)
(define-cat-function a-to-d50 +illum-a+ +illum-d50+ :cat +cmccat97+)

(test cat
  (let ((cat-func (gen-cat-function *illum-d55-10* +illum-a+))
        (cat-func-rev (gen-cat-function +illum-a+ *illum-d55-10*)))
    (dolist (xyz *xyz-set*)
      (is (nearly-equal 1d-4
		        xyz
		        (multiple-value-list
		         (multiple-value-call cat-func-rev
			   (apply cat-func xyz)))))))
  (dolist (xyz *xyz-set*)
    (is (nearly-equal 1d-4
		      xyz
		      (multiple-value-list
		       (multiple-value-call #'a-to-d50
		         (apply #'d50-to-a xyz)))))))

(test rgb
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
             :rgbspace +bg-srgb-16+)))))

(test gen-rgbspace-changer
  (dolist (rgb *rgb-set*)
    (is (nearly-equal 1d-4
		      rgb
		      (multiple-value-list
		       (multiple-value-call (gen-rgbspace-changer +scrgb-nl+ +pal/secam+
                                                                  :target :rgb)
			 (apply (gen-rgbspace-changer +pal/secam+ +scrgb-nl+
                                                      :target :rgb)
				rgb)))))))

(defconverter lchab xyy)
(defconverter xyy lchab)
(test lab/luv
  (dolist (xyy *xyy-set*)
    (is (nearly-equal 1d-4
		      xyy
		      (multiple-value-list
		       (multiple-value-call #'lchab-to-xyy
			 (apply (rcurry #'xyy-to-lchab :illuminant *illum-d55-10*)
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

(test hsv/hsl
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

(test deltae
  ;; From Bruce Lindbloom's calculator
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

(test line-distance
  (is (nearly-equal 1d-4
                    (list (mb-line-distance-from-origin (make-mb-line :slope 0.0d0 :intercept 1.0d0)))
                    (list 1.0)))
  (is (nearly-equal 1d-4
                    (list (mb-line-distance-from-origin (make-mb-line :slope 1.0d0 :intercept 1.0d0)))
                    (list (sqrt 0.5))))
  (is (nearly-equal 1d-4
                    (list (mb-line-distance-from-origin (make-mb-line :slope -1.0d0 :intercept 1.0d0)))
                    (list (sqrt 0.5)))))

(defstruct hsluv-test-case
  hsluv lchuv hpluv)

;;; test cases taken from HSLuv reference implementation snapshot V4
(defparameter *hsluv-cases*
  (list (make-hsluv-test-case
         :lchuv (list 77.2032167276219d0 117.94096456620099d0 127.71501294924047d0)
         :hsluv (list 127.71501294924047d0 98.70483751132377d0 77.2032167276219d0)
         :hpluv (list 127.71501294924047d0 208.9730008442179d0 77.2032167276219d0))
        (make-hsluv-test-case
         :lchuv (list 77.24812495150218d0 116.21996857450615d0 128.24071522256395d0)
         :hsluv (list 128.24071522256395d0 98.71146173323343d0 77.24812495150218d0)
         :hpluv (list 128.24071522256395d0 206.41396744743724d0 77.24812495150218d0))
        (make-hsluv-test-case
         :lchuv (list 77.32197160747141d0 113.4430274506468d0 129.12990557508522d0)
         :hsluv (list 129.12990557508522d0 98.72222202456504d0 77.32197160747141d0)
         :hpluv (list 129.12990557508522d0 202.2730815078432d0 77.32197160747141d0))
        (make-hsluv-test-case
         :lchuv (list 78.23214368803829d0 84.74620740212417d0 143.08347255529662d0)
         :hsluv (list 143.08347255529662d0 98.84260711485369d0 78.23214368803829d0)
         :hpluv (list 143.08347255529662d0 158.72132442321617d0 78.23214368803829d0))
        (make-hsluv-test-case
         :lchuv (list 74.6113685470067d0 58.3540675645504d0 192.1770506300611d0)
         :hsluv (list 192.1770506300611d0 98.85580172073762d0 74.6113685470067d0)
         :hpluv (list 192.1770506300611d0 99.24427292038125d0 74.6113685470067d0))
        (make-hsluv-test-case
         :lchuv (list 76.38871441689477d0 77.32286491015678d0 226.66675785151392d0)
         :hsluv (list 226.66675785151392d0 99.99999999999692d0 76.38871441689477d0)
         :hpluv (list 226.66675785151392d0 131.30464263084565d0 76.38871441689477d0))
        (make-hsluv-test-case
         :lchuv (list 79.72126370567014d0 62.7929370759791d0 179.20908792727695d0)
         :hsluv (list 179.20908792727695d0 99.0006225499112d0 79.72126370567014d0)
         :hpluv (list 179.20908792727695d0 127.93520146708772d0 79.72126370567014d0))
        (make-hsluv-test-case
         :lchuv (list 81.31855622903718d0 72.41200951862642d0 215.7863164786126d0)
         :hsluv (list 215.7863164786126d0 99.99999999999599d0 81.31855622903718d0)
         :hpluv (list 215.7863164786126d0 162.4288072777225d0 81.31855622903718d0))
        (make-hsluv-test-case
         :lchuv (list 78.5371801027425d0 77.54917814941687d0 149.21222230972467d0)
         :hsluv (list 149.21222230972467d0 98.87847942467567d0 78.5371801027425d0)
         :hpluv (list 149.21222230972467d0 147.71057434042635d0 78.5371801027425d0))
        (make-hsluv-test-case
         :lchuv (list 77.75027951054962d0 98.64527130389259d0 134.95379759235584d0)
         :hsluv (list 134.95379759235584d0 98.78155463182881d0 77.75027951054962d0)
         :hpluv (list 134.95379759235584d0 179.96882228706605d0 77.75027951054962d0))
        (make-hsluv-test-case
         :lchuv (list 77.4283833065767d0 109.5576380871678d0 130.4674793571826d0)
         :hsluv (list 130.4674793571826d0 98.73744412848694d0 77.4283833065767d0)
         :hpluv (list 130.4674793571826d0 196.4551111055974d0 77.4283833065767d0))
        (make-hsluv-test-case
         :lchuv (list 77.97031134139297d0 91.92726261128094d0 138.44093756400994d0)
         :hsluv (list 138.44093756400994d0 98.8101351189296d0 77.97031134139297d0)
         :hpluv (list 138.44093756400994d0 169.72389392093928d0 77.97031134139297d0))
        (make-hsluv-test-case
         :lchuv (list 75.74614504038564d0 68.39002293254342d0 217.35669023777243d0)
         :hsluv (list 217.35669023777243d0 98.96898585581087d0 75.74614504038564d0)
         :hpluv (list 217.35669023777243d0 114.57016558150043d0 75.74614504038564d0))
        (make-hsluv-test-case
         :lchuv (list 78.88653107683908d0 70.94957502627213d0 157.18712158734837d0)
         :hsluv (list 157.18712158734837d0 98.91718289161727d0 78.88653107683908d0)
         :hpluv (list 157.18712158734837d0 137.80723273971586d0 78.88653107683908d0))
        (make-hsluv-test-case
         :lchuv (list 77.17897292086379d0 118.88019105271435d0 127.43582058867105d0)
         :hsluv (list 127.43582058867105d0 100.0000000000023d0 77.17897292086379d0)
         :hpluv (list 127.43582058867105d0 210.3672401370558d0 77.17897292086379d0))
        (make-hsluv-test-case
         :lchuv (list 75.15356872935901d0 61.80873654660663d0 205.7022764106217d0)
         :hsluv (list 205.7022764106217d0 98.91247496876854d0 75.15356872935901d0)
         :hpluv (list 205.7022764106217d0 104.36132500203259d0 75.15356872935901d0))
        (make-hsluv-test-case
         :lchuv (list 79.28103287595944d0 65.73632150259745d0 167.24786192995697d0)
         :hsluv (list 167.24786192995697d0 98.95810499467045d0 79.28103287595944d0)
         :hpluv (list 167.24786192995697d0 130.57254218104305d0 79.28103287595944d0))
        (make-hsluv-test-case
         :lchuv (list 80.74002497469166d0 66.16295143342687d0 204.75729527822082d0)
         :hsluv (list 204.75729527822082d0 99.08806330147246d0 80.74002497469166d0)
         :hpluv (list 204.75729527822082d0 143.22067561241397d0 80.74002497469166d0))
        (make-hsluv-test-case
         :lchuv (list 77.57032746304822d0 104.58906633595367d0 132.3609308411799d0)
         :hsluv (list 132.3609308411799d0 98.75724513588756d0 77.57032746304822d0)
         :hpluv (list 132.3609308411799d0 188.97465719842904d0 77.57032746304822d0))
        (make-hsluv-test-case
         :lchuv (list 80.20755874836644d0 62.85039418089378d0 192.17705063006113d0)
         :hsluv (list 192.17705063006113d0 99.04413022019311d0 80.20755874836644d0)
         :hpluv (list 192.17705063006113d0 131.7670090827411d0 80.20755874836644d0))))

(test hsluv/lchuv
  (dolist (case *hsluv-cases*)
    (is (nearly-equal 1d-5
                      (hsluv-test-case-lchuv case)
                      (multiple-value-list (apply #'dufy/core:hsluv-to-lchuv (hsluv-test-case-hsluv case)))))))

(test hpluv/lchuv
  (dolist (case *hsluv-cases*)
    (is (nearly-equal 1d-5
                      (hsluv-test-case-lchuv case)
                      (multiple-value-list (apply #'dufy/core:hpluv-to-lchuv (hsluv-test-case-hpluv case)))))))

(test lchuv/hsluv
  (dolist (case *hsluv-cases*)
    (is (nearly-equal 1d-5
                      (hsluv-test-case-hsluv case)
                      (multiple-value-list (apply #'dufy/core:lchuv-to-hsluv (hsluv-test-case-lchuv case)))))))

(test lchuv/hpluv
  (dolist (case *hsluv-cases*)
    (is (nearly-equal 1d-5
                      (hsluv-test-case-hpluv case)
                      (multiple-value-list (apply #'dufy/core:lchuv-to-hpluv (hsluv-test-case-lchuv case)))))))
