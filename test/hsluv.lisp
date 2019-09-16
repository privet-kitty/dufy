;;;
;;; Test for dufy/hsluv
;;;

(in-package :dufy/test)

(def-suite hsluv-suite :in main-suite)
(in-suite hsluv-suite)

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
                      (multiple-value-list (apply #'hsluv-to-lchuv (hsluv-test-case-hsluv case)))))))

(test hpluv/lchuv
  (dolist (case *hsluv-cases*)
    (is (nearly-equal 1d-5
                      (hsluv-test-case-lchuv case)
                      (multiple-value-list (apply #'hpluv-to-lchuv (hsluv-test-case-hpluv case)))))))

(test lchuv/hsluv
  (dolist (case *hsluv-cases*)
    (is (nearly-equal 1d-5
                      (hsluv-test-case-hsluv case)
                      (multiple-value-list (apply #'lchuv-to-hsluv (hsluv-test-case-lchuv case)))))))

(test lchuv/hpluv
  (dolist (case *hsluv-cases*)
    (is (nearly-equal 1d-5
                      (hsluv-test-case-hpluv case)
                      (multiple-value-list (apply #'lchuv-to-hpluv (hsluv-test-case-lchuv case)))))))
