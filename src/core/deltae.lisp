;; -*- coding: utf-8 -*-
;;;
;;; Several color difference functions
;;;

(in-package :dufy/core)

;; Export the function names of delta-E converters automatically
(dolist (colorspace '(lab qrgb xyz))
  (dolist (name '(#:deltaeab #:deltae94 #:deltae00 #:deltaecmc))
    (export (intern (format nil "~:@(~A-~A~)" colorspace name)))))

(define-functional (lab-deltaeab lab :term deltaeab) (l1 a1 b1 l2 a2 b2)
  (declare (optimize (speed 3) (safety 1)))
  "CIE 1976. Euclidean distance in L*a*b* space."
  (with-ensuring-type double-float (l1 a1 b1 l2 a2 b2)
    (sqrt (+ (square (- l1 l2))
             (square (- a1 a2))
             (square (- b1 b2))))))

(extend-functional deltaeab xyz)
(extend-functional deltaeab qrgb)


(define-functional (lab-deltae94 lab :term deltae94) (l1 a1 b1 l2 a2 b2 &key (application :graphic-arts))
  (declare (optimize (speed 3) (safety 1)))
  "CIE 1994. 
APPLICATION ::= :graphic-arts | :textiles"
  (with-ensuring-type double-float (l1 a1 b1 l2 a2 b2)
    (let ((c1 (sqrt (+ (square a1) (square b1))))
          (c2 (sqrt (+ (square a2) (square b2)))))
      (let* ((delta-l (- l1 l2))
             (delta-c (- c1 c2))
             (delta-a (- a1 a2))
             (delta-b (- b1 b2))
             (delta-h (sqrt (the (double-float 0d0)
                                 (+ (square delta-a)
                                    (square delta-b)
                                    (- (square delta-c)))))))
        (multiple-value-bind (kL k1 k2)
            (ecase application
              (:graphic-arts (values 1d0 0.045d0 0.015d0))
              (:textiles (values 2d0 0.048d0 0.014d0)))
          (let ((sc (+ 1d0 (* k1 c1)))
                (sh (+ 1d0 (* k2 c1))))
            (sqrt (+ (square (/ delta-l kL))
                     (square (/ delta-c sc))
                     (square (/ delta-h sh))))))))))

(extend-functional deltae94 xyz)
(extend-functional deltae94 qrgb)


(define-functional (lab-deltae00 lab :term deltae00) (l1 a1 b1 l2 a2 b2)
  (declare (optimize (speed 3) (safety 1)))
  "CIEDE2000. 

Tested with the test set by Sharma-Wu-Dalal. See \"The CIEDE2000
Color-Difference Formula: Implementation Notes, Supplementary Test Data, and
Mathematical Observations\", 2004."
  (with-ensuring-type double-float (l1 a1 b1 l2 a2 b2)
    (let* ((C1 (sqrt (+ (square a1) (square b1))))
           (C2 (sqrt (+ (square a2) (square b2))))
           (deltaLprime (- l2 l1))
           (Lmean (* 0.5d0 (+ l1 l2)))
           (Cmean (* 0.5d0 (+ C1 C2)))
           (Cmean7 (pow Cmean 7))
           (G (* 0.5d0 (- 1d0 (sqrt (/ Cmean7 (+ Cmean7 #.(expt 25 7)))))))
           (aprime1 (+ a1 (* a1 G)))
           (aprime2 (+ a2 (* a2 G)))
           (Cprime1 (sqrt (+ (square aprime1) (square b1))))
           (Cprime2 (sqrt (+ (square aprime2) (square b2))))
           (Cmeanprime (* 0.5d0 (+ Cprime1 Cprime2)))
           (Cmeanprime7 (pow Cmeanprime 7))
           (deltaCprime (- Cprime2 Cprime1))
           (hprime1 (mod (atan b1 aprime1) TWO-PI))
           (hprime2 (mod (atan b2 aprime2) TWO-PI))
           (deltahprime (- hprime2 hprime1)) ; mut.
           (capHmeanprime 0d0)) ; mut.
      (cond ((or (zerop Cprime1) (zerop Cprime2))
             (setf deltahprime 0d0)
             (setf capHmeanprime (+ hprime1 hprime2)))
            ((<= (abs deltahprime) PI)
             (setf capHmeanprime (* (+ hprime1 hprime2) 0.5d0)))
            ((<= hprime2 hprime1)
             (incf deltahprime TWO-PI)
             (if (< (+ hprime1 hprime2) TWO-PI)
                 (setf capHmeanprime (* 0.5d0 (+ hprime1 hprime2 TWO-PI)))
                 (setf capHmeanprime (* 0.5d0 (+ hprime1 hprime2 (- TWO-PI))))))
            (t
             (decf deltahprime TWO-PI)
             (if (< (+ hprime1 hprime2) TWO-PI)
                 (setf capHmeanprime (* 0.5d0 (+ hprime1 hprime2 TWO-PI)))
                 (setf capHmeanprime (* 0.5d0 (+ hprime1 hprime2 (- TWO-PI)))))))
      (let* ((deltacapHprime (* 2d0
                                (sqrt (* Cprime1 Cprime2))
                                (sin (* deltahprime 0.5d0))))
             (varT (+ 1d0
                      (* -0.17d0 (cos (- capHmeanprime #.(degree-to-radian 30d0))))
                      (* 0.24d0 (cos (* 2d0 capHmeanprime)))
                      (* 0.32d0 (cos (+ (* 3d0 capHmeanprime) #.(degree-to-radian 6d0))))
                      (* -0.20d0 (cos (- (* 4d0 capHmeanprime) #.(degree-to-radian 63d0))))))
             (Lmean-offsetted-squared (square (- Lmean 50d0)))
             (varSL (+ 1d0 (/ (* 0.015d0 Lmean-offsetted-squared)
                              (sqrt (+ 20d0 Lmean-offsetted-squared)))))
             (varSC (+ 1d0 (* 0.045d0 Cmeanprime)))
             (varSH (+ 1d0 (* 0.015d0 Cmeanprime varT)))
             (varRT  (* -2d0
                        (sqrt (/ Cmeanprime7 (+ Cmeanprime7 #.(expt 25 7))))
                        (sin (* #.(* 60d0 +TWO-PI/360+)
                                (exp (- (square (* (- capHmeanprime #.(degree-to-radian 275d0))
                                                   #.(/ (degree-to-radian 25d0))))))))))
             (factor-l (/ deltaLprime varSL))
             (factor-c (/ deltaCprime varSC))
             (factor-h (/ deltacapHprime varSH)))
        (sqrt (the (double-float 0d0)
                   (+ (square factor-l)
                      (square factor-c)
                      (square factor-h)
                      (* varRT factor-c factor-h))))))))

(extend-functional deltae00 xyz)
(extend-functional deltae00 qrgb)


(define-functional (lab-deltaecmc lab :term deltaecmc) (l1 a1 b1 l2 a2 b2 &key (l-factor 2d0) (c-factor 1d0))
  (declare (optimize (speed 3) (safety 1)))
  "CMC l:c"
  (with-ensuring-type double-float (l1 a1 b1 l2 a2 b2 l-factor c-factor)
    (let* ((deltaa (- a1 a2))
           (deltab (- b1 b2))
           (deltal (- l1 l2))
           (c1-squared (+ (square a1) (square b1)))
           (c1 (sqrt c1-squared))
           (c1-4 (square c1-squared))
           (c2 (sqrt (+ (* a2 a2) (* b2 b2))))
           (deltac (- c1 c2))
           (deltah-2 (the (double-float 0d0)
                          (+ (square deltaa) (square deltab) (- (square deltac)))))
           (h1 (mod (atan b1 a1) TWO-PI))
           (f (sqrt (/ c1-4 (+ 1900d0 c1-4))))
           (tt (if (or (< h1 #.(degree-to-radian 164d0))
                       (< #.(degree-to-radian 345d0) h1))
                   (+ 0.36d0 (abs (* 0.4d0 (cos (+ h1 #.(degree-to-radian 35d0))))))
                   (+ 0.56d0 (abs (* 0.2d0 (cos (+ h1 #.(degree-to-radian 168d0))))))))
           (sl (if (< l1 16d0)
                   0.511d0
                   (/ (* 0.040975d0 l1) (1+ (* 0.01765d0 l1)))))
           (sc (+ 0.638d0 (/ (* 0.0638d0 c1) (1+ (* 0.0131d0 c1)))))
           (sh (* sc (+ 1d0 (* f tt) (- f))))
           (factor-l (/ deltal (* l-factor sl)))
           (factor-c (/ deltac (* c-factor sc)))
           (factor-h-2 (/ deltah-2 (* sh sh))))
      (sqrt (+ (* factor-l factor-l)
               (* factor-c factor-c)
               factor-h-2)))))

(extend-functional deltaecmc xyz)
(extend-functional deltaecmc qrgb)
