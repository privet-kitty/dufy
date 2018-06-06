;;;
;;; Several color difference functions
;;;

(in-package :dufy-core)

(define-primary-functional (lab-deltaeab lab :term deltaeab) (l1 a1 b1 l2 a2 b2)
  "CIE 1976. Euclidean distance in L*a*b* space."
  (declare (optimize (speed 3) (safety 1))
           (real l1 a1 b1 l2 a2 b2))
  (let ((deltal (- (float l1 1d0) (float l2 1d0)))
	(deltaa (- (float a1 1d0) (float a2 1d0)))
	(deltab (- (float b1 1d0) (float b2 1d0))))
    (sqrt (+ (* deltal deltal)
	     (* deltaa deltaa)
	     (* deltab deltab)))))

(define-secondary-functional xyz-deltaeab deltaeab xyz)
(define-secondary-functional qrgb-deltaeab deltaeab qrgb)



(define-primary-functional (lab-deltae94 lab :term deltae94) (l1 a1 b1 l2 a2 b2 &key (application :graphic-arts))
  "CIE 1994. 
APPLICATION::= :graphic-arts | :textiles"
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (l1 a1 b1 l2 a2 b2)
    (let ((c1 (sqrt (+ (* a1 a1) (* b1 b1))))
	  (c2 (sqrt (+ (* a2 a2) (* b2 b2)))))
      (let* ((delta-l (- l1 l2))
	     (delta-c (- c1 c2))
	     (delta-a (- a1 a2))
	     (delta-b (- b1 b2))
	     (delta-h (sqrt (the (double-float 0d0)
				 (+ (* delta-a delta-a)
				    (* delta-b delta-b)
				    (- (* delta-c delta-c)))))))
	(multiple-value-bind (kL k1 k2)
	    (ecase application
	      (:graphic-arts (values 1d0 0.045d0 0.015d0))
	      (:textiles (values 2d0 0.048d0 0.014d0)))
	  (let ((sc (+ 1d0 (* k1 c1)))
		(sh (+ 1d0 (* k2 c1))))
	    (let ((diff-l (/ delta-l kL))
		  (diff-c (/ delta-c sc))
		  (diff-h (/ delta-h sh)))
	      (sqrt (+ (* diff-l diff-l)
		       (* diff-c diff-c)
		       (* diff-h diff-h))))))))))

(define-secondary-functional xyz-deltae94 deltae94 xyz)
(define-secondary-functional qrgb-deltae94 deltae94 qrgb)



(define-primary-functional (lab-deltae00 lab :term deltae00) (l1 a1 b1 l2 a2 b2)
  "CIEDE2000."
  (declare (optimize (speed 3) (safety 1)))
  (with-double-float (l1 a1 b1 l2 a2 b2)
    (let* ((c1 (sqrt (+ (* a1 a1) (* b1 b1))))
           (c2 (sqrt (+ (* a2 a2) (* b2 b2))))
           (deltaLprime (- l2 l1))
           (Lmean (* 0.5d0 (+ l1 l2)))
           (Cmean (* 0.5d0 (+ c1 c2)))
           (Cmean7 (the (double-float 0d0) (expt Cmean 7)))
           (const1 (* 0.5d0 (- 1d0 (sqrt (/ Cmean7 (+ Cmean7 #.(expt 25 7)))))))
           (aprime1 (+ a1 (* a1 const1)))
           (aprime2 (+ a2 (* a2 const1)))
           (Cprime1 (sqrt (+ (* aprime1 aprime1) (* b1 b1))))
           (Cprime2 (sqrt (+ (* aprime2 aprime2) (* b2 b2))))
           (Cmeanprime (* 0.5d0 (+ Cprime1 Cprime2)))
           (deltaCprime (- Cprime2 Cprime1))
           (hprime1 (if (and (= b1 0) (= aprime1 0))
                        0d0
                        (mod (* (atan b1 aprime1) #.(/ 180d0 PI)) 360d0)))
           (hprime2 (if (and (= b2 0) (= aprime2 0))
                        0d0
                        (mod (* (atan b2 aprime2) #.(/ 180d0 PI)) 360d0)))
           (deltahprime (cond ((or (= Cprime1 0d0) (= Cprime2 0d0))
                               0d0)
                              ((<= (abs (- hprime1 hprime2)) 180d0)
                               (- hprime2 hprime1))
                              ((<= hprime2 hprime1)
                               (+ (- hprime2 hprime1) 360d0))
                              (t
                               (- hprime2 hprime1 360))))
           (deltalargeHprime (* 2d0
                                (sqrt (* Cprime1 Cprime2))
                                (sin (* deltahprime #.(* 0.5d0 PI 1/180)))))
           (Hmeanprime (cond ((or (= Cprime1 0) (= Cprime2 0))
                              (+ hprime1 hprime2))
                             ((<= (abs (- hprime1 hprime2)) 180d0)
                              (* 0.5d0 (+ hprime1 hprime2)))
                             ((< (+ hprime1 hprime2) 360d0)
                              (* 0.5d0 (+ hprime1 hprime2 360d0)))
                             (t
                              (* 0.5d0 (+ hprime1 hprime2 -360d0)))))
           (varT (+ 1d0
                    (* -0.17d0 (cos (* (- Hmeanprime 30) #.(/ PI 180d0))))
                    (* 0.24d0 (cos (* 2d0 Hmeanprime #.(/ PI 180d0))))
                    (* 0.32d0 (cos (* (+ (* 3d0 Hmeanprime) 6d0) #.(/ PI 180))))
                    (* -0.20d0 (cos (* (- (* 4d0 Hmeanprime) 63d0) #.(/ PI 180))))))
           (Lmean-50 (- Lmean 50d0))
           (tmp (* Lmean-50 Lmean-50))
           (varSL (+ 1d0 (/ (* 0.015d0 tmp)
                            (sqrt (+ 20d0 tmp)))))
           (varSC (+ 1d0 (* 0.045d0 Cmeanprime)))
           (varSH (+ 1d0 (* 0.015d0 Cmeanprime varT)))
           (Cmeanprime7 (the (double-float 0d0) (expt Cmeanprime 7)))
           (varRT  (* -2d0
                      (sqrt (/ Cmeanprime7 (+ Cmeanprime7 #.(expt 25 7))))
                      (sin (* 60d0
                              (exp (- (expt (* (- Hmeanprime 275d0) 1/25) 2)))
                              #.(/ PI 180)))))
           (diff-l (/ deltaLprime varSL))
           (diff-c (/ deltaCprime varSC))
           (diff-h (/ deltalargeHprime varSH)))
      (sqrt (the (double-float 0d0)
                 (+ (* diff-l diff-l)
                    (* diff-c diff-c)
                    (* diff-h diff-h)
                    (* varRT (/ deltaCprime varSC) (/ deltalargeHprime varSH))))))))

(define-secondary-functional xyz-deltae00 deltae00 xyz)
(define-secondary-functional qrgb-deltae00 deltae00 qrgb)

(defun bench-deltae00 (&optional (num 1000000))
  (declare (optimize (speed 3) (safety 1)))
  (time-after-gc
    (dotimes (x num)
      (qrgb-deltae00 (random 65536) (random 65536) (random 65536)
                     (random 65536) (random 65536) (random 65536)
                     :rgbspace +bg-srgb-16+))))



(define-primary-functional (lab-deltaecmc lab :term deltaecmc) (l1 a1 b1 l2 a2 b2 &key (l-factor 2d0) (c-factor 1d0))
  (declare (optimize (speed 3) (safety 1)))
  "CMC l:c"
  (with-double-float (l1 a1 b1 l2 a2 b2 l-factor c-factor)
    (let* ((deltaa (- a1 a2))
           (deltab (- b1 b2))
           (deltal (- l1 l2))
           (c1-2 (+ (* a1 a1) (* b1 b1)))
           (c1 (sqrt c1-2))
           (c1-4 (* c1-2 c1-2))
           (c2 (sqrt (+ (* a2 a2) (* b2 b2))))
           (deltac (- c1 c2))
           (deltah-2 (+ (* deltaa deltaa) (* deltab deltab) (- (* deltac deltac))))
           (h1 (mod (atan b1 a1) TWO-PI))
           (f (sqrt (/ c1-4 (+ 1900d0 c1-4))))
           (tt (if (or (< h1 #.(* 164d0 +TWO-PI/360+))
                       (< #.(* 345d0 +TWO-PI/360+) h1))
                   (+ 0.36d0 (abs (* 0.4d0 (cos (+ h1 #.(* 35d0 +TWO-PI/360+))))))
                   (+ 0.56d0 (abs (* 0.2d0 (cos (+ h1 #.(* 168d0 +TWO-PI/360+))))))))
           (sl (if (< l1 16d0)
                   0.511d0
                   (/ (* 0.040975d0 l1) (1+ (* 0.01765d0 l1)))))
           (sc (+ 0.638d0 (/ (* 0.0638d0 c1) (1+ (* 0.0131d0 c1)))))
           (sh (* sc (+ 1d0 (* f tt) (- f))))
           (diff-l (/ deltal (* l-factor sl)))
           (diff-c (/ deltac (* c-factor sc)))
           (diff-h-2 (the (double-float 0d0) (/ deltah-2 (* sh sh)))))
      (sqrt (+ (* diff-l diff-l)
               (* diff-c diff-c)
               diff-h-2)))))

(define-secondary-functional xyz-deltaecmc deltaecmc xyz)
(define-secondary-functional qrgb-deltaecmc deltaecmc qrgb)

