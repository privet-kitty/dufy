;;;
;;; HSV/HSL
;;;

(in-package :dufy/core)

(define-colorspace hsv (hue sat val)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "HUE is in the circle group R/360Z. The nominal range of SAT and VAL is [0, 1].")
(define-colorspace hsl (hue sat lum)
  :arg-types (real real real)
  :return-types (double-float double-float double-float)
  :documentation "HUE is in the circle group R/360Z. The nominal range of SAT and LUM is [0, 1].")

(define-primary-converter (hsv rgb) (hue sat val)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the situation
whether the returned values are meaningful."
  (let ((hue (the (double-float 0d0 360d0) (mod (float hue 1d0) 360d0)))
        (sat (float sat 1d0))
        (val (float val 1d0)))
    (let* ((c (* val sat))
           (h-prime (* hue #.(float 1/60 1d0)))
           (h-prime-int (floor h-prime))
           (x (* c (- 1d0 (abs (- (mod h-prime 2d0) 1d0)))))
           (base (- val c)))
      (cond ((= sat 0d0) (values base base base))
            ((= 0 h-prime-int) (values val (+ base x) base))
            ((= 1 h-prime-int) (values (+ base x) val base))
            ((= 2 h-prime-int) (values base val (+ base x)))
            ((= 3 h-prime-int) (values base (+ base x) val))
            ((= 4 h-prime-int) (values (+ base x) base val))
            ((= 5 h-prime-int) (values val base (+ base x)))
            (t (error "Reached unreachable clause"))))))

(defconverters hsv (rgbpack qrgb lrgb xyz))

(define-primary-converter (rgb hsv) (r g b)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the
situation whether the returned values are meaningful."
  (with-ensuring-type double-float (r g b)
    (let* ((maxrgb (max r g b))
           (minrgb (min r g b))
           (s (if (= maxrgb 0d0)
                  0d0
                  (/ (- maxrgb minrgb) maxrgb)))
           (h (cond ((= minrgb maxrgb) 0d0)
                    ((= minrgb b) (+ (* 60d0 (/ (- g r) (- maxrgb minrgb))) 60d0))
                    ((= minrgb r) (+ (* 60d0 (/ (- b g) (- maxrgb minrgb))) 180d0))
                    ((= minrgb g) (+ (* 60d0 (/ (- r b) (- maxrgb minrgb))) 300d0)))))
      (values h s maxrgb))))

(defconverters (qrgb rgbpack lrgb xyz) hsv)

(define-primary-converter (hsl rgb) (hue sat lum)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the
situation whether the returned values are meaningful."
  (with-ensuring-type double-float (hue sat lum)
    (let* ((hue (mod hue 360d0))
           (tmp (* 0.5d0 sat (- 1d0 (abs (+ lum lum -1d0)))))
           (max (+ lum tmp))
           (min (- lum tmp))
           (delta (- max min))
           (h-prime (floor (the (double-float 0d0 6d0) (* hue 1/60)))))
      (cond ((= sat 0d0) (values max max max))
            ((= 0 h-prime) (values max
                                   (+ min (* delta hue 1/60))
                                   min))
            ((= 1 h-prime) (values (+ min (* delta (- 120d0 hue) 1/60))
                                   max
                                   min))
            ((= 2 h-prime) (values min
                                   max
                                   (+ min (* delta (- hue 120d0) 1/60))))
            ((= 3 h-prime) (values min
                                   (+ min (* delta (- 240d0 hue) 1/60))
                                   max))
            ((= 4 h-prime) (values (+ min (* delta (- hue 240d0) 1/60))
                                   min
                                   max))
            ((= 5 h-prime) (values max
                                   min
                                   (+ min (* delta (- 360d0 hue) 1/60))))
            (t (error "Reached unreachable clause."))))))

(defconverters hsl (rgbpack qrgb lrgb xyz))

(define-primary-converter (rgb hsl) (r g b)
  (declare (optimize (speed 3) (safety 1)))
  "Non-normal RGB space is also accepted, though it depends on the
situation whether the returned values are meaningful."
  (with-ensuring-type double-float (r g b)
    (let ((minrgb (min r g b))
          (maxrgb (max r g b)))
      (values (cond ((= minrgb maxrgb) 0d0)
                    ((= minrgb b) (+ 60d0 (* 60d0 (/ (- g r) (- maxrgb minrgb)))))
                    ((= minrgb r) (+ 180d0 (* 60d0 (/ (- b g) (- maxrgb minrgb)))))
                    ((= minrgb g) (+ 300d0 (* 60d0 (/ (- r b) (- maxrgb minrgb))))))
              (let ((denom (- 1d0 (abs (+ maxrgb minrgb -1d0)))))
                (if (zerop denom)
                    0d0
                    (/ (- maxrgb minrgb) denom)))
              (* 0.5d0 (+ maxrgb minrgb))))))

(defconverters (qrgb rgbpack lrgb xyz) hsl)
