(uiop:define-package #:dufy/examples/visualize-munsell
  (:use #:cl #:alexandria #:dufy)
  (:import-from #:lparallel)
  (:import-from #:lispbuilder-sdl)
  (:export #:draw-srgb-in-munsell))

(in-package :dufy/examples/visualize-munsell)

(deftype uint nil '(integer 0 #.(expt 10 9)))
(deftype sint nil '(integer #.(- (expt 10 9)) #.(expt 10 9)))

(setf lparallel:*kernel* (lparallel:make-kernel 4))

(declaim (inline mhvc-to-qrgb)
         (ftype (function * (values fixnum fixnum fixnum &optional)) mhvc-to-qrgb))
(defun mhvc-to-qrgb (hue40 value chroma &key (rgbspace +srgb+) (clamp t))
  "Illuminant D65.
The illuminant of RGBSPACE must also be D65."
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-call #'xyz-to-qrgb
    (mhvc-to-xyz hue40 value chroma)
    :rgbspace rgbspace
    :clamp clamp))

(defun draw-srgb-in-munsell (&optional (size 300) (framerate 10) (bg-color sdl:*black*))
  "Graphical demonstration with SDL. Renders the sRGB space in the
Munsell space."
  (declare (optimize (speed 3) (safety 0))
           (type uint size framerate))
  (let* ((value100 0) ; mut.
         (radius (round (/ size 2)))
         (center-x radius)
         (center-y radius)
         (max-chroma 30d0)
         (line-col (sdl:color :r 255 :g 255 :b 255 :a 128)))
    (labels ((polar (i j)
               (declare (type sint i j))
               (let ((delta-x (- j center-x))
                     (delta-y (- i center-y)))
                 (declare (type sint delta-x delta-y))
                 (values (sqrt (+ (* delta-y delta-y)
                                  (* delta-x delta-x)))
                         (atan delta-y delta-x))))
             (coord-to-mhvc (i j)
               (multiple-value-bind (r theta) (polar i j)
                 (values (- 20 (* theta #.(/ 40 dufy/internal:two-pi)))
                         (* value100 0.1d0)
                         (* max-chroma (/ r radius))))))
      (declare (inline coord-to-mhvc polar)
               (uint value100 radius center-x center-y))
      (sdl:with-init ()
        (sdl:window size size
                    :bpp 32
                    :title-caption "sRGB in Munsell space"
                    :sw t)
        (sdl:initialise-default-font sdl:*font-10x20*)
        (sdl:clear-display bg-color)
        (setf (sdl:frame-rate) framerate)
        (sdl:with-events ()
          (:quit-event () t)
          (:key-down-event ()
                           (sdl:push-quit-event))
          (:mouse-button-down-event (:x j :y i)
                                    (format t "(H V C) = ~A~%"
                                            (multiple-value-list (coord-to-mhvc i j))))
          (:idle ()
                 (when (<= value100 100)
                   (sdl:clear-display bg-color)
                   (lparallel:pdotimes (i size)
                     (dotimes (j size)
                       (multiple-value-bind (qr qg qb)
                           (multiple-value-call #'mhvc-to-qrgb
                             (coord-to-mhvc i j)
                             :clamp nil)
                         (declare (type sint qr qg qb))
                         (when (and (<= 0 qr 255)
                                    (<= 0 qg 255)
                                    (<= 0 qb 255))
                           (sdl:draw-pixel-* i j
                                             :color (sdl:color :r qr :g qg :b qb :a 0))))))
                   (sdl:draw-vline center-x 0 size :color line-col)
                   (sdl:draw-hline 0 size center-y :color line-col)
                   (sdl:draw-string-solid (format nil "V=~,2F" (* value100 0.1d0))
                                          (sdl:point :x 10 :y 10))
                   (sdl:update-display)
                   (incf value100))))))))


(defun draw-mrd-in-lchab (value &key (size 300) (max-chroma 50) (max-cstarab 500d0))
  "Draws Munsell Renotation Data and its extrapolation in LCH(ab)
space."
  (check-type max-chroma (integer 0))
  (let* ((radius (round (/ size 2)))
         (center-x radius)
         (center-y radius)
         (purple (sdl:color :r 251 :g 101 :b 176)))
    (labels ((polar-to-cartesian (c h)
               (let ((h-rad (mod (* h dufy/internal:+TWO-PI/360+) dufy/internal:TWO-PI))
                     (c-scaled (* c (/ radius max-cstarab))))
                 (values (* c-scaled (cos h-rad))
                         (* c-scaled (sin h-rad))))))
      (sdl:with-init ()
        (sdl:window size size
                    :bpp 32
                    :title-caption "Munsell Renotation Data")
        (sdl:clear-display sdl:*black*)
        (loop for hue40 from 0 below 40
              do (loop for chroma from 0 below max-chroma by 2
                       do (multiple-value-bind (_ cstarab1 hab1)
                              (dufy:mhvc-to-lchab-illum-c hue40 value chroma)
                            (declare (ignore _))
                            (multiple-value-bind (_ cstarab2 hab2)
                                (dufy:mhvc-to-lchab-illum-c hue40 value (+ chroma 2))
                              (declare (ignore _))
                              (multiple-value-bind (x1-offseted y1-offseted)
                                  (polar-to-cartesian cstarab1 hab1)
                                (multiple-value-bind (x2-offseted y2-offseted)
                                    (polar-to-cartesian cstarab2 hab2)
                                  (unless (and (<= (abs x1-offseted) radius)
                                               (<= (abs y1-offseted) radius)
                                               (<= (abs x2-offseted) radius)
                                               (<= (abs y2-offseted) radius))
                                    (loop-finish))
                                  (let ((x1 (round (+ center-x x1-offseted)))
                                        (y1 (round (+ center-y y1-offseted)))
                                        (x2 (round (+ center-x x2-offseted)))
                                        (y2 (round (+ center-y y2-offseted))))
                                    (sdl:draw-line-* x1 y1 x2 y2
                                                     :color (if (evenp hue40)
                                                                sdl:*white*
                                                                purple)))))))))
        (sdl:update-display)
        (sdl:with-events ()
          (:quit-event () t)
          (:key-down-event () (sdl:push-quit-event))
          (:video-expose-event () (sdl:update-display)))))))
