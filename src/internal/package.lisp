(defpackage :dufy/internal
  (:use :cl :alexandria :trivia)
  (:export #:*dat-dir-path*
           #:print-make-array
           #:time-after-gc
           #:internal-real-time-after-gc
           #:time-median
           #:call-with-profiling
           #:with-profiling
           #:with-ensuring-type
           #:dotimes-unroll
           #:tuple
           #:nlet
           #:with-read-only
           
           #:TWO-PI
           #:+TWO-PI/360+
           #:+360/TWO-PI+
           #:degree-to-radian
           #:radian-to-degree
           #:nearly=
           #:nearly<=
           #:nearly-equal
           #:nearly-equal-values
           #:circular-nearer
           #:circular-clamp
           #:circular-lerp-loose
           #:circular-lerp
           #:circular-member
           #:square
           #:pow

           #:matrix33
           #:+identity-matrix+
           #:+empty-matrix+
           #:invert-matrix
           #:multiply-mat-vec
           #:multiply-mat-mat
           #:multiply-matrices

           #:colorspace
           #:define-colorspace
           #:primary-converter
           #:define-primary-converter
           #:defconverter
           #:defconverters
           #:functional
           #:define-functional
           #:extend-functional

           #:make-mb-line
           #:mb-line-ray-intersect-distance
           #:mb-line-distance-from-origin))
