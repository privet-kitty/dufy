(cl:in-package :cl-user)

(defpackage #:dufy/internal
  (:nicknames #:dufy-internal)
  (:use #:cl #:alexandria #:dufy/internal)
  (:export
   #:*dat-dir-path*
   #:print-make-array
   #:time-after-gc
   #:internal-real-time-after-gc
   #:time-median
   #:call-with-profiling
   #:with-profiling
   #:with-ensuring-type
   #:dotimes-unroll
   #:tuple

   #:TWO-PI
   #:+TWO-PI/360+ #:+360/TWO-PI+
   #:nearly=
   #:nearly<=
   #:nearly-equal
   #:circular-nearer
   #:circular-clamp
   #:circular-lerp-loose
   #:circular-lerp
   #:circular-member
   #:square
   #:fast-expt

   #:matrix33
   #:+identity-matrix+
   #:+empty-matrix+
   #:invert-matrix
   #:multiply-mat-vec
   #:multiply-mat-mat
   #:multiply-matrices))
