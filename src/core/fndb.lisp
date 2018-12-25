(in-package :dufy/core)

(defknown rgbspace-min (rgbspace) double-float
    (movable flushable foldable)
  :overwrite-fndb-silently t)

(defknown rgbspace-qmax (rgbspace) (integer 0 #.most-positive-fixnum)
    (movable flushable foldable)
  :overwrite-fndb-silently t)

(defknown rgbspace-qmax-float/length (rgbspace) double-float
    (movable flushable foldable)
  :overwrite-fndb-silently t)

(defknown rgbspace-delinearizer (rgbspace) (function * (values double-float &optional))
    (movable flushable foldable)
  :overwrite-fndb-silently t)

(defknown rgbspace-illuminant (rgbspace) illuminant
    (movable flushable foldable)
  :overwrite-fndb-silently t)

(defknown rgbspace-from-xyz-matrix (rgbspace) matrix33
    (movable flushable foldable)
  :overwrite-fndb-silently t)

(defknown illuminant-x (illuminant) double-float
    (movable flushable foldable)
  :overwrite-fndb-silently t)

(defknown illuminant-z (illuminant) double-float
    (movable flushable foldable)
  :overwrite-fndb-silently t)
