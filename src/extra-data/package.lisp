(defpackage :dufy/extra-data
  (:use :cl :dufy/core)
  #.`(:export #:+illum-d55+
              #:+illum-d75+
              #:+illum-b+
              ,@(loop for i from 1 to 12
                      collect (make-symbol (format nil "+ILLUM-F~A+" i)))
              ,@(loop for i from 1 to 15
                      collect (make-symbol (format nil "+ILLUM-F3.~A+" i)))
              #:+illum-sox+
              #:+illum-hp1+
              #:+illum-hp2+
              #:+illum-mb+
              #:+illum-mbf+
              #:+illum-mbtf+
              #:+illum-hmi+
              #:+illum-xenon+))
