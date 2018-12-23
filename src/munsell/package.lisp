(defpackage :dufy/munsell
  (:nicknames :dufy-munsell)
  (:use :cl :alexandria :cl-ppcre :dufy/internal :dufy/core)
  (:export #:mhvc #:munsell
           #:invalid-mhvc-error
           #:munsellspec-parse-error
           
           #:*most-positive-non-large-double-float*
           #:non-negative-non-large-double-float
           #:max-chroma-in-mrd
           #:mhvc-out-of-mrd-p
           #:munsell-out-of-mrd-p
           
           #:munsell-value-to-y
           #:y-to-munsell-value
           #:munsell-value-to-lstar
           #:lstar-to-munsell-value

           #:munsell-to-mhvc
           #:mhvc-to-munsell

           #:mhvc-to-lchab-illum-c
           #:mhvc-to-xyz-illum-c
           #:mhvc-to-xyz
           #:munsell-to-lchab-illum-c
           #:munsell-to-xyz-illum-c
           #:munsell-to-xyz

           #:large-approximation-error
           #:lchab-to-mhvc-illum-c
           #:lchab-to-munsell-illum-c
           #:xyz-to-mhvc-illum-c
           #:xyz-to-mhvc
           #:xyz-to-munsell-illum-c
           #:xyz-to-munsell))
