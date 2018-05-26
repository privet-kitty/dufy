(cl:in-package :cl-user)

(defpackage dufy-munsell
  (:use :cl :dufy-core :dufy-internal :alexandria)
  (:import-from :dufy-core
   dufy-core::define-primary-converter
   dufy-core::define-colorspace
   dufy-core::defconverter)
  (:export
   :invalid-mhvc-error
   :munsellspec-parse-error
   :large-approximation-error

   :*maximum-chroma*
   :max-chroma-in-mrd
   :mhvc-out-of-mrd-p
   :munsell-out-of-mrd-p
   
   :munsell-value-to-y
   :y-to-munsell-value

   :mhvc-to-lchab-illum-c
   :mhvc-to-xyz-illum-c
   :mhvc-to-xyz
   :mhvc-to-munsell
   :munsell-to-mhvc
   :munsell-to-lchab-illum-c
   :munsell-to-xyz-illum-c
   :munsell-to-xyz

   ;; ad hoc
   :mhvc-to-qrgb
   :munsell-to-qrgb
   
   :lchab-to-mhvc-illum-c
   :lchab-to-munsell-illum-c
   :xyz-to-mhvc-illum-c
   :xyz-to-mhvc
   :xyz-to-munsell-illum-c
   :xyz-to-munsell))
