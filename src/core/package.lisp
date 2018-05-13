(cl:in-package :cl-user)

(defpackage dufy.package.def.core
  (:use :cl))
(in-package :dufy.package.def.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-deltae-symbols (name)
    "Generates the function names of delta-E converters automatically."
    (cons name
	  (mapcar #'(lambda (term)
		      (intern (format nil "~:@(~A-~A~)" term name) :keyword))
		  '(qrgb xyz)))))

(defpackage dufy-core
  (:use :cl :alexandria :dufy-internal)
  #.`(:export  ;; xyz.lisp
      :xyy-to-xyz
      :xyz-to-xyy
      
      :gen-spectrum
      :approximate-spectrum
      :gen-illum-d-spectrum
      :spectrum-to-xyz
      :xyz-to-spectrum
      :bb-spectrum
      :optimal-spectrum1
      :optimal-spectrum2
      :flat-spectrum
      :spectrum-sum

      :observer
      :make-observer
      :observer-cmf-x
      :observer-cmf-y
      :observer-cmf-z
      :observer-cmf
      :observer-cmf-arr
      :observer-begin-wl
      :observer-end-wl
      :+obs-cie1931+
      :+obs-cie1964+

      :no-spd-error
      :illuminant
      :illuminant-x
      :illuminant-z
      :illuminant-spectrum
      :illuminant-observer
      :illuminant-has-spectrum
      :illuminant-xy
      :make-illuminant
      :+illum-a+ :+illum-e+
      :+illum-c+ :+illum-d50+ :+illum-d65+
      :gen-cat-function
      :def-cat-function

      :cat
      :make-cat
      :cat-matrix
      :cat-inv-matrix
      :xyz-to-lms
      :lms-to-xyz
      :+bradford+
      :+xyz-scaling+
      :+von-kries+
      :+cmccat97+
      :+cmccat2000+
      :+cat97s-revised+
      :+cat02+

      ;; rgb.lisp
      :rgbspace
      :+srgb+
      :+bg-srgb-10+ :+bg-srgb-12+ :+bg-srgb-16+
      :+scrgb-16+ :+scrgb-nl+
      :+adobe+ :+adobe-16+
      :+cie-rgb+
      :+ntsc1953+ :+pal/secam+
      :+prophoto+ :+prophoto-12+ :+prophoto-16+
      :+wide-gamut+
      :make-rgbspace
      :copy-rgbspace
      :rgbspace-illuminant
      :rgbspace-xr
      :rgbspace-yr
      :rgbspace-xg
      :rgbspace-yg
      :rgbspace-xb
      :rgbspace-yb
      :rgbspace-to-xyz-matrix
      :rgbspace-from-xyz-matrix
      :rgbspace-normal
      :rgbspace-lmin
      :rgbspace-lmax
      :rgbspace-bit-per-channel
      :rgbspace-min
      :rgbspace-max
      :rgbspace-qmax
      :rgbspace-quantizer
      :rgbspace-dequantizer
      :gen-linearizer
      :gen-delinearizer

      :delinearize
      :linearize
      :xyz-to-lrgb
      :lrgb-to-xyz
      :lrgb-out-of-gamut-p
      :rgb-to-lrgb
      :lrgb-to-rgb
      :rgb-out-of-gamut-p
      :xyz-to-rgb
      :rgb-to-xyz
      :rgb-to-qrgb
      :qrgb-to-rgb
      :qrgb-out-of-gamut-p
      :lrgb-to-qrgb
      :qrgb-to-lrgb
      :xyz-to-qrgb
      :qrgb-to-xyz
      
      :qrgb-to-int
      :int-to-qrgb
      :rgb-to-int
      :int-to-rgb
      :lrgb-to-int
      :int-to-lrgb
      :xyz-to-int
      :int-to-xyz

      :gen-rgbspace-changer
      
      :hsv-to-rgb
      :rgb-to-hsv
      :hsv-to-qrgb
      :qrgb-to-hsv
      :hsv-to-xyz
      :xyz-to-hsv

      :hsl-to-rgb
      :rgb-to-hsl
      :hsl-to-qrgb
      :qrgb-to-hsl
      :hsl-to-xyz
      :xyz-to-hsl

      ;; lab-and-luv.lisp
      :xyz-to-lab
      :lab-to-xyz
      :lstar-to-y
      :lab-to-lchab
      :lchab-to-lab
      :xyz-to-lchab
      :lchab-to-xyz

      :xyz-to-luv
      :luv-to-xyz
      :luv-to-lchuv
      :lchuv-to-luv
      :xyz-to-lchuv
      :lchuv-to-xyz

      ;; deltae.lisp
      ,@(alexandria:mappend #'gen-deltae-symbols
                            '(:deltae
                              :deltae94
                              :deltae00))))
