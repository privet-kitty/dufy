(uiop:define-package :dufy/core
  (:nicknames :dufy-core)
  (:use :cl :alexandria :dufy/internal)
  (:export #:spectrum
           #:gen-spectrum
           #:approximate-spectrum
           #:gen-illum-d-spectrum
           #:bb-spectrum
           #:optimal-spectrum-peak
           #:optimal-spectrum-trough
           #:flat-spectrum

           ;; observer
           #:observer
           #:make-observer
           #:observer-cmf-x
           #:observer-cmf-y
           #:observer-cmf-z
           #:observer-cmf
           #:observer-cmf-arr
           #:observer-begin-wl
           #:observer-end-wl
           #:+obs-cie1931+
           #:+obs-cie1964+

           ;; illuminant
           #:no-spd-error
           #:illuminant
           #:illuminant-x
           #:illuminant-z
           #:illuminant-spectrum
           #:illuminant-observer
           #:illuminant-no-spd-p
           #:illuminant-xy
           #:make-illuminant

           #:+illum-a+ #:+illum-c+ #:+illum-e+ #:+illum-d50+ #:+illum-d65+
           
           #:xyz #:xyy
           #:xyy-to-xyz
           #:xyz-to-xyy 
           #:spectrum-to-xyz
           #:xyz-to-spectrum

           #:lrgb #:rgb #:qrgb #:rgbpack
           #:rgba #:qrgba #:rgbapack
           #:rgbspace
           #:make-rgbspace
           #:rgbspace-illuminant
           #:rgbspace-xr
           #:rgbspace-yr
           #:rgbspace-xg
           #:rgbspace-yg
           #:rgbspace-xb
           #:rgbspace-yb
           #:rgbspace-to-xyz-matrix
           #:rgbspace-from-xyz-matrix
           #:rgbspace-normal
           #:rgbspace-lmin
           #:rgbspace-lmax
           #:rgbspace-bit-per-channel
           #:rgbspace-min
           #:rgbspace-max
           #:rgbspace-qmax
           #:rgbspace-linearizer
           #:rgbspace-delinearizer
           #:gen-linearizer
           #:gen-delinearizer

           #:linearize
           #:delinearize
           #:xyz-to-lrgb
           #:lrgb-to-xyz
           #:lrgb-out-of-gamut-p
           #:rgb-to-lrgb
           #:lrgb-to-rgb
           #:rgb-out-of-gamut-p
           #:xyz-to-rgb
           #:rgb-to-xyz
           #:quantize
           #:dequantize
           #:rgb-to-qrgb
           #:qrgb-to-rgb
           #:qrgb-out-of-gamut-p
           #:lrgb-to-qrgb
           #:qrgb-to-lrgb
           #:xyz-to-qrgb
           #:qrgb-to-xyz
           
           #:qrgb-to-rgbpack
           #:rgbpack-to-qrgb
           #:rgb-to-rgbpack
           #:rgbpack-to-rgb
           #:lrgb-to-rgbpack
           #:rgbpack-to-lrgb
           #:xyz-to-rgbpack
           #:rgbpack-to-xyz

           #:hsv
           #:hsv-to-rgb
           #:rgb-to-hsv
           #:hsv-to-lrgb
           #:lrgb-to-hsv
           #:hsv-to-qrgb
           #:qrgb-to-hsv
           #:hsv-to-rgbpack
           #:rgbpack-to-hsv
           #:hsv-to-xyz
           #:xyz-to-hsv

           #:hsl
           #:hsl-to-rgb
           #:rgb-to-hsl
           #:hsl-to-lrgb
           #:lrgb-to-hsl
           #:hsl-to-qrgb
           #:qrgb-to-hsl
           #:hsl-to-rgbpack
           #:rgbpack-to-hsl
           #:hsl-to-xyz
           #:xyz-to-hsl

           #:lab #:lchab
           #:xyz-to-lab
           #:lab-to-xyz
           #:lstar-to-y
           #:lab-to-lchab
           #:lchab-to-lab
           #:xyz-to-lchab
           #:lchab-to-xyz

           #:luv #:lchuv
           #:xyz-to-luv
           #:luv-to-xyz
           #:luv-to-lchuv
           #:lchuv-to-luv
           #:xyz-to-lchuv
           #:lchuv-to-xyz

           #:hsluv
           #:hsluv-to-lchuv
           #:lchuv-to-hsluv
           #:hsluv-to-xyz
           #:xyz-to-hsluv
           #:hsluv-to-lrgb
           #:lrgb-to-hsluv
           #:hsluv-to-rgb
           #:rgb-to-hsluv
           #:hsluv-to-qrgb
           #:qrgb-to-hsluv
           #:hsluv-to-rgbpack
           #:rgbpack-to-hsluv

           #:hpluv
           #:hpluv-to-lchuv
           #:lchuv-to-hpluv
           #:hpluv-to-xyz
           #:xyz-to-hpluv
           #:hpluv-to-lrgb
           #:lrgb-to-hpluv
           #:hpluv-to-rgb
           #:rgb-to-hpluv
           #:hpluv-to-qrgb
           #:qrgb-to-hpluv
           #:hpluv-to-rgbpack
           #:rgbpack-to-hpluv

           #:lms
           #:cat
           #:make-cat
           #:cat-matrix
           #:cat-inv-matrix
           #:xyz-to-lms
           #:lms-to-xyz
           #:+bradford+
           #:+xyz-scaling+
           #:+von-kries+
           #:+cmccat97+
           #:+cmccat2000+
           #:+cat97s-revised+
           #:+cat02+

           #:gen-cat-function
           #:define-cat-function
           #:gen-rgbspace-changer
           #:copy-rgbspace

           #:+srgb+
           #:+bg-srgb-10+ #:+bg-srgb-12+ #:+bg-srgb-16+
           #:+scrgb-16+ #:+scrgb-nl+
           #:+adobe+ #:+adobe-16+
           #:+cie-rgb+
           #:+ntsc1953+ #:+pal/secam+
           #:+prophoto+ #:+prophoto-12+ #:+prophoto-16+
           #:+wide-gamut+))

;; Export the function names of delta-E
(dolist (colorspace '(lab qrgb xyz))
  (dolist (name '(#:deltaeab #:deltae94 #:deltae00 #:deltaecmc))
    (export (intern (format nil "~:@(~A-~A~)" colorspace name)))))
