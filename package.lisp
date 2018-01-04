(cl:in-package :cl-user)

(defpackage :dufy
  (:use :common-lisp :alexandria)
  ;(:shadowing-import-from :alexandria :rcurry :clamp)
  (:export :xyy-to-xyz
	   :xyz-to-xyy
	   :illuminant
	   :illuminant-x
	   :illuminant-y
	   :illuminant-largex
	   :illuminant-largey
	   :illuminant-largez
	   :new-illuminant
	   :defilluminant
	   :illum-a :illum-e
	   :illum-c :illum-d50 :illum-d65
	   :calc-ca-matrix
	   :gen-ca-converter
	   
	   :bradford
	   :xyz-scaling
	   :von-kries
	   :cmccat97
	   :cmccat2000
	   :cat02

	   :rgbspace
	   :srgb :srgbd65 :adobe :ntsc1953 :pal/secam :prophoto
	   :new-rgbspace
	   :rgbspace-linearizer
	   :rgbspace-delinearizer
	   :rgbspace-illuminant
	   :rgbspace-xr
	   :rgbspace-yr
	   :rgbspace-xg
	   :rgbspace-yg
	   :rgbspace-xb
	   :rgbspace-yb
	   :rgbspace-to-xyz-matrix
	   :rgbspace-from-xyz-matrix
	   :gen-linearizer
	   :gen-delinearizer

	   :xyz-to-lab
	   :lab-to-xyz
	   :lstar-to-y
	   :rgb255-to-lab
	   :lab-to-lchab
	   :lchab-to-lab
	   :xyy-to-lab
	   :lab-to-xyy
	   :xyz-to-lchab
	   :xyy-to-lchab
	   :lchab-to-xyz
	   :lchab-to-xyy

	   :xyz-to-luv
	   :luv-to-xyz
	   :luv-to-lchuv
	   :lchuv-to-luv
	   :xyz-to-lchuv
	   :lchuv-to-xyz

	   :deltae
	   :xyz-deltae
	   :rgb255-deltae
	   :deltae94
	   :xyz-deltae94
	   :rgb255-deltae94
	   :deltae00
	   :xyz-deltae00
	   :rgb255-deltae00

	   :delinearize
	   :linearize
	   :nearly=
	   :nearly<=
	   :xyz-to-lrgb
	   :lrgb-to-xyz
	   :rgb-to-lrgb
	   :lrgb-to-rgb
	   :xyz-to-rgb
	   :rgb-to-xyz
	   :rgb-to-rgb255
	   :rgb255-to-rgb
	   :xyz-to-rgb255
	   :rgb255-to-xyz
	   :rgb255-to-hex
	   :hex-to-rgb255
	   :xyz-to-hex
	   :hex-to-xyz
	   :two-pi
	   :subtract-with-mod
	   :circular-lerp
	   ;:polar-mean-of-xy
	   ;:xy-to-polar
	   ;:polar-to-xy
	   :rgb1+
	   :rgb1-

	   :hsv-to-rgb
	   :rgb-to-hsv
	   :hsv-to-rgb255
	   :rgb255-to-hsv
	   :hsv-to-xyz
	   :xyz-to-hsv

	   :hsl-to-rgb
	   :rgb-to-hsl
	   :hsl-to-rgb255
	   :rgb255-to-hsl
	   :hsl-to-xyz
	   :xyz-to-hsl

	   :munsell-value-to-y
	   :y-to-munsell-value
	   :rgb255-to-munsell-value
	   :munsell-hvc-out-of-mrd-p
	   :munsell-hvc-to-xyy
	   :munsell-hvc-to-xyz
	   :munsell-hvc-to-lrgb
	   :munsell-hvc-to-rgb255
	   :munsell-hvc-to-lchab
	   :munsell-hvc-to-spec
	   :munsell-spec-to-hvc
	   :munsell-spec-out-of-mrd-p
	   :munsell-spec-to-lchab
	   :munsell-spec-to-xyz
	   :munsell-spec-to-xyy
	   :munsell-spec-to-rgb255
	   :max-chroma

	   :color-matching-x
	   :color-matching-y
	   :color-matching-z
	   :color-matching
	   :spectrum-to-xyz
	   :bb-spectrum
	   :optimal-spectrum
))
