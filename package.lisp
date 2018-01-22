(cl:in-package :cl-user)

(defpackage :dufy
  (:use :common-lisp :alexandria)
  (:export :gen-spectrum ;; xyz.lisp
	   :gen-illum-d-spectrum
	   :color-matching-x
	   :color-matching-y
	   :color-matching-z
	   :color-matching
	   :spectrum-to-xyz
	   :bb-spectrum
	   :optimal-spectrum
	   :flat-spectrum
	   :spectrum-sum
	   
	   :xyy-to-xyz
	   :xyz-to-xyy
	   :illuminant
	   :illuminant-x
	   :illuminant-y
	   :illuminant-largex
	   :illuminant-largey
	   :illuminant-largez
	   :illuminant-spd
	   :make-illuminant
	   :illum-a :illum-e
	   :illum-c :illum-d50 :illum-d65
	   :calc-ca-matrix
	   :gen-ca-converter
	   :gen-ca-converter-xyy
	   
	   :bradford
	   :xyz-scaling
	   :von-kries
	   :cmccat97
	   :cmccat2000
	   :cat02

	   :rgbspace
	   :srgb :srgbd65 :srgbd50
	   :adobe :adobed65 :adobed50
	   :ntsc1953 :pal/secam :prophoto
	   :make-rgbspace
	   :copy-rgbspace
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
	   
	   ;; deltae.lisp
	   :deltae
	   :xyz-deltae
	   :rgb255-deltae
	   :deltae94
	   :xyz-deltae94
	   :rgb255-deltae94
	   :deltae00
	   :xyz-deltae00
	   :rgb255-deltae00

	   ;; munsell.lisp
	   :munsell-value-to-y
	   :y-to-munsell-value
	   :rgb255-to-munsell-value
	   :mhvc-out-of-mrd-p
	   :mhvc-to-xyy
	   :mhvc-to-xyz
	   :mhvc-to-xyz-illum-c
	   :mhvc-to-lrgb
	   :mhvc-to-rgb255
	   :mhvc-to-lchab
	   :mhvc-to-munsell
	   :munsell-to-mhvc
	   :munsell-out-of-mrd-p
	   :munsell-to-lchab
	   :munsell-to-xyz
	   :munsell-to-xyy
	   :munsell-to-rgb255
	   :max-chroma

))
