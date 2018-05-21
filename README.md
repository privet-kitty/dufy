Dufy - Color Library for Common Lisp
====
[![Build Status](https://travis-ci.org/privet-kitty/dufy.svg?branch=master)](https://travis-ci.org/privet-kitty/dufy)
[![Quicklisp dist](http://quickdocs.org/badge/dufy.svg)](http://quickdocs.org/dufy/)

Dufy is a library for exact color manipulation and conversion in various color spaces, which supports the following color models:

* Munsell color system
* RGB
* XYZ
* xyY
* HSV
* HSL
* CIELAB and LCh<sub>ab</sub>
* CIELUV and LCh<sub>uv</sub>
* LMS
* Spectrum (as spectral power distribution function)

Dufy can deal with the following concepts:

* Illuminant: C, D65, etc. A new illuminant can be defined by white point or SPD.
* RGB space: sRGB, Adobe RGB, scRGB, etc.  A new RGB space can be defined by primary coordinates, illuminant, method of gamma correction, bit per channel and other encoding characteristics.
* Observer (Color Matching Functions): CIE 1931 2&deg; Standard Observer, CIE 1964 10&deg;. Other observer model can be defined by color matching data.
* Color difference: Delta-E<sup>*</sup><sub>ab</sub>, CIE94, CIEDE2000.
* Chromatic adaptaion transform: Bradford, Von Kries, etc. User-defined CAT is also available.


# Documentation
Besides this README file, most of the documentation is written as docstrings in the source code. Some other information is in [github wiki](https://github.com/privet-kitty/dufy/wiki).


# Dependencies
* alexandria
* cl-ppcre

All of the dependent libraries can be installed with quicklisp.

# Install
The easiest way to install dufy is to use [quicklisp](https://www.quicklisp.org/beta/):

    * (ql:quickload :dufy)

The latest version in this repository can also be installed with quicklisp:

    $ cd ~/quicklisp/local-projects   # the path is held in ql:*local-project-directories*
    $ git clone git@github.com:privet-kitty/dufy.git
    $ sbcl   # , ccl, etc.
    
    * (ql:register-local-projects)
    * (ql:quickload :dufy)

If you want to use ASDF directly without quicklisp, you should put the directory of dufy to an appropriate location and do `(asdf:load-system :dufy)`.

# Basic Usage
![Tree of Direct Converters](https://g.gravizo.com/source/converter_tree?https%3A%2F%2Fraw.githubusercontent.com%2Fprivet-kitty%2Fdufy%2Fdevelop%2FREADME.md)

<details> 
<summary></summary>
converter_tree
  graph G {
    graph [
      labelloc = "t",
      label = "Tree of Primary Converters",
      fontsize = 16
    ];
    node [shape = "box", fontname = "helvetica"]
    xyz [ label = "XYZ" ]
    xyy [ label = "XYY\n(xyY)" ]
    lrgb [ label = "LRGB\n(linear RGB)" ]
    rgb [ label = "RGB\n(gamma-corrected RGB)" ]
    qrgb [ label = "QRGB\n(quantized RGB)" ]
    int [ label = "INT" ]
    lab [ label = "LAB" ]
    lchab [ label = "LCHAB" ]
    luv [ label = "LUV" ]
    lchuv [ label = "LCHUV" ]
    mhvc [ label = "MHVC\n(Munsell 3-number spec.)" ]
    munsell [ label = "MUNSELL\n(Munsell string spec.)" ]
    hsv [ label = "HSV" ]
    hsl [ label = "HSL" ]
    spectrum [ label = "SPECTRUM" ]
    lms [ label = "LMS" ]
    xyz -- xyy
    xyz -- lms
    xyz -- spectrum
    xyz -- lrgb
    lrgb -- rgb
    rgb -- qrgb
    qrgb -- int
  
    xyz -- lab
    lab -- lchab
    xyz -- luv
    luv -- lchuv
    rgb -- hsv
    rgb -- hsl

    lchab -- mhvc  [ label = "(illuminant C)" ]
    mhvc -- munsell

  }
converter_tree
</details>

The fundamental color space of dufy is CIE XYZ (Illuminant D65): There are `xyz-to-` and `-to-xyz` converters for all other color spaces. Every converter function just receives numbers and returns multiple numbers:

```lisp
(dufy:lab-to-xyz 87.07 -78.15 -20.51)  ; L*=87.07, a*=-78.15, b*=-20.51
;; => 0.3731544163010862d0 ; X
;; 0.701492216468595d0     ; Y
;; 1.0600774614243746d0    ; Z
   
(multiple-value-call #'dufy:xyz-to-qrgb
  (dufy:lab-to-xyz 87.07 -78.15 -20.51)
  :clamp nil)
;; => -169 ; R
;;    255  ; G
;;    255  ; B

(multiple-value-call #'dufy:xyz-to-qrgb
  (dufy:lab-to-xyz 87.07 -78.15 -20.51)
  :clamp t)
;; => 0    ; R
;;    255  ; G
;;    255  ; B
```

In the second example, a conversion from CIELAB to quantized RGB, `xyz-to-qrgb` returns a negative R value, which means the color is out of gamut; it is clamped in the third example.

Out of which gamut, however? By default, `xyz-to-qrgb` (and all other RGB converters) regard it as sRGB (D65). You can specify the RGB space explicitly:

```lisp
(dufy:xyz-to-qrgb 0.37314 0.70144 1.0601 :rgbspace dufy:+srgb+ :clamp nil)  ; sRGB
;; => -169
;;    255
;;    255

(dufy:xyz-to-qrgb 0.37314 0.70144 1.0601 :rgbspace dufy:+adobe+ :clamp nil) ; Adobe RGB
;; => 2
;;    255
;;    255
   
(dufy:xyz-to-qrgb 0.37314 0.70144 1.0601 :rgbspace dufy:+bg-srgb-10+ :clamp nil) ; bg-sRGB (10 bit)
;; => 47
;;    893
;;    893
;; In the Adobe RGB space and bg-sRGB space the color is within gamut.
```

Likewise most converters regard the implicit illuminant as D65. You can also specify it explicitly:

```lisp
(dufy:luv-to-xyz 100 0 0)                  ; Illuminant D65 
(dufy:luv-to-xyz 100 0 0 :illuminant dufy:+illum-d65+) ; Illuminant D65
;; => 0.9504692366968726d0
;; 1.0d0
;; 1.0889440678362423d0
;; the white point of standard illuminant D65

(dufy:luv-to-xyz 100 0 0 :illuminant dufy:+illum-e+)   ; Illuminant E
;; => 1.0d0
;;    1.0d0
;;    1.0000000000000004d0
```


# Modules
Dufy consists of several independent modules:
- dufy
  - dufy-core
  - dufy-munsell
- dufy-extra-data
- dufy-examples

Since the main package `dufy` contains slightly large colorimetric data, you may want to load `dufy-core` instead of `dufy` in some cases.
