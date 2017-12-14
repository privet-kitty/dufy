Dufy - Color Library for Common Lisp
====

Dufy is a library for an exact color manipulation and conversion in various color models. It supports following color spaces:

* Munsell color system
* all kinds of RGB spaces: sRGB, Adobe RGB, etc. (It can handle a user-defined RGB working space.)
* XYZ
* xyY
* HSV
* HSL
* CIELAB and LCH(ab)
* CIELUV and LCH(uv)

Dufy has following features:

* It can deal with a prepared or user-defined standard illuminant for each of the color spaces. It has various chromatic adapatation routines between two standard illuminants: Bradford, Von Kries, CIECAT02, etc.
* It avoids defining special structures or classes to express a color: e.g., a converter from RGB to XYZ receives just three numbers and returns (a list of) three numbers.

# Dependencies
* alexandria
* cl-ppcre

All of the dependent libraries can be installed with quicklisp.

# Install

The easiest way to install Dufy is to use quicklisp. The following is an example of the installation on SBCL:

    > cd ~/quicklisp/local-projects
    > git clone git@github.com:privet-kitty/dufy.git
    > sbcl
    
    * (ql:register-local-projects)
    * (ql:quickload :dufy)

The path of the local projects is holded in `ql:*local-project-directories*`.

If you want to use ASDF without quicklisp, you should put the directory of Dufy to an appropriate location and do `(asdf:load-system :dufy)`.

# Usage
## Basics

The fundamental color space of Dufy is CIE XYZ (Illuminant D65): There are `xyz-to-` and `-to-xyz` converters for all other color spaces. Every converter function just receives numbers and returns a list of numbers:

    * (dufy:lab-to-xyz 48.26 -28.84 -8.475)
    => (0.11617539329731778d0 0.1699996724486797d0 0.23092502506058624d0)

    * (apply #'dufy:xyz-to-rgb255 *)
    => (0 128 128)
    => NIL

In the above example of a conversion from CIELAB to RGB, `xyz-to-rgb255` returns two values. The second value is an out-of-gamut flag.

    * (dufy:xyz-to-rgb255 0.37314 0.70144 1.0601)
    => (0 255 255)
    => T
    ; i.e. The input XYZ color is out of gamut,

Out of which gamut, however? By default, `xyz-to-rgb255` (and other `-to-rgb255` converters) regard it as sRGB (D65). You can specify the RGB space explicitly:

    * (dufy:xyz-to-rgb255 0.37314 0.70144 1.0601 :rgbspace dufy:srgb)  ; sRGB
    => (0 255 255)
    => T 

    * (dufy:xyz-to-rgb255 0.37314 0.70144 1.0601 :rgbspace dufy:adobe) ; Adobe RGB
    => (0 255 255)
    => NIL

Likewise most converters regard the implicit standard illuminant as D65. You can also specify it explicitly:

    * (dufy:lab-to-xyz 48.26 -28.84 -8.475)                ; Illuminant D65 
    * (dufy:lab-to-xyz 48.26 -28.84 -8.475 dufy:illum-d65) ; Illuminant D65
    => (0.11617539329731778d0 0.1699996724486797d0 0.23092502506058624d0)

    * (dufy:lab-to-xyz 48.26 -28.84 -8.475 dufy:illum-a)   ; Illuminant A
    => (0.13427072267932444d0 0.1699996724486797d0 0.07545996979158637d0)

When you nest two or more converters, you may want to use higher-order functions as [alexandria:rcurry](https://common-lisp.net/project/alexandria/draft/alexandria.html#index-rcurry-61):

    * (apply #'dufy:xyz-to-rgb255
             (dufy:lab-to-xyz 87.0676 -78.1391 -20.5142))
    => (0 255 255)
    => T

    * (apply #'dufy:xyz-to-rgb255
             (dufy:lab-to-xyz 87.0676 -78.1391 -20.5142)
             :rgbspace dufy:adobe)
    => GRAMMATICAL ERROR

    * (apply (alexandria:rcurry #'dufy:xyz-to-rgb255 :rgbspace dufy:adobe)
             (dufy:lab-to-xyz 87.0676 -78.1391 -20.5142))
    => (0 255 255)
    => NIL
