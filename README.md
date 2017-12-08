CLCL - Color Library for Common Lisp
====

CLCL is a library for an exact color manipulation and conversion in various color models. It supports following color spaces:

* Munsell color system
* all kinds of RGB spaces: sRGB, Adobe RGB, etc. (CLCL can handle a user-defined RGB working space.)
* XYZ
* xyY
* HSV
* HSL
* CIELAB and LCH(ab)
* CIELUV and LCH(uv)

CLCL has following features:

* It can deal with a prepared or user-defined standard illuminant for each of the color spaces.
* It avoids defining special structures or classes to express a color: e.g., a converter from RGB to XYZ receives just three numbers and returns (a list of) three numbers. 

# Dependencies
* alexandria
* cl-ppcre

All of the dependent libraries can be installed with quicklisp.

# Install

The easiest way to install CLCL is to use quicklisp. The following is an example of the installation on SBCL:

    > cd ~/quicklisp/local-projects
    > git clone git@github.com:privet-kitty/clcl.git
    > sbcl
    
    * (ql:register-local-projects)
    * (ql:quickload :clcl)

The path of the local projects is holded in `ql:*local-project-directories*`.

If you want to use ASDF without quicklisp, you should put the CLCL directory to an appropriate location and do `(asdf:load-system :clcl)`.

# Usage

The basic color space of CLCL is XYZ. There are `xyz-to-` and `-to-xyz` converters for all other color spaces. Every converter function just receives numbers and returns a list of numbers:

    * (clcl:lab-to-xyz 48.26 -28.84 -8.475)
    => (0.11617539329731778d0 0.1699996724486797d0 0.23092502506058624d0)

    * (apply #'clcl:xyz-to-rgb255 *)
    => (0 128 128)
    => NIL

In the above example of a conversion from CIELAB to RGB, `xyz-to-rgb255` returns two values. The second value is an out-of-gamut flag.

    * (clcl:xyz-to-rgb255 0.37314 0.70144 1.0601)
    => (0 255 255)
    => T
    ; i.e. The input color is out of gamut, to which the color (0 255 255) is close.

Which gamut, however? By default, `xyz-to-rgb255` (and most other converters) regard it as sRGB (D65). You can specify the RGB space explicitly:

    * (clcl:xyz-to-rgb255 0.37314 0.70144 1.0601 :rgbspace clcl:srgb)
    => (0 255 255)
    => T 

    * (clcl:xyz-to-rgb255 0.37314 0.70144 1.0601 :rgbspace clcl:adobe)
    => (0 255 255)
    => NIL

Likewise most converters regard the implicit standard illuminant as D65. You can also specify it explicitly:

    * (clcl:lab-to-xyz 48.26 -28.84 -8.475)
    * (clcl:lab-to-xyz 48.26 -28.84 -8.475 clcl:d65)
    => (0.11617539329731778d0 0.1699996724486797d0 0.23092502506058624d0)

    * (clcl:lab-to-xyz 48.26 -28.84 -8.475 clcl:c)
    => (0.11987634685509602d0 0.1699996724486797d0 0.25072173849374196d0)

When you nest two or more converters, you may want to use higher-order functions as [alexandria:rcurry](https://common-lisp.net/project/alexandria/draft/alexandria.html#index-rcurry-61):

    * (apply #'clcl:xyz-to-rgb255
             (clcl:lab-to-xyz 87.0676 -78.1391 -20.5142))
    => (0 255 255)
    => T

    * (apply #'clcl:xyz-to-rgb255
             (clcl:lab-to-xyz 87.0676 -78.1391 -20.5142)
	     :rgbspace clcl:adobe)
    => GRAMMATICAL ERROR.

    * (apply (alexandria:rcurry #'clcl:xyz-to-rgb255 :rgbspace clcl:adobe)
             (clcl:lab-to-xyz 87.0676 -78.1391 -20.5142))
    => (0 255 255)
    => NIL
