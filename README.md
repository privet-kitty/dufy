Dufy - Color Library for Common Lisp
====

Dufy is a library for an exact color manipulation and conversion in various color models. It supports following color spaces:

* Munsell color system
* all kinds of RGB spaces: sRGB, Adobe RGB, etc. (User-defined RGB working space is available.)
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

The easiest way to install dufy is to use quicklisp. The following is an example of the installation on SBCL:

    > cd ~/quicklisp/local-projects
    > git clone git@github.com:privet-kitty/dufy.git
    > sbcl
    
    * (ql:register-local-projects)
    * (ql:quickload :dufy)

The path of the local projects is held in `ql:*local-project-directories*`.

If you want to use ASDF without quicklisp, you should put the directory of dufy to an appropriate location and do `(asdf:load-system :dufy)`.

# Usage
## Basics
![Tree of Direct Converters](https://g.gravizo.com/source/converter_tree?https%3A%2F%2Fraw.githubusercontent.com%2Fprivet-kitty%2Fdufy%2Fdevelop%2FREADME.md)

<details> 
<summary></summary>
converter_tree
  graph  {
    graph [
      labelloc = "t",
      label = "Tree of Direct Converters",
      fontsize = 18
    ];
    node [shape = "box", fontname = "helvetica"]
    "XYZ" -- "XYY"
    "XYZ" -- "LRGB\n(linear RGB)"
    "LRGB\n(linear RGB)" -- "RGB\n(gamma-corrected RGB)"
    "RGB\n(gamma-corrected RGB)" -- "RGB255\n(quantized RGB)"
    "RGB255\n(quantized RGB)" -- "HEX"
  
    "XYZ" -- "LAB"
    "LAB" -- "LCHAB"
    "XYZ" -- "LUV"
    "LUV" -- "LCHUV"
    "RGB\n(gamma-corrected RGB)" -- "HSV"
    "RGB\n(gamma-corrected RGB)" -- "HSL"

    "LCHAB" -- "MUNSELL-HVC"
    "MUNSELL-HVC" -- "MUNSELL-SPEC"
  }
converter_tree
</details>

The fundamental color space of dufy is CIE XYZ (Illuminant D65): There are `xyz-to-` and `-to-xyz` converters for all other color spaces. Every converter function just receives numbers and returns a list of numbers:

    * (dufy:lab-to-xyz 48.26 -28.84 -8.475)  ; L*=48.26, a*=-28.84, b*=-8.475
    => (0.11617539329731778d0 0.1699996724486797d0 0.23092502506058624d0)

    * (apply #'dufy:xyz-to-rgb255 *)
    => (0 128 128)
    => NIL

In the above example of a conversion from CIELAB to RGB, `xyz-to-rgb255` returns two values. The second value is an out-of-gamut flag.

    * (dufy:xyz-to-rgb255 0.37314 0.70144 1.0601)
    => (0 255 255)
    => T
    ;; i.e. The input XYZ color is out of gamut,

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

## Munsell Color System
Dufy can handle Munsell color system in the same way as other color spaces:

    * (dufy:munsell-spec-to-xyz "3.2R 4.5/6.1")
    => (0.19362651667300654d0 0.1514271852669221d0 0.12281280847832986d0)
    => NIL
    * (dufy:munsell-spec-to-xyz "3.2R 4.5/26.1")
    => (-1.7976931348623157d308 -1.7976931348623157d308 -1.7976931348623157d308)
    => T

The converters are based on [Munsell renotation data](https://www.rit.edu/cos/colorscience/rc_munsell_renotation.php). The second value is a flag, meaning out of the data; chroma is to large in the second example.

    * (dufy:munsell-spec-to-hvc "3.2R 4.5/6.1")
    => (1.28 4.5 6.1)
    * (dufy:munsell-hvc-to-xyz 1.28 4.5 6.1)
    => (0.19362651667300654d0 0.1514271852669221d0 0.12281280847832986d0)

`munsell-spec` is a standard string notation of Munsell color. `munsell-hvc` is three-number-specification, which is easier to handle in some cases. The hue number of `munsell-hvc` corresponds to the hue string of `munsell-spec` as follows:

| Hue in `munsell-hvc` | Hue in `munsell-spec` | 
| 0 - 4 | 10RP (=0R) - 10R (=0YR) |
| 4 - 8 | 10R (=0YR) - 10YR (=0Y) |
| ... | ... |
| 36 - 40 | 10P (=0RP) - 10RP (=0R) |

The hue number of `munsell-hvc` is a circle group; hues outside the interval [0, 40] are available:

    * (dufy:munsell-hvc-to-spec -400.0 4.5 6.1) ; same as (0.0 4.5 6.1)
    => "0.00R 4.50/6.10"

There are some more points to remember: First, the [Munsell renotation data](https://www.rit.edu/cos/colorscience/rc_munsell_renotation.php) is measured not with illuminant D65, but with C. Sometimes you may want to use a direct converter with illuminant C, for e.g. efficiency or accuracy. The following converters are under illuminant C: `munsell-spec-to-lchab`, `munsell-hvc-to-lchab`, `munsell-hvc-to-xyz-illum-c`. 

Second, you can find the feasible chroma for a given hue number and value by `max-chroma`:

    * (dufy:max-chroma 1.28 6.1)
    => 24
    * (dufy:munsell-hvc-to-xyz 1.28 4.5 24.0)
    => (0.37763067574238846d0 0.14935160313469042d0 0.05453354054199556d0)
    * (dufy:munsell-hvc-to-xyz 1.28 4.5 24.1)
    => ERROR: Out of Munsell renotation data.