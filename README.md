Dufy - Color Library for Common Lisp
====

Dufy is a library for an exact color manipulation and conversion in various color models, which supports the following color spaces:

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

* Standard illuminant: C, D65, etc. A new illuminant can be defined by white point or SPD.
* RGB space: sRGB, Adobe RGB, scRGB, etc.  A new RGB space can be defined by primary coordinates, illuminant, method of gamma correction, bit per channel and other encoding characteristics.
* Observer (Color Matching Functions): CIE 1931 2&deg; Standard Observer, CIE 1964 10&deg;. Other observer model can be defined by color matching data.
* Color difference: Delta-E<sup>*</sup><sub>ab</sub>, CIE94, CIEDE2000.
* Chromatic adaptaion transform: Bradford, Von Kries, etc. User-defined CAT is also available.


# Dependencies
* alexandria
* cl-ppcre

All of the dependent libraries can be installed with quicklisp.

# Install
The easiest way to install dufy is to use [quicklisp](https://www.quicklisp.org/beta/):

    * (ql:quickload :dufy)

The latest version can also be installed with quicklisp:

    $ cd ~/quicklisp/local-projects   # the path is held in ql:*local-project-directories*
    $ git clone git@github.com:privet-kitty/dufy.git
    $ sbcl   # , ccl, etc.
    
    * (ql:register-local-projects)
    * (ql:quickload :dufy)

If you want to use ASDF directly without quicklisp, you should put the directory of dufy to an appropriate location and do `(asdf:load-system :dufy)`.

# Usage
## Basics
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
    hex [ label = "HEX" ]
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
    qrgb -- hex
  
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

    * (dufy:lab-to-xyz 87.07 -78.15 -20.51)  ; L*=87.07, a*=-78.15, b*=-20.51
    => 0.3731384408806708d0 ; X
       0.701492216468595d0  ; Y
       1.060034922742541d0  ; Z
       
    * (multiple-value-call #'dufy:xyz-to-qrgb
        (dufy:lab-to-xyz 87.07 -78.15 -20.51))
    => -169 ; R
       255  ; G
       255  ; B

    * (multiple-value-call #'dufy:xyz-to-qrgb
        (dufy:lab-to-xyz 87.07 -78.15 -20.51)
        :clamp t)
    => 0    ; R
       255  ; G
       255  ; B

In the second example, a conversion from CIELAB to quantized RGB, `xyz-to-qrgb` returns a negative R value, which means the color is out of gamut; it is clamped in the third example.

Out of which gamut, however? By default, `xyz-to-qrgb` (and all other RGB converters) regard it as sRGB (D65). You can specify the RGB space explicitly:

    * (dufy:xyz-to-qrgb 0.37314 0.70144 1.0601 :rgbspace dufy:+srgb+)  ; sRGB
    => -169
       255
       255

    * (dufy:xyz-to-qrgb 0.37314 0.70144 1.0601 :rgbspace dufy:+adobe+) ; Adobe RGB
    => 2
       255
       255
       
    * (dufy:xyz-to-qrgb 0.37314 0.70144 1.0601 :rgbspace dufy:+bg-srgb-10+) ; bg-sRGB (10 bit)
    => 47
       893
       893
    ;; In the Adobe RGB space and bg-sRGB space the color is within gamut.

Likewise most converters regard the implicit standard illuminant as D65. You can also specify it explicitly:

    * (dufy:luv-to-xyz 100 0 0)                  ; Illuminant D65 
    * (dufy:luv-to-xyz 100 0 0 dufy:+illum-d65+) ; Illuminant D65
    => 0.9504285453771808d0
       1.0d0
       1.0889003707981282d0
    ;; the white point of standard illuminant D65

    * (dufy:luv-to-xyz 100 0 0 dufy:+illum-e+)   ; Illuminant E
    => 0.9999999999999999d0
       1.0d0
       1.0000000000000004d0


## Munsell Color System
Dufy can handle the Munsell color system in the same way as other color spaces:

    * (dufy:munsell-to-xyz "3.2R 4.5/6.1")
    => 0.19362651748394688d0
       0.15142718526032797d0
       0.12281280741243561d0
       
    * (dufy:munsell-to-xyz "3.2R 4.5/86.1")
    => 1.7829826744317545d0
       0.13203741531162577d0
       -0.02903836122088697d0

The converters are based on [Munsell renotation data](https://www.rit.edu/cos/colorscience/rc_munsell_renotation.php). In the second example `munsell-to-xyz` extrapolate the colors far out of the data (and the MacAdam limits, of course), which is meaningless in many cases but will be necessary to process the boundary colors.

    * (dufy:munsell-to-mhvc "3.2R 4.5/6.1")
    => 1.28d0
       4.5d0
       6.1d0
       
    * (dufy:mhvc-to-xyz 1.28 4.5 6.1)
    => 0.19362651748394688d0
       0.15142718526032797d0
       0.12281280741243561d0

`munsell` is the standard string notation of Munsell color. `mhvc` is its three-number expression, which will be easier to deal with in some cases. A hue number of `mhvc` corresponds to a hue string of `munsell` as follows:

| Hue in `mhvc` | Hue in `munsell` |
| -------------------- | --------------------- | 
| 0 to 4 | 10RP (=0R) to 10R (=0YR) |
| 4 to 8 | 10R (=0YR) to 10YR (=0Y) |
| ... | ... |
| 36 to 40 | 10P (=0RP) to 10RP (=0R) |

The hue of of `mhvc` is a circle group: i.e. hues outside the interval [0, 40] are acceptable:

    * (dufy:mhvc-to-munsell -400.0 4.5 6.1) ; the same as (0.0 4.5 6.1)
    => "0.00R 4.50/6.10"
    
There are some more points to remember: First, since the [Munsell renotation data](https://www.rit.edu/cos/colorscience/rc_munsell_renotation.php) is measured not with illuminant D65, but with C, the converters like `mhvc-to-xyz` do the (Bradford) transformation from C to D65. If you want to use a direct converter with illuminant C, for e.g. accuracy or efficiency, the following converters are available under illuminant C: `munsell-to-lchab`, `lchab-to-munsell`, `mhvc-to-lchab`, `lchab-to-mhvc` `mhvc-to-xyz-illum-c`. 

Second, if you want to know the gamut of the Munsell renotation data, you can find the maximum chroma for a given hue and value by `max-chroma`:

    * (dufy:max-chroma 1.28 4.5)
    => 24
    * (dufy:mhvc-to-xyz 1.28 4.5 6.1)
    => 0.19362651667300654d0
       0.1514271852669221d0
       0.12281280847832986d0 ; interpolated, since 6.1 < 24
    * (dufy:mhvc-to-xyz 1.28 4.5 24.1)
    => 0.378725146277375d0
       0.14933867420177885d0
       0.05430213863814263d0 ; extrapolated, since 24.1 > 24
       