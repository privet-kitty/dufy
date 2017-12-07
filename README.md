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
* It avoids defining special structures or classes to express a color. E.g., a converter from RGB to XYZ receives just three numbers and returns (a list of) three numbers. 

# Dependencies
* alexandria
* cl-ppcre

All dependent libraries can be installed with quicklisp.

# Install

The easiest way is to use quicklisp. The following is an example of installation on SBCL:

    > cd ~/quicklisp/local-projects
    > git clone git@github.com:privet-kitty/clcl.git
    > sbcl
    
    * (ql:register-local-projects)
    * (ql:quickload :clcl)

The path of the local projects is holded in `ql:*local-project-directories*`

If you'd like to use ASDF without quicklisp, you should put the CLCL directory to an appropriate location and do `(asdf:load-system :clcl)`.

# Usage

To Do
