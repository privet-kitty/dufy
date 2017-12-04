CLCL - Color Library for Common Lisp
====

CLCL is a color library for Common Lisp which is aimed at an exact color manipulation and conversion in various color models. It supports following color spaces:

* Munsell color system
* all kinds of RGB color spaces: sRGB, Adobe RGB, etc. (CLCL can handle a user-defined RGB working space.)
* HSV
* XYZ
* xyY 
* CIELAB and LCH(ab)

CLCL has following features:

* It can deal with a prepared or user-defined standard illuminant for each of the color spaces.
* It avoids defining special structures or classes to express a color. E.g., a converter from RGB to XYZ takes just three numbers and returns (a list of) three numbers. 