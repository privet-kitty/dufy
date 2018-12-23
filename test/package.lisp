(defpackage :dufy/test
    (:use :cl :dufy :dufy/internal :fiveam :alexandria :cl-csv :parse-float)
  (:export #:main-suite))

(in-package :dufy/test)

(def-suite main-suite)

