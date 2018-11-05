;;; This is a script file which generates fundamental data and saves
;;; them as a .lisp file.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :dufy-internal)))

(use-package :dufy-internal)

;;
;; Definitions
;;

(defparameter this-dir-path (uiop:pathname-directory-pathname *load-pathname*))
(defparameter obj-name "cmf-data.lisp")
(defparameter obj-path (merge-pathnames obj-name this-dir-path))

(defparameter cmf-arr-cie1931
  (make-array '(471 3) :element-type 'double-float :initial-element 0d0))
(defparameter cmf-arr-cie1964
  (make-array '(471 3) :element-type 'double-float :initial-element 0d0))

(defun fill-color-matching-arr (arr csv-path)
  (with-open-file (in csv-path :direction :input)
    (let ((*read-default-float-format* 'double-float))
      (dotimes (idx 471)
        (read in) ; Skips the first column
        (dotimes (coord 3)
          (setf (aref arr idx coord)
                (coerce (read in) 'double-float)))))))

;;
;; Main
;;

(fill-color-matching-arr cmf-arr-cie1931
                         (merge-pathnames "cmf-cie1931.tsv" *dat-dir-path*))
(fill-color-matching-arr cmf-arr-cie1964
                         (merge-pathnames "cmf-cie1964.tsv" *dat-dir-path*))

(with-open-file (out obj-path
                     :direction :output
                     :if-exists :supersede)
  (format out ";;; This file is automatically generated by ~a.~%~%"
          (file-namestring *load-pathname*))
  (format out "(in-package :dufy-core)~%~%")
  (print-make-array "CMF-ARR-CIE1931" cmf-arr-cie1931 out)
  (print-make-array "CMF-ARR-CIE1964" cmf-arr-cie1964 out))

(format t "The file is saved at ~A~%" obj-path)

#-swank (uiop:quit)
