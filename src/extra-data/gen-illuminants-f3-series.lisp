;;; This is a script file which generates fundamental data and saves them as a .lisp file.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:alexandria :dufy)))

(use-package :dufy/internal)

(defparameter this-dir-path (uiop:pathname-directory-pathname *load-pathname*))
(defparameter dat-path (merge-pathnames "FL3.x.tsv" *dat-dir-path*))
(defparameter obj-name "illuminants-f3-series.lisp")
(defparameter obj-path (merge-pathnames obj-name this-dir-path))

(eval
 `(progn
    ,@(loop for idx from 1 to 15
            collect `(defparameter ,(intern (format nil "+F3.~A-ARR+" idx))
                       (make-array 81 :element-type 'double-float)))))

(eval
 `(with-open-file (in dat-path :direction :input)
    (let ((*read-default-float-format* 'double-float))
      (dotimes (wl-idx 81)
        (read in)
        ,@(loop for i from 1 to 15
                collect `(setf (aref ,(intern (format nil "+F3.~A-ARR+" i)) wl-idx)
                               (coerce (read in) 'double-float)))))))


(with-open-file (out obj-path
                     :direction :output
                     :if-exists :supersede)
  (format out ";;; This file is automatically generated by ~a.~%~%"
          (file-namestring *load-pathname*))
  (format out "(in-package :dufy/extra-data)~%~%")
  (format out "(eval-when (:compile-toplevel :load-toplevel :execute)~%")
  (loop for i from 1 to 15
        do (print-make-array (format nil "+F3.~A-ARR+" i)
                             (eval (intern (format nil "+F3.~A-ARR+" i)))
                             out t))
  (format out ")~%")
  (loop for i from 1 to 15
        do (print
            `(defparameter ,(intern (format nil "+ILLUM-F3.~A+" i))
               (dufy:make-illuminant :spectrum
                                     (dufy:gen-spectrum ,(intern (format nil "+F3.~A-ARR+" i))
                                                        380 780)
                                     :begin-wl 380 :end-wl 780
                                     :compile-time t))
            out)))

(format t "The file is saved at ~A~%" obj-path)

#-swank (uiop:quit 1)
