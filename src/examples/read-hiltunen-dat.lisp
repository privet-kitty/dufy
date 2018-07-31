;; -*- coding:utf-8 -*-

;;
;; This script file compares the XYZ values of Munsell renotation data
;; (1943) and Joensuu's spectral data (Munsell Book 1976, matt chip,
;; spectralphotometer).
;;

;; Usage:
;; $ sbcl --load read-hiltunen-dat.lisp
;; (Quicklisp is necessary. Clozure CL also works.)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ppcre)
  (ql:quickload :cl-ftp)
  (ql:quickload :babel)
  (ql:quickload :dufy)
  (ql:quickload :quri)
  (ql:quickload :cl-fad))

(defparameter *label-url* (quri:uri "ftp://ftp.cs.joensuu.fi/pub/color/spectra/mspec/README.txt"))
(defparameter *label-filename* "README-joensuu-matt1.txt")
(defparameter *label-path* (merge-pathnames *label-filename* (asdf:component-pathname (asdf:find-component :dufy :dat))))

(defparameter *dat-url* (quri:uri "ftp://ftp.cs.joensuu.fi/pub/color/spectra/mspec/munsell380_800_1.asc.gz"))
(defparameter *dat-filename* "munsell-joensuu-matt1.dat")
(defparameter *dat-path* (merge-pathnames *dat-filename* (asdf:component-pathname (asdf:find-component :dufy :dat))))


;; download
(unless (probe-file *label-path*)
  (ftp:with-ftp-connection (conn :hostname (quri:uri-host *label-url*))
    (format t "downloading ~A to ~A ...~%" (quri:render-uri *label-url*) *label-path*)
    (ftp:retrieve-file conn (quri:uri-path *label-url*) *label-path*)))

(unless (probe-file *dat-path*)
  (ftp:with-ftp-connection (conn :hostname (quri:uri-host *dat-url*))
    (let ((dat-archive-path
            (fad:with-output-to-temporary-file (zipout
                                                :direction :output
                                                :element-type '(unsigned-byte 8)
                                                :template "TEMPORARY-FILES:TEMP-%.gz")
              (format t "downloading ~A to ~A ...~%"
                      (quri:render-uri *dat-url*)
                      (pathname zipout))
              (ftp:retrieve-file conn (quri:uri-path *dat-url*) zipout))))
      (with-open-file (datout *dat-path* :direction :output)
        (let ((uncompress-command (format nil "gunzip -c ~A" dat-archive-path)))
          (format t "~A~%" uncompress-command)
          (uiop:run-program uncompress-command :output datout)
          (format t "dat file saved to ~A~%" *dat-path*))))))


;; utilities
(defun contain-digits-p (string)
  (loop for c across string
        when (digit-char-p c)
          do (return t)
        finally (return nil)))

(defun read-spd (in &optional (begin-wl 380) (end-wl 800))
  (let* ((size (+ 1 (- end-wl begin-wl)))
	 (spd-arr (make-array size :element-type 'double-float)))
    (dotimes (idx size)
      (setf (aref spd-arr idx) (float (read in) 1d0)))
    (dufy:gen-spectrum spd-arr begin-wl end-wl)))

(defun scale-xyz (x y z &optional dest-y)
  "Scales XYZ so that y = dest-y."
  (let ((factor (if dest-y (/ dest-y y) 1d0)))
    (values (* x factor) (* y factor) (* z factor))))


;; main
(with-open-file (label-in *label-path*)
  (with-open-file (dat-in *dat-path*)
    (format t "Color difference of XYZ values between Munsell renotation data (1943) and Joensuu's spectral data (Munsell Book 1976, matt chip, spectralphotometer).
Note: ⊿E stands for CIEDE2000.~%")
    (terpri)
    (format t "Munsell color: XYZ in Munsell renotation data, XYZ in Hiltunen's data, ⊿E~%")
    (loop repeat 62 do (read-line label-in))
    (loop for line = (read-line label-in nil 'eof)
          with idx = 0
          with max-delta = 0d0
          with delta-sum = 0d0
          until (eql line 'eof)
          when (contain-digits-p line)
            do (let ((munsellspec (delete #\space (ppcre:scan-to-strings "[0-9.]+\\s*[A-Z]+\\s*[0-9.]+\\s*/\\s*[0-9.]+" line))))
                 (multiple-value-bind (x-mrd y-mrd z-mrd)
                     (dufy:munsell-to-xyz-illum-c munsellspec)
                   (multiple-value-bind (x-new y-new z-new)
                       (dufy:spectrum-to-xyz (read-spd dat-in)
                                             :illuminant dufy:+illum-c+
                                             :begin-wl 380
                                             :end-wl 800)
                     (let ((delta (dufy:xyz-deltae00 x-mrd y-mrd z-mrd x-new y-new z-new
                                                     :illuminant dufy:+illum-c+)))
                       (incf delta-sum delta)
                       (setf max-delta (max delta max-delta))
                       (format t "~A: MRD(~,4F, ~,4F, ~,4F), Hiltunen(~,4F, ~,4F, ~,4F), ⊿E=~,3F ~%"
                               munsellspec
                               x-mrd y-mrd z-mrd
                               x-new y-new z-new
                               delta))))
                 (incf idx))
          finally (format t "max. ⊿E = ~,5F, mean ⊿E = ~,5F.~%"
                          max-delta
                          (/ delta-sum idx)))))

#-swank (quit)
