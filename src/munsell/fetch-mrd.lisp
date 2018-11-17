;;;
;;; This script file fetches the Munsell renotation data and saves
;;; several arrays as a .lisp file.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:drakma :babel :dufy :alexandria)))

(use-package :dufy/internal)

(defparameter this-dir-path (uiop:pathname-directory-pathname *load-pathname*))

(defparameter dat-url "http://www.rit-mcsl.org/MunsellRenotation/all.dat")
(defparameter dat-txt (babel:octets-to-string (drakma:http-request dat-url) :encoding :ascii))


(defparameter munsell-renotation-data nil)

;; Reads the Munsell renotation data to a list. Y values in the MRD
;; are substituted by #'dufy:munsell-value-to-y (the formula in ASTM
;; D1535-08e1)
(with-input-from-string (in dat-txt)
  (setf munsell-renotation-data nil)
  (read-line in) ; the first row is the label of the data
  (let ((*read-default-float-format* 'double-float))
    (loop
      (let* ((hue (read in nil))
             (value (read in nil))
             (chroma (read in nil))
             (x (read in nil))
             (y (read in nil))
             (largey (read in nil)))
        (declare (ignore largey))
        (if (null hue)
            (return)
            (unless (<= y 0) ; ignore non-positive y
              (let ((row (list hue value chroma x y
                               (dufy:munsell-value-to-y value))))
                (push row munsell-renotation-data))))))))


(defun quantize-40hue (hue-name hue-prefix)
  (let ((hue-number
         (alexandria:switch (hue-name :test #'string=)
           ("R" 0) ("YR" 1) ("Y" 2) ("GY" 3) ("G" 4)
           ("BG" 5) ("B" 6) ("PB" 7) ("P" 8) ("RP" 9)
           (t (error "invalid spec")))))
    (mod (+ (* 4 hue-number) (round (/ hue-prefix 2.5))) 40)))

(defun subseq-if (predicate sequence &rest args)
  (let ((len (length sequence))
        (str (make-array 0
                         :fill-pointer 0
                         :adjustable t
                         :element-type (array-element-type sequence))))
    (dotimes (idx len str)
      (let ((x (elt sequence idx)))
        (if (apply predicate (cons x args))
            (vector-push-extend x str))))))

;; Quantizes hue spec. in the list.
(let ((quantized-data nil))
  (dolist (x munsell-renotation-data)
    (let* ((hue-str (string (car x)))
           (hue-name (subseq-if #'alpha-char-p hue-str))
           (hue-prefix (read-from-string (subseq-if (complement #'alpha-char-p) hue-str))))
      (push (cons (quantize-40hue hue-name hue-prefix) (cdr x)) quantized-data)))
  (setf munsell-renotation-data quantized-data))

;; the largest chroma in the renotation data
(defparameter max-chroma-overall
  (apply #'max (mapcar #'third munsell-renotation-data)))

;; Constructs max-chroma-table (for 40 hues and V in {0, ..., 10}) and
;; max-chroma-table-dark (for 40 hues and V in {0, 0.2, ..., 1}).
(defparameter max-chroma-table
  (make-array '(40 11) :element-type 'fixnum))
(defparameter max-chroma-table-dark
  (make-array '(40 6) :element-type 'fixnum))

(dotimes (hue 40)
  (dotimes (value 11)
     ;; use value=1 when value=0, as the data V=0 are not in mrd. 
    (let ((value$ (if (zerop value) 1 value)))
      (setf (aref max-chroma-table hue value)
            (let ((rows nil))
              (dolist (row munsell-renotation-data)
                (if (and (= (first row) hue)
                         (= (second row) value$))
                    (push (third row) rows)))
              (apply #'max rows))))))


;; We need to interpolate the missing data at 10Y 0.2/2.
;;     H         V         C         x         y         Y
;;  7.5Y        0.2         2     1.434     1.459   0.002311024478048d0
;; lchab = 2.08753985167084d0 7.549195077210431d0 89.8496567373897d0
;; 2.5GY        0.2         2     0.713     1.414   0.002311024478048d0
;; lchab = 2.08753985167084d0 7.4444050402972115d0 125.96055583396455d0

;; the mean on LCH(ab) space
;;   10Y        0.2         2
;; xyY = 1.1087179491051968d0 1.568249876229978d0 0.0023110244780479997d0
;; lchab = 2.08753985167084d0 7.496800058753822d0 107.90510628567712d0

;; the mean on xy-plane (with polar coordinates)
;; 10Y        0.2         2
;; xyY = 1.051555310936564d0 1.4873480274716935d0 0.002311024478048d0


;; (push (append (list 12 0.2d0 2)
;;               (dufy::polar-mean-of-xy 1.434d0 1.459d0 0.713d0 1.414d0)
;;               (list 0.00237d0))
;;       munsell-renotation-data)

(push (list 12 0.2d0 2
            1.051555310936564d0 1.4873480274716935d0 0.002311024478048d0)
      munsell-renotation-data)


(dotimes (hue 40)
  (dotimes (dark-value 6)
     ;; use dark-value=1 (i.e. 0.2) when dark-value=0, as the data V=0 are not in mrd. 
    (let ((value$ (if (zerop dark-value) 1 dark-value)))
      (setf (aref max-chroma-table-dark hue dark-value)
            (let ((rows nil))
              (dolist (row munsell-renotation-data)
                (if (and (= (first row) hue)
                         (nearly= 0.0001d0 (second row) (* value$ 0.2d0)))
                    (push (third row) rows)))
              (apply #'max rows))))))

(defun max-chroma-simple-case (hue value)
  ;; use value=0.2d0 when value=0, as the data value=0 are not in mrd. 
  (let ((value$ (if (zerop value) 0.2d0 value)))
    (let ((rows nil))
      (dolist (row munsell-renotation-data)
        (if (and (= (first row) hue)
                 (= (second row) value$))
            (push (third row) rows)))
      (apply #'max rows))))


(defun munsell-value-to-achromatic-xyy (v)
  "Illuminant C."
  (multiple-value-bind (x y) (dufy:illuminant-xy dufy:+illum-c+)
    (values x y (dufy:munsell-value-to-y v))))

(defun get-xyy-from-dat (hue-num value chroma)
  "Illuminant C. Returns a list. 

Note: The data with value=0 are substituted with the data with
value=0.2."
  (cond ((= chroma 0)
         (multiple-value-list (munsell-value-to-achromatic-xyy value)))
        ((= value 0)
         (funcall #'(lambda (lst)
                      (if (null lst)
                          nil
                          (append (subseq lst 3 5) '(0d0))))
                  (find-if #'(lambda (row)
                               (and (= (mod (first row) 40) (mod hue-num 40))
                                    (nearly= 0.001d0 (second row) 0.2d0)
                                    (= (third row) chroma)))
                           munsell-renotation-data)))
        (t
         (funcall #'(lambda (lst)
                      (if (null lst)
                          nil
                          (subseq lst 3 6)))
                  (find-if #'(lambda (row)
                               (and (= (mod (first row) 40) (mod hue-num 40))
                                    (nearly= 0.001d0 (second row) value)
                                    (= (third row) chroma)))
                           munsell-renotation-data)))))


(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun get-lchab-from-dat (hue-num value chroma)
  (aif (get-xyy-from-dat hue-num value chroma)
       (multiple-value-list (multiple-value-call #'dufy:xyz-to-lchab
                              (apply #'dufy:xyy-to-xyz it)
                              :illuminant dufy:+illum-c+))))

(defun get-extrapolated-lchab-from-dat (hue-num value chroma)
  "CHROMA must be even."
  (or (get-lchab-from-dat hue-num value chroma)
      (let* ((mchroma (max-chroma-simple-case hue-num value)))
        (destructuring-bind (lstar cstarab hab)
            (get-lchab-from-dat hue-num value mchroma)
          (list lstar
                (* cstarab (/ chroma mchroma))
                hab)))))



;; constructs Munsell-to-LCHab data.

(defparameter half-chroma-size (+ (/ max-chroma-overall 2) 1))

(defparameter mrd-table-ch
  (make-array (list 40 11 half-chroma-size 2)
              :element-type 'double-float))
;; separate the data whose values are within [0, 1]
(defparameter mrd-table-ch-dark
  (make-array (list 40 6 half-chroma-size 2)
              :element-type 'double-float))

(defparameter mrd-table-l
  (make-array 11 :element-type 'double-float))
(defparameter mrd-table-l-dark
  (make-array 6 :element-type 'double-float))


(defun xyy-to-lchab (x y largey)
  (multiple-value-bind (lstar cstarab hab)
      (multiple-value-call #'dufy:xyz-to-lchab
        (dufy:xyy-to-xyz x y largey)
        :illuminant dufy:+illum-c+)
    (list (alexandria:clamp lstar 0d0 100d0)
          cstarab
          hab)))

(dotimes (hue 40)
  (dolist (value '(0 1 2 3 4 5 6 7 8 9 10))
    (dotimes (half-chroma half-chroma-size)
      (destructuring-bind (lstar cstarab hab)
          (get-extrapolated-lchab-from-dat hue value (* half-chroma 2))
        (setf (aref mrd-table-l value) lstar)
        (setf (aref mrd-table-ch hue value half-chroma 0) cstarab)
        (setf (aref mrd-table-ch hue value half-chroma 1) hab)))))

(dotimes (hue 40)
  (dotimes (value-idx 6)
    (let ((value (aref #(0d0 0.2d0 0.4d0 0.6d0 0.8d0 1d0) value-idx)))
      (dotimes (half-chroma half-chroma-size)
        (destructuring-bind (lstar cstarab hab)
            (get-extrapolated-lchab-from-dat hue value (* half-chroma 2))
          (setf (aref mrd-table-l-dark value-idx) lstar)
          (setf (aref mrd-table-ch-dark hue value-idx half-chroma 0) cstarab)
          (setf (aref mrd-table-ch-dark hue value-idx half-chroma 1) hab))))))


;; Saves to the .lisp file.
(defun main (obj-filename)
  (let ((obj-path (merge-pathnames obj-filename this-dir-path)))
    (with-open-file (out obj-path
                         :direction :output
                         :if-exists :supersede)
      (format out ";;; This file is automatically generated by ~a.~%~%"
              (file-namestring *load-pathname*))
      (format out "(in-package :dufy/munsell)~%~%")
      (print-make-array "+mrd-table-ch+" mrd-table-ch out t)
      (print-make-array "+mrd-table-ch-dark+" mrd-table-ch-dark out t)
      (print-make-array "+mrd-table-l+" mrd-table-l out t)
      (print-make-array "+mrd-table-l-dark+" mrd-table-l-dark out t)
      (print-make-array "+max-chroma-table+" max-chroma-table out t)
      (print-make-array "+max-chroma-table-dark+" max-chroma-table-dark out t))

    (format t "Munsell Renotation Data is successfully fetched and converted.~%")
    (format t "The file is saved at ~A~%" obj-path)))

(main "munsell-renotation-data.lisp")

#-swank(uiop:quit)
