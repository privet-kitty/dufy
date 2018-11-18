;;;
;;; This script file fetches the Munsell renotation data and saves
;;; several arrays as a .lisp file.
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:drakma :babel :dufy/core :alexandria)))

(use-package :dufy/internal)

(defparameter *this-pathname* (load-time-value (or #.*compile-file-pathname* *load-pathname* )))

(defparameter *dat-url* "http://www.rit-mcsl.org/MunsellRenotation/all.dat")
(defparameter *dat-txt* (babel:octets-to-string (drakma:http-request *dat-url*) :encoding :ascii))

;;;
;;; Some required definitions
;;;

(defun munsell-value-to-y (v)
  (let ((v (float v 1d0)))
    (* v (+ 1.1914d0 (* v (+ -0.22533d0 (* v (+ 0.23352d0 (* v (+ -0.020484d0 (* v 0.00081939d0)))))))) 0.01d0)))

(defun lchab-to-xyy (lstar cstarab hab)
  (multiple-value-call #'dufy/core:xyz-to-xyy
    (dufy/core:lchab-to-xyz lstar cstarab hab :illuminant dufy/core:+illum-c+)))

(defun xyy-to-lchab (small-x small-y y)
  (multiple-value-call #'dufy/core:xyz-to-lchab
    (dufy/core:xyy-to-xyz small-x small-y y)
    :illuminant dufy/core:+illum-c+))

(defun midpoint-in-lchab (l1 c1 h1 l2 c2 h2)
  (values (/ (+ l1 l2) 2)
          (/ (+ c1 c2) 2)
          (circular-lerp 0.5d0 h1 h2 360d0)))

;;;
;;; Parse and process the Munsell Renotation Data
;;;

(defparameter munsell-renotation-data nil)

;; Reads the Munsell Renotation Data to a list. Y values in the MRD
;; are substituted by #'munsell-value-to-y (the formula in the ASTM
;; D1535-08e1)
(with-input-from-string (in *dat-txt*)
  (setf munsell-renotation-data nil)
  (read-line in) ; the first row is the label of the data
  (let ((*read-default-float-format* 'double-float))
    (loop
      (let* ((hue (read in nil))
             (value (read in nil))
             (chroma (read in nil))
             (small-x (read in nil))
             (small-y (read in nil))
             (large-y (read in nil)))
        (declare (ignore large-y))
        (if (null hue)
            (return)
            (unless (<= small-y 0) ; ignore non-positive y
              (let ((row (list hue value chroma small-x small-y
                               (munsell-value-to-y value))))
                (push row munsell-renotation-data))))))))

(defun quantize-hue-specifier (hue-name hue-prefix)
  (let ((hue-number
         (alexandria:switch (hue-name :test #'string=)
           ("R" 0) ("YR" 1) ("Y" 2) ("GY" 3) ("G" 4)
           ("BG" 5) ("B" 6) ("PB" 7) ("P" 8) ("RP" 9)
           (t (error "invalid spec")))))
    (mod (+ (* 4 hue-number) (round (/ hue-prefix 2.5))) 40)))

;; Quantizes hue spec. in the list.
(setf munsell-renotation-data
      (mapcar #'(lambda (row)
                  (let* ((hue-str (string (car row)))
                         (hue-name (remove-if-not #'alpha-char-p hue-str))
                         (hue-prefix (read-from-string (remove-if #'alpha-char-p hue-str))))
                    (cons (quantize-hue-specifier hue-name hue-prefix) (cdr row))))
              munsell-renotation-data))

;; Constructs max-chroma-table (for 40 hues and 11 values in {0, ...,
;; 10}) and max-chroma-table-dark (for 40 hues and 6 values in {0,
;; 0.2, ..., 1}).
(defparameter max-chroma-table
  (make-array '(40 11) :element-type 'fixnum))
(defparameter max-chroma-table-dark
  (make-array '(40 6) :element-type 'fixnum))

(dotimes (hue 40)
  (dotimes (value 11)
     ;; Uses value = 1 when value = 0, as the data V = 0 are not in the MRD. 
    (let ((adopted-value (max value 1)))
      (setf (aref max-chroma-table hue value)
            (apply #'max
                   (loop for row in munsell-renotation-data
                         when (and (= (first row) hue)
                                   (= (second row) adopted-value))
                           collect (third row)))))))


;; We need to interpolate the missing data at 10Y 0.2/2.
;;     H         V         C         x         y         Y
;;  7.5Y        0.2         2     1.434     1.459   0.002311024478048d0
;; lchab = 2.08753985167084d0 7.549196715434751d0 89.85043633813427d0
;; 2.5GY        0.2         2     0.713     1.414   0.002311024478048d0
;; lchab = 2.08753985167084d0 7.44443693317718d0 125.96087881778826d0

;; the midpoint in LCH(ab) space
;;   10Y        0.2         2
;; xyY = 1.2831848831448003d0 1.7589921911402864d0 0.0023110244780479997d0
;; lchab = 2.08753985167084d0 7.496816824305965d0 107.90565757796126d0

;; the midpoint in xy-plane (with polar coordinates)
;; 10Y        0.2         2
;; xyY = 1.051555310936564d0 1.4873480274716935d0 0.002311024478048d0

(push (append (list 12 0.2d0 2)
              (multiple-value-list
               (multiple-value-call #'lchab-to-xyy
                 (multiple-value-call #'midpoint-in-lchab
                   (xyy-to-lchab 1.434d0 1.459d0 (munsell-value-to-y 0.2d0))
                   (xyy-to-lchab 0.713d0 1.414d0 (munsell-value-to-y 0.2d0))))))
      munsell-renotation-data)

(dotimes (hue 40)
  (dotimes (dark-value 6)
     ;; Use dark-value = 1 (i.e. 0.2) when dark-value = 0, as the data
     ;; V = 0 are not in the MRD.
    (let ((adopted-dark-value (max 1 dark-value)))
      (setf (aref max-chroma-table-dark hue dark-value)
            (apply #'max
                   (loop for row in munsell-renotation-data
                         when (and (= (first row) hue)
                                   (nearly= 1d-4 (second row) (* adopted-dark-value 0.2d0)))
                           collect (third row)))))))

(defun max-chroma-in-mrd (hue value)
  ;; Use value = 0.2d0 when value = 0, as the data value = 0 are not in the MRD.
  (let ((adopted-value (max 0.2d0 value)))
    (apply #'max
           (loop for row in munsell-renotation-data
                 when (and (= (first row) hue)
                           (= (second row) adopted-value))
                   collect (third row)))))

;; We set the xyY values for (h v c) = (34 0.4 22), (34 0.4 24), (34
;; 0.4 26) as follows:
;; 
;; (34 0.4 22): mid-point (in LCHab space) of (33 0.4 22) and (35 0.4 22)
;; (34 0.4 24): mid-point (in LCHab space) of (33 0.4 24) and (35 0.4 24)
;; (34 0.4 26): mid-point (in LCHab space) of (33 0.4 26) and (35 0.4 26)
;; 
;; It is because the extrapolation of the original data produces
;; `hue reversal' in (33 0.4 26) and (34 0.4 26).
(push (list 34 0.4d0 22
            0.23863304104174685d0 0.021956689226634594d0 0.004549364801536d0)
      munsell-renotation-data)
(push (list 34 0.4d0 24
            0.2359524230440069d0 0.01812880106322657d0 0.004549364801536d0)
      munsell-renotation-data)
(push (list 34 0.4d0 26
            0.23314942818551052d0 0.013166613598197537d0 0.004549364801536d0)
      munsell-renotation-data)

(defun munsell-value-to-achromatic-xyy (v)
  "Illuminant C."
  (multiple-value-bind (x y) (dufy/core:illuminant-xy dufy/core:+illum-c+)
    (values x y (munsell-value-to-y v))))

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
       (multiple-value-list (multiple-value-call #'dufy/core:xyz-to-lchab
                              (apply #'dufy/core:xyy-to-xyz it)
                              :illuminant dufy/core:+illum-c+))))

(defun get-extrapolated-lchab-from-dat (hue-num value chroma)
  "CHROMA must be even."
  (or (get-lchab-from-dat hue-num value chroma)
      (let* ((mchroma (max-chroma-in-mrd hue-num value)))
        (destructuring-bind (lstar cstarab hab)
            (get-lchab-from-dat hue-num value mchroma)
          (list lstar
                (* cstarab (/ chroma mchroma))
                hab)))))



;; constructs Munsell-to-LCHab data.

(defconstant number-of-chromas 26)

(defparameter mrd-table-ch
  (make-array (list 40 11 number-of-chromas 2)
              :element-type 'double-float))
;; separate the data whose values are within [0, 1]
(defparameter mrd-table-ch-dark
  (make-array (list 40 6 number-of-chromas 2)
              :element-type 'double-float))

(defparameter mrd-table-l
  (make-array 11 :element-type 'double-float))
(defparameter mrd-table-l-dark
  (make-array 6 :element-type 'double-float))


(defun xyy-to-lchab (x y largey)
  (multiple-value-bind (lstar cstarab hab)
      (multiple-value-call #'dufy/core:xyz-to-lchab
        (dufy/core:xyy-to-xyz x y largey)
        :illuminant dufy/core:+illum-c+)
    (list (alexandria:clamp lstar 0d0 100d0)
          cstarab
          hab)))

(dotimes (hue 40)
  (dolist (value '(0 1 2 3 4 5 6 7 8 9 10))
    (dotimes (half-chroma number-of-chromas)
      (destructuring-bind (lstar cstarab hab)
          (get-extrapolated-lchab-from-dat hue value (* half-chroma 2))
        (setf (aref mrd-table-l value) lstar)
        (setf (aref mrd-table-ch hue value half-chroma 0) cstarab)
        (setf (aref mrd-table-ch hue value half-chroma 1) hab)))))

(dotimes (hue 40)
  (dotimes (value-idx 6)
    (let ((value (aref #(0d0 0.2d0 0.4d0 0.6d0 0.8d0 1d0) value-idx)))
      (dotimes (half-chroma number-of-chromas)
        (destructuring-bind (lstar cstarab hab)
            (get-extrapolated-lchab-from-dat hue value (* half-chroma 2))
          (setf (aref mrd-table-l-dark value-idx) lstar)
          (setf (aref mrd-table-ch-dark hue value-idx half-chroma 0) cstarab)
          (setf (aref mrd-table-ch-dark hue value-idx half-chroma 1) hab))))))


;; Saves to the .lisp file.
(defun main (dest-filenamy)
  (let ((dest-path (uiop:merge-pathnames* dest-filenamy *this-pathname*)))
    (with-open-file (out dest-path
                         :direction :output
                         :if-exists :supersede)
      (format out ";;; This file is automatically generated by ~a.~%~%"
              (file-namestring *this-pathname*))
      (format out "(in-package :dufy/munsell)~%~%")
      (print-make-array "+mrd-table-ch+" mrd-table-ch out t)
      (print-make-array "+mrd-table-ch-dark+" mrd-table-ch-dark out t)
      (print-make-array "+mrd-table-l+" mrd-table-l out t)
      (print-make-array "+mrd-table-l-dark+" mrd-table-l-dark out t)
      (print-make-array "+max-chroma-table+" max-chroma-table out t)
      (print-make-array "+max-chroma-table-dark+" max-chroma-table-dark out t))

    (format t "Munsell Renotation Data is successfully fetched and converted.~%")
    (format t "The file is saved at ~A~%" dest-path)))

(main "munsell-renotation-data.lisp")

#-swank(uiop:quit)
