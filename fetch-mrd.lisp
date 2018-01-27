(require :drakma)
(require :babel)
(require :dufy)
(require :alexandria)

;;; This is a script file which fetches the Munsell renotation data and saves several arrays as a .lisp file.

(defparameter obj-name "munsell-renotation-data2.lisp")
(defparameter obj-path (merge-pathnames obj-name *load-pathname*))

(defparameter dat-url "http://www.rit-mcsl.org/MunsellRenotation/all.dat")
(defparameter dat-txt (babel:octets-to-string (drakma:http-request dat-url) :encoding :ascii))

(defun make-adjustable-string (s)
               (make-array (length s)
                           :fill-pointer (length s)
                           :adjustable t
                           :initial-contents s
                           :element-type (array-element-type s)))

(defun subseq-if (predicate sequence &rest args)
  (let ((len (length sequence))
	(str (make-adjustable-string "")))
    (dotimes (idx len str)
      (let ((x (elt sequence idx)))
	(if (apply predicate (cons x args))
	    (vector-push-extend x str))))))


(defun quantize-40hue (hue-name hue-prefix)
  (let ((hue-number
	 (alexandria:switch (hue-name :test #'string=)
	   ("R" 0) ("YR" 1) ("Y" 2) ("GY" 3) ("G" 4)
	   ("BG" 5) ("B" 6) ("PB" 7) ("P" 8) ("RP" 9)
	   (t (error "invalid spec")))))
    (mod (+ (* 4 hue-number) (round (/ hue-prefix 2.5))) 40)))

(defparameter munsell-renotation-data nil)

;; hold the munsell renotation data as list
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
	     (let ((row (list hue value chroma x y
			      (dufy:munsell-value-to-y value))))
	       (push row munsell-renotation-data)))))))

(let ((quantized-data nil))
  (dolist (x munsell-renotation-data)
    (let* ((hue-str (string (car x)))
	   (hue-name (subseq-if #'alpha-char-p hue-str))
	   (hue-prefix (read-from-string (subseq-if (complement #'alpha-char-p) hue-str))))
      (push (cons (quantize-40hue hue-name hue-prefix) (cdr x)) quantized-data)))
  (setf munsell-renotation-data quantized-data))

;; the largest chroma in the renotation data
(defparameter max-chroma-overall (apply #'max (mapcar #'third munsell-renotation-data)))


;; construct max-chroma-arr(for 40 hues and V in [1, 10])
;; and max-chroma-arr-dark (for 40 hues and V in [0, 1])
(defparameter max-chroma-arr
  (make-array '(40 11) :element-type 'fixnum))
(defparameter max-chroma-arr-dark
  (make-array '(40 6) :element-type 'fixnum))

(dotimes (hue 40)
  (dotimes (value 11)
     ;use value=1 when value=0, as the data V=0 are not in mrd. 
    (let ((value$ (if (zerop value) 1 value)))
      (setf (aref max-chroma-arr hue value)
	    (let ((rows nil))
	      (dolist (row munsell-renotation-data)
		(if (and (= (first row) hue)
			 (= (second row) value$))
		    (push (third row) rows)))
	      (apply #'max rows))))))


;; (defun mean (x y)
;;   (* (+ x y) 0.5))


;; We need to interpolate the missing data at 10Y 0.2/2.
;;     H         V         C         x         y         Y
;;  7.5Y        0.2         2     1.434     1.459   0.237
;; lchab = 55.78616839326824d0 535.9969016772114d0 89.62792336089832d0
;; 2.5GY        0.2         2     0.713     1.414   0.237
;; lchab = 55.78616839326824d0 371.0625522161684d0 99.13832996818876d0

;; the mean on LCH(ab) space
;;   10Y        0.2         2
;; xyY = 1.0817607707836834d0 1.6255500743912765d0 0.23700000349119013d0
;; lchab = 55.78616839326824d0 453.52972694668995d0 94.38312666454354d0

;; the mean on xy-plane (with polar coordinates)
;; 10Y        0.2         2
;; xyY = 1.051555310936564d0 1.4873480274716935d0 0.237d0


;; (push (append (list 12 0.2d0 2)
;; 	      (dufy::polar-mean-of-xy 1.434d0 1.459d0 0.713d0 1.414d0)
;; 	      (list 0.00237d0))
;;       munsell-renotation-data)

(push (list 12 0.2d0 2
	    1.0817607707836834d0 1.6255500743912765d0 0.237d0)
      munsell-renotation-data)


(dotimes (hue 40)
  (dotimes (dark-value 6)
     ; use dark-value=1 (i.e. 0.2) when dark-value=0, as the data V=0 are not in mrd. 
    (let ((value$ (if (zerop dark-value) 1 dark-value)))
      (setf (aref max-chroma-arr-dark hue dark-value)
	    (let ((rows nil))
	      (dolist (row munsell-renotation-data)
		(if (and (= (first row) hue)
			 (dufy:nearly= 0.0001d0 (second row) (* value$ 0.2d0)))
		    (push (third row) rows)))
	      (apply #'max rows))))))

;; (defun max-chroma-integer-case (hue value)
;;   ;use value=1 when value=0, as the data value=0 are not in mrd. 
;;   (let ((value$ (if (zerop value) 1 value)))
;;     (let ((rows nil))
;;       (dolist (row munsell-renotation-data)
;; 	(if (and (= (first row) hue)
;; 		 (= (second row) value$))
;; 	    (push (third row) rows)))
;;       (apply #'max rows))))


;; get data without correcting the luminance factor, i.e. max(Y) = 1.0257 (not 1.00)
;; The data with value=0 are substituted with the data with value=0.2.
(defun get-xyy-from-dat (hue-num value chroma)
  (cond ((= chroma 0)
	 (dufy::munsell-value-to-achromatic-xyy value))
	((= value 0)
	 (funcall #'(lambda (lst)
		      (if (null lst)
			  nil
			  (append (subseq lst 3 5) '(0d0))))
		  (find-if #'(lambda (row)
			       (and (= (mod (first row) 40) (mod hue-num 40))
				    (dufy:nearly= 0.001d0 (second row) 0.2d0)
				    (= (third row) chroma)))
			   munsell-renotation-data)))
	(t
	 (funcall #'(lambda (lst)
		      (if (null lst)
			  nil
			  (subseq lst 3 6)))
		  (find-if #'(lambda (row)
			       (and (= (mod (first row) 40) (mod hue-num 40))
				    (dufy:nearly= 0.001d0 (second row) value)
				    (= (third row) chroma)))
			   munsell-renotation-data)))))

;; (defun get-xyy-from-dat (hue-num value chroma)
;;   (let ((mchroma (dufy:max-chroma hue-num values)))
;;     (if (<= chroma mchroma)
;; 	(get-xyy-from-dat hue-num value chroma)
;; 	(let ((factor (/ chroma mchroma))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun get-lchab-from-dat (hue-num value chroma)
  (aif (get-xyy-from-dat hue-num value chroma)
       (apply (alexandria:rcurry #'dufy:xyz-to-lchab dufy:illum-c)
	      (apply #'dufy:xyy-to-xyz it))))

(defun get-extrapolated-lchab-from-dat (hue-num value chroma)
  "CHROMA must be even."
  (aif (get-lchab-from-dat hue-num value chroma)
       it
       (let* ((mchroma (dufy:max-chroma hue-num value)))
	 (destructuring-bind (lstar cstarab hab)
	     (get-lchab-from-dat hue-num value mchroma)
	   (list lstar
		 (* cstarab (/ chroma mchroma))
		 hab)))))

(defparameter value-list '(0 1 2 3 4 5 6 7 8 9 10 0.2 0.4 0.6 0.8))
(defparameter value-variety (length value-list))

(defparameter half-chroma-variety (+ (/ max-chroma-overall 2) 1))
(defparameter mrd-array
  (make-array (list 40 11 half-chroma-variety 3)
	      :element-type 'double-float))

;; separate the data whose values are within [0, 1]
(defparameter mrd-array-dark
  (make-array (list 40 6 half-chroma-variety 3)
	      :element-type 'double-float))

(defparameter mrd-array-lchab
  (make-array (list 40 11 half-chroma-variety 3)
	      :element-type 'double-float))

(defparameter mrd-array-lchab-dark
  (make-array (list 40 6 half-chroma-variety 3)
	      :element-type 'double-float))
  

;; construct mrd-array

(defparameter large-negative-float -1d99)

(defun xyy-to-lchab (x y largey)
  (destructuring-bind (lstar cstarab hab)
      (apply (alexandria:rcurry #'dufy:xyz-to-lchab dufy:illum-c)
	     (dufy:xyy-to-xyz x y largey))
    (list (alexandria:clamp lstar 0d0 100d0)
	  cstarab
	  hab)))

;; (dotimes (hue 40)
;;   (dolist (value '(0 1 2 3 4 5 6 7 8 9 10))
;;     (dotimes (half-chroma half-chroma-variety)
;;       (let ((xyy (get-xyy-from-dat hue value (* half-chroma 2))))
;; 	(if (null xyy)
;; 	    (progn
;; 	      (setf (aref mrd-array hue value half-chroma 0) large-negative-float)
;; 	      (setf (aref mrd-array hue value half-chroma 1) large-negative-float)
;; 	      (setf (aref mrd-array hue value half-chroma 2) large-negative-float))
;; 	    (destructuring-bind (x y largey) xyy
;; 	      (destructuring-bind (lstar cstarab hab) (xyy-to-lchab x y largey)
;; 		(setf (aref mrd-array hue value half-chroma 0) (coerce x 'double-float))
;; 		(setf (aref mrd-array hue value half-chroma 1) (coerce y 'double-float))
;; 		(setf (aref mrd-array hue value half-chroma 2) largey))))))))


(dotimes (hue 40)
  (dolist (value '(0 1 2 3 4 5 6 7 8 9 10))
    (dotimes (half-chroma half-chroma-variety)
      (destructuring-bind (lstar cstarab hab)
	  (get-extrapolated-lchab-from-dat hue value (* half-chroma 2))
	(setf (aref mrd-array-lchab hue value half-chroma 0) lstar)
	(setf (aref mrd-array-lchab hue value half-chroma 1) cstarab)
	(setf (aref mrd-array-lchab hue value half-chroma 2) hab)))))

;; construct mrd-array-dark
;; (dotimes (hue 40)
;;   (dotimes (value-idx 6)
;;     (let ((value (* 0.2d0 value-idx)))
;;       (dotimes (half-chroma half-chroma-variety)
;; 	(let ((xyy (get-xyy-from-dat hue value (* half-chroma 2))))
;; 	  (if (null xyy)
;; 	      (progn
;; 		(setf (aref mrd-array-dark hue value-idx half-chroma 0) large-negative-float)
;; 		(setf (aref mrd-array-dark hue value-idx half-chroma 1) large-negative-float)
;; 		(setf (aref mrd-array-dark hue value-idx half-chroma 2) large-negative-float))     
;; 	      (destructuring-bind (x y largey) xyy
;; 		(destructuring-bind (lstar cstarab hab) (xyy-to-lchab x y largey)
;; 		  (setf (aref mrd-array-dark hue value-idx half-chroma 0) (coerce x 'double-float))
;; 		  (setf (aref mrd-array-dark hue value-idx half-chroma 1) (coerce y 'double-float))
;; 		  (setf (aref mrd-array-dark hue value-idx half-chroma 2) largey)))))))))

(dotimes (hue 40)
  (dotimes (value-idx 6)
    (let ((value (* 0.2d0 value-idx)))
      (dotimes (half-chroma half-chroma-variety)
	(destructuring-bind (lstar cstarab hab)
	    (get-extrapolated-lchab-from-dat hue value (* half-chroma 2))
	  (setf (aref mrd-array-lchab-dark hue value-idx half-chroma 0) lstar)
	  (setf (aref mrd-array-lchab-dark hue value-idx half-chroma 1) cstarab)
	  (setf (aref mrd-array-lchab-dark hue value-idx half-chroma 2) hab))))))

(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (apply #'aref array indices)
                               (recurse (1+ n))))))
      (recurse 0))))

(defun print-make-array (var-name array &optional (stream t) (declaration t))
  (let ((typ (array-element-type array))
	(dims (array-dimensions array)))
    (when declaration
      (prin1 `(declaim (type (simple-array ,typ ,dims)
			     ,(intern (string-upcase var-name))))
	     stream)
      (terpri stream))
    (format stream "(defparameter ~a ~% #." var-name)
    (prin1 `(make-array (quote ,dims)
			:element-type (quote ,typ)
			:initial-contents (quote ,(array-to-list array)))
	   stream)
    (princ ")" stream)
    (terpri stream)))


(defun main ()
  (with-open-file (out obj-path
		       :direction :output
		       :if-exists :supersede)
    (format out ";;; This file is automatically generated by ~a.~%~%"
	    (file-namestring *load-pathname*))
    (format out "(in-package :dufy)~%~%")
    ;; (print-make-array "mrd-array" mrd-array out)
    ;; (print-make-array "mrd-array-dark" mrd-array-dark out)
    (print-make-array "mrd-array-lchab" mrd-array-lchab out)
    (print-make-array "mrd-array-lchab-dark" mrd-array-lchab-dark out)
    (print-make-array "max-chroma-arr" max-chroma-arr out)
    (print-make-array "max-chroma-arr-dark" max-chroma-arr-dark out))

  (format t "Munsell Renotation Data is successfully fetched and converted.~%")
  (format t "The file is saved at ~A~%" obj-path))

(main)
