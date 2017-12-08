(defpackage :clcltool
  (:use :common-lisp :clcl :alexandria)
  (:export :make-munsell-inversion-data
	   :interpolate-munsell-inversion-data
	   :load-munsell-inversion-data
	   :save-munsell-inversion-data
	   :rgb255-to-munsell-hvc
	   :build-mid
	   :examine-interpolation-error
	   :encode-munsell-hvc1000
	   :decode-munsell-hvc1000
	   :decode-munsell-hvc
	   :interpolatedp
	   :set-interpolated
))

(in-package :clcltool)

(defconstant possible-colors 16777216) ;256*256*256

(defun encode-munsell-hvc1000 (h1000 v1000 c500 &optional (flag-interpolated 0))
  (+ (ash flag-interpolated 31)
     (ash h1000 20)
     (ash v1000 10)
     c500))

(defun interpolatedp (u32)
  (not (zerop (logand u32 #b10000000000000000000000000000000))))

(defun set-interpolated (u32)
  (logior #b10000000000000000000000000000000 u32))

(defun decode-munsell-hvc1000 (u32)
  (list (logand (ash u32 -20) #b1111111111)
	(logand (ash u32 -10) #b1111111111)
	(logand u32 #b1111111111)))

(defun decode-munsell-hvc (u32)
  (destructuring-bind (h1000 v1000 c500) (decode-munsell-hvc1000 u32)
    (list (/ h1000 25.0)
	  (/ v1000 100.0)
	  (/ c500 10.0))))

(defconstant +maxu32+ #xffffffff)

;; (defparameter munsell-inversion-data nil)

;; (defun initialize-munsell-inversion-data ()
;;   (setf munsell-inversion-data
;; 	(make-array possible-colors
;; 		    :element-type '(unsigned-byte 32)
;; 		    :initial-element +maxu32+)))

(defun make-munsell-inversion-data (&optional (with-interpolation t))
  (let ((mid (make-array possible-colors :element-type '(unsigned-byte 32) :initial-element +maxu32+))
	(deltae-arr (make-array possible-colors :element-type 'double-float :initial-element most-positive-double-float)))
    (dotimes (h1000 1000)
      (let ((hue (/ h1000 25.0d0)))
	(format t "processing data at hue ~a / 1000~%" h1000)
	(dotimes (v1000 1001)
	  (let* ((value (/ v1000 100.0d0))
		 (maxc500 (1+ (* (clcl:max-chroma hue value) 10))))
	    (dotimes (c500 maxc500)
	      (let ((chroma (/ c500 10.0d0)))
		(destructuring-bind (x y z)
		    (clcl::munsell-hvc-to-xyz hue value chroma)
		  (multiple-value-bind (rgb255 out-of-gamut)
		      (clcl:xyz-to-rgb255 x y z :threshold 0.001d0)
		    (unless out-of-gamut
		      (let ((hex (apply #'clcl:rgb255-to-hex rgb255)))
			(destructuring-bind (true-x true-y true-z)
			    (apply #'clcl:rgb255-to-xyz rgb255)
			  (let ((old-deltae (aref deltae-arr hex))
				(new-deltae (clcl:xyz-deltae x y z true-x true-y true-z)))
			    (when (< new-deltae old-deltae)
					;rotate if the new color is nearer to the true color than the old one.
			      (setf (aref mid hex)
				    (encode-munsell-hvc1000 h1000 v1000 c500))
			      (setf (aref deltae-arr hex)
				    new-deltae))))))))))))))
    (let ((gaps (count-gaps mid)))
      (format t "Primary data are set. Number of gaps is ~A (~A).~%"
	      gaps (/ gaps (float possible-colors))))
    (when with-interpolation
      (format t "Now interpolating...~%")
      (interpolate-munsell-inversion-data mid))
    mid))
  

;; return 0 if it is already fully interpolated
;; (defun interpolate-once ()
;;   (let ((original-mid (copy-seq munsell-inversion-data))
;; 	(not-interpolated 0))
;;     (dotimes (hex possible-colors not-interpolated)
;;       (let ((u32 (aref munsell-inversion-data hex)))
;; 	(when (= u32 +maxu32+)
;; 	  (destructuring-bind (r g b) (clcl:hex-to-rgb255 hex)
;; 	    (let ((u32-neighbor1 (aref original-mid
;; 				       (clcl:rgb255-to-hex r g (clcl:rgb1+ b)))))
;; 	      (if (not (= u32-neighbor1 +maxu32+))
;; 		  (setf (aref munsell-inversion-data hex)
;; 			(set-interpolated u32-neighbor1))
;; 		  (let ((u32-neighbor2 (aref original-mid
;; 					     (clcl:rgb255-to-hex r g (clcl:rgb1- b)))))
;; 		    (if (not (= u32-neighbor2 +maxu32+))
;; 			(setf (aref munsell-inversion-data hex)
;; 			      (set-interpolated u32-neighbor2))
;; 			(let ((u32-neighbor3 (aref original-mid
;; 						   (clcl:rgb255-to-hex r (clcl:rgb1+ g) b))))
;; 			  (if (not (= u32-neighbor3 +maxu32+))
;; 			      (setf (aref munsell-inversion-data hex)
;; 				    (set-interpolated u32-neighbor3))
;; 			      (let ((u32-neighbor4 (aref original-mid
;; 							 (clcl:rgb255-to-hex r (clcl:rgb1- g) b))))
;; 				(if (not (= u32-neighbor4 +maxu32+))
;; 				    (setf (aref munsell-inversion-data hex)
;; 					  (set-interpolated u32-neighbor4))
;; 				    (let ((u32-neighbor5 (aref original-mid
;; 							       (clcl:rgb255-to-hex (clcl:rgb1+ r) g b))))
;; 				      (if (not (= u32-neighbor5 +maxu32+))
;; 					  (setf (aref munsell-inversion-data hex)
;; 						(set-interpolated u32-neighbor5))
;; 					  (let ((u32-neighbor6 (aref original-mid
;; 								     (clcl:rgb255-to-hex (clcl:rgb1- r) g b))))
;; 					    (if (not (= u32-neighbor6 +maxu32+))
;; 						(setf (aref munsell-inversion-data hex)
;; 						      (set-interpolated u32-neighbor6))
;;						(incf not-interpolated)))))))))))))))))))

(defun find-least-score-rec (testfunc lst l-score l-node)
  (if (null lst)
      l-node
      (let ((score (funcall testfunc (car lst))))
	(if (< score l-score)
	    (find-least-score-rec testfunc (cdr lst) score (car lst))
	    (find-least-score-rec testfunc (cdr lst) l-score l-node)))))

;; return a node where the testfunc gives the minimum value.
(defun find-least-score (testfunc lst)
  (find-least-score-rec testfunc
			lst
			(funcall testfunc (car lst))
			(car lst)))

(defun interpolate-once (munsell-inversion-data)
  (let ((source-mid (copy-seq munsell-inversion-data))
	(not-interpolated 0))
    (dotimes (hex possible-colors not-interpolated)
      (let ((u32 (aref source-mid hex)))
	(when (= u32 +maxu32+)
	  (destructuring-bind (r g b) (clcl:hex-to-rgb255 hex)
	    (destructuring-bind (x y z) (clcl:rgb255-to-xyz r g b)
	      (let ((neighbors
		     (list (list r g (clcl:rgb1+ b))
			   (list r g (clcl:rgb1- b))
			   (list r (clcl:rgb1+ g) b)
			   (list r (clcl:rgb1- g) b)
			   (list (clcl:rgb1+ r) g b)
			   (list (clcl:rgb1- r) g b))))
		(let ((nearest-hex
		       (apply #'clcl:rgb255-to-hex
			(find-least-score
			 #'(lambda (n-rgb255)
			     (let* ((n-hex (apply #'clcl:rgb255-to-hex n-rgb255))
				    (n-u32 (aref source-mid n-hex)))
			       (if (= n-u32 +maxu32+)
				   most-positive-double-float
				   (destructuring-bind (n-x n-y n-z)
				       (apply #'clcl::munsell-hvc-to-xyz (decode-munsell-hvc n-u32))
				     (clcl:xyz-deltae x y z n-x n-y n-z)))))
			 neighbors))))
		  (if (= (aref source-mid nearest-hex) +maxu32+)
		      (incf not-interpolated)
		      (setf (aref munsell-inversion-data hex)
			    (set-interpolated (aref source-mid nearest-hex)))))))))))))



(defun interpolate-munsell-inversion-data (munsell-inversion-data)
  (let ((i 0))
    (loop
       (let ((remaining (interpolate-once munsell-inversion-data)))
	 (if (zerop remaining)
	     (progn
	       (format t "Loop: ~a: Perfectly interpolated.~%" (incf i))
	       (return))
	     (format t "Loop: ~a: Remaining nodes = ~A~%" (incf i) remaining))))))

; set value by y-to-munsell-value in MID. Thereby chroma is properly corrected.
(defun set-atsm-value (munsell-inversion-data)
  (dotimes (hex possible-colors)
    (destructuring-bind (h1000 nil c500)
	(decode-munsell-hvc1000 (aref munsell-inversion-data hex))
      (let* ((hue40 (clamp (/ h1000 25.0) 0 40))
	     (new-value (y-to-munsell-value (second (apply (rcurry #'bradford clcl:d65 clcl:c)
							(apply #'rgb255-to-xyz
							       (hex-to-rgb255 hex))))))
	     (chroma (* c500 0.1))
	     (v1000-new (round (* new-value 100)))
	     (c500-new (round (* (min (max-chroma hue40 new-value) chroma) 10))))
	(setf (aref munsell-inversion-data hex) (encode-munsell-hvc1000 h1000 v1000-new c500-new))))))


;; save/load Munsell inversion data to/from a binary file with big endian

(defun save-munsell-inversion-data (munsell-inversion-data &optional (filename "srgbd65-to-munsell-be.dat"))
  (let ((path (merge-pathnames (asdf:system-source-directory :clcl) filename)))
    (with-open-file (out path
			 :direction :output
			 :element-type '(unsigned-byte 8)
			 :if-exists :supersede)
      (dotimes (x possible-colors)
	(nibbles:write-ub32/be (aref munsell-inversion-data x) out))
      (format t "Munsell inversion data is saved in ~A.~%" path))))

(defun load-munsell-inversion-data (&optional (filename "srgbd65-to-munsell-be.dat"))
  (let ((path (merge-pathnames (asdf:system-source-directory :clcl) filename)))
    (with-open-file (in path
			:direction :input
			:element-type '(unsigned-byte 8))
      (let ((munsell-inversion-data (make-array possible-colors :element-type '(unsigned-byte 32) :initial-element +maxu32+)))
	(dotimes (x possible-colors munsell-inversion-data)
	  (setf (aref munsell-inversion-data x) (nibbles:read-ub32/be in)))))))

(defun check-data-from-srgb (munsell-inversion-data r g b)
  (let ((u32 (aref munsell-inversion-data (clcl:rgb255-to-hex r g b))))
    (if (= u32 +maxu32+)
	nil
	(apply #'clcl:munsell-hvc-to-rgb255 (decode-munsell-hvc u32)))))

(defun check-all-data (munsell-inversion-data)
  (dotimes (x possible-colors)
    (let* ((srgb (clcl:hex-to-rgb255 x))
	   (srgb2 (apply #'check-data-from-srgb (append (list munsell-inversion-data) srgb))))
      (unless (null srgb2)
	(when (not (equal srgb srgb2))
	  (format t "inacurrate value at position: ~a" x))))))
	  

;; sRGB d65 to munsell HVC	  
(defun rgb255-to-munsell-hvc (r g b munsell-inversion-data)
  (decode-munsell-hvc (aref munsell-inversion-data (clcl:rgb255-to-hex r g b))))


;; one-in-all function
;; (defun generate-all (&key (filename "srgbd65-to-munsell-be.dat") (with-interpolate t))
;;   (time
;;    (progn
;;      (format t "generating Munsell inversion data...~%")
;;      (make-munsell-inversion-data)
;;      ;; (format t "checking the reliability of the data...~%")
;;      ;; (check-all-data)
;;      (when with-interpolate
;;        (format t "interpolating the Munsell inversion data...~%")
;;        (interpolate-munsell-inversion-data))
;;      (format t "save data to ~a.~%" filename)
;;      (save-dat-file filename))))

(defun build-mid (&optional (filename "srgbd65-to-munsell-be.dat") (with-interpolation t))
  (format t "generating Munsell inversion data...~%")
  (let ((mid (make-munsell-inversion-data with-interpolation)))
    (save-munsell-inversion-data mid filename)))

(defun count-gaps (munsell-inversion-data)
  (let ((gaps 0))
    (dotimes (hex possible-colors)
      (when (= +maxu32+ (aref munsell-inversion-data hex))
	  (incf gaps)))
    gaps))
  
(defun gap-rate-b (munsell-inversion-data)
  (let ((gaps-sum 0))
    (dotimes (b 256)
     (let ((gaps 0))
       (dotimes (r 256)
	 (dotimes (g 256)
	   (if (= +maxu32+ (aref munsell-inversion-data (clcl:rgb255-to-hex r g b)))
	       (incf gaps))))
       (format t "b = ~a, gap rate = ~a~%" b (/ gaps 65536.0))
       (setf gaps-sum (+ gaps-sum gaps))))
    (format t "total gap rate = ~a~%" (/ gaps-sum (float possible-colors)))))


(defun gap-rate-by-flag (munsell-inversion-data)
  (let ((gaps-sum 0))
    (dotimes (b 256)
     (let ((gaps 0))
       (dotimes (r 256)
	 (dotimes (g 256)
	   (if (interpolatedp (aref munsell-inversion-data (clcl:rgb255-to-hex r g b)))
	       (incf gaps))))
       (format t "b = ~a, gap rate = ~a~%" b (/ gaps 65536.0))
       (setf gaps-sum (+ gaps-sum gaps))))
    (format t "total gap rate = ~a~%" (/ gaps-sum (float possible-colors)))))

(defun gap-rate-by-brightness (munsell-inversion-data)
  (let ((gaps-sum 0))
    (loop for brightness-sum from 0 to 765 do
	 (let ((gaps 0)
	       (number-of-colors 0)
	       (max-r (min 255 brightness-sum)))
	   (loop for r from 0 to max-r do
		(let ((min-g (max 0 (- brightness-sum 255 r)))
		      (max-g (min 255 (- brightness-sum r))))
		  (loop for g from min-g to max-g do
		       (let ((min-b (max 0 (- brightness-sum r g)))
			     (max-b (min 255 (- brightness-sum r g))))
			 (loop for b from min-b to max-b do
			      (incf number-of-colors)
			      (when (= +maxu32+ (aref munsell-inversion-data (clcl:rgb255-to-hex r g b)))
				(incf gaps)
				(incf gaps-sum)))))))
	   (format t "brightness = ~a, gap rate = ~a (= ~a / ~a).~%"
		   brightness-sum
		   (/ (float gaps) number-of-colors) 
		   gaps
		   number-of-colors)))
    (format t "total gap rate = ~a~%" (/ gaps-sum (float possible-colors)))))
    

;; examine the total error of interpolated data in MID	
(defun examine-interpolation-error (munsell-inversion-data &optional (start 0) (end possible-colors))
  (let ((maximum 0)
	(worst-hex nil)
	(sum 0)
	(nodes 0))
    (loop for hex from start below end do
      (let ((u32 (aref munsell-inversion-data hex)))
	(if (interpolatedp u32)
	    (destructuring-bind  (r1 g1 b1) (clcl:hex-to-rgb255 hex)
	      (destructuring-bind (r2 g2 b2) (apply #'clcl::munsell-hvc-to-rgb255 (decode-munsell-hvc u32))
		(let ((delta (clcl:rgb255-deltae r1 g1 b1 r2 g2 b2)))
		  (setf sum (+ sum delta))
		  (when (> delta maximum)
		    (setf maximum delta)
		    (setf worst-hex hex))
		  (incf nodes)))))))
    (format t "Number of Interpolated Nodes = ~A (~,3F%)~%" nodes (* 100d0 (/ nodes (- end start))))
    (format t "Mean Color Difference: ~a~%" (/ sum nodes))
    (format t "Maximum Color Difference: ~a at hex ~a~%" maximum worst-hex)))

;;; xyY interpolation version
;; Number of Interpolated Nodes = 3729095 (22.227%)
;; Mean Color Difference: 0.3134200498636899d0
;; Maximum Color Difference: 8.859190406312553d0 at hex 19198

;;; LCH(ab) version
;; (clcl::examine-interpolation-error mid)
;; Number of Interpolated Nodes = 3716977 (22.155%)
;; Mean Color Difference: 0.32306184556274486d0
;; Maximum Color Difference: 10.38150980126532d0 at hex 17908

;; (clcl::examine-interpolation-error mid #x282828)
;; Number of Interpolated Nodes = 2683082 (17.965%)
;; Mean Color Difference: 0.281657302033876d0
;; Maximum Color Difference: 5.651441223834461d0 at hex 4269568

;; get the maximun radius of the spheres of missing values in the non-interpolated munsell inversion data.
(defun get-radius-of-blank-sphere (mid depth r g b)
  (if (not (= +maxu32+ (aref mid (clcl:rgb255-to-hex r g b))))
      depth
      (max (get-radius-of-blank-sphere mid (1+ depth) r g (clcl:rgb1+ b))
	   (get-radius-of-blank-sphere mid (1+ depth) r g (clcl:rgb1- b))
	   (get-radius-of-blank-sphere mid (1+ depth) r (clcl:rgb1+ g) b)
	   (get-radius-of-blank-sphere mid (1+ depth) r (clcl:rgb1- g) b)
	   (get-radius-of-blank-sphere mid (1+ depth) (clcl:rgb1+ r) g b)
	   (get-radius-of-blank-sphere mid (1+ depth) (clcl:rgb1- r) g b))))

(defun maximum-radius-of-blank-sphere (mid)
  (let ((maximum 0))
    (dotimes (hex possible-colors maximum)
      (when (= (mod hex 10000) 0)
	(format t "~a / ~a hues were processed." hex possible-colors))
      (let ((rad (apply #'get-radius-of-blank-sphere mid 0 (clcl:hex-to-rgb255 hex))))
	(if (> rad maximum)
	    (setf maximum rad))))))



;; (defun find-value-in-mrd (value)
;;   (let ((xyy-lst nil))
;;     (dolist (line munsell-renotation-data xyy-lst)
;;       (when (= (second line) value)
;; 	(push (cdddr line) xyy-lst)))))


;; (defun find-value-in-general (value)
;;   (let ((xyy-lst nil))
;;     (dotimes (hue40 40 xyy-lst)
;;       (let ((max-c (clcl:max-chroma hue40 value)))
;; 	(dotimes (chroma max-c)
;; 	  (push (clcl:munsell-hvc-to-xyy hue40 value chroma)
;; 		xyy-lst))))))

(defun test-blue (lb)
  (apply #'xyz-to-lchab (lrgb-to-xyz 0 0 lb)))
