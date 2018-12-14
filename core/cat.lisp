;;;
;;; LMS and chromatic adaptation transformation
;;;

(uiop:define-package :dufy/core/cat
  (:use :cl :dufy/internal/* :dufy/core/spectrum :dufy/core/illuminants-data :dufy/core/xyz :dufy/core/rgb)
  (:export #:lms
           #:cat
           #:make-cat
           #:cat-matrix
           #:cat-inv-matrix
           #:xyz-to-lms
           #:lms-to-xyz
           #:+bradford+
           #:+xyz-scaling+
           #:+von-kries+
           #:+cmccat97+
           #:+cmccat2000+
           #:+cat97s-revised+
           #:+cat02+

           #:gen-cat-function
           #:define-cat-function
           #:gen-rgbspace-changer
           #:copy-rgbspace))

(in-package :dufy/core/cat)

(defstruct (cat (:constructor %make-cat
                  (matrix &aux (inv-matrix (invert-matrix matrix)))))
  "Expresses a model of chromatic adaptation transformation. Currently
only linear models are available."
  (matrix +empty-matrix+ :type matrix33)
  (inv-matrix +empty-matrix+ :type matrix33))

(defun make-cat (mat)
  "Generates a (linear) CAT model from a 3*3 matrix."
  (%make-cat
   (etypecase mat
     (matrix33 mat)
     (sequence (make-array '(3 3)
                           :element-type 'double-float
                           :initial-contents mat))
     (array (let ((coerced-mat (make-array '(3 3) :element-type 'double-float)))
              (dotimes-unroll (i 3)
                (dotimes-unroll (j 3)
                  (setf (aref coerced-mat i j)
                        (coerce (aref mat i j) 'double-float))))
              coerced-mat)))))

(defparameter +bradford+
  (make-cat #2a((0.8951d0 0.2664d0 -0.1614d0)
                (-0.7502d0 1.7135d0 0.0367d0)
                (0.0389d0 -0.0685d0 1.0296d0))))

(defparameter +xyz-scaling+
  (make-cat #2a((1d0 0d0 0d0)
                (0d0 1d0 0d0)
                (0d0 0d0 1d0))))

(defparameter +von-kries+
  (make-cat #2a((0.4002d0 0.7076d0 -0.0808d0)
                (-0.2263d0 1.1653d0 0.0457d0)
                (0.0000d0 0.0000d0 0.9182d0))))

(defparameter +cmccat97+
  (make-cat #2a((0.8951d0 -0.7502d0 0.0389d0)
                (0.2664d0 1.7135d0 0.0685d0)
                (-0.1614d0 0.0367d0 1.0296d0))))

(defparameter +cmccat2000+
  (make-cat #2a((0.7982d0 0.3389d0 -0.1371d0)
                (-0.5918d0 1.5512d0 0.0406d0)
                (0.0008d0 0.0239d0 0.9753d0))))

(defparameter +cat97s-revised+
  (make-cat #2a((0.8562d0 0.3372d0 -0.1934d0)
                (-0.8360d0 1.8327d0 0.0033d0)
                (0.0357d0 -0.0469d0 1.0112d0)))
  "Fairchild, Mark D. (2001).\"A Revision of CIECAM97s for Practical
Applications\" http://rit-mcsl.org/fairchild//PDFs/PAP10.pdf")

(defparameter +cat02+
  (make-cat #2a((0.7328d0 0.4296d0 -0.1624d0)
                (-0.7036d0 1.6975d0 0.0061d0)
                (0.0030d0 0.0136d0 0.9834d0)))
  "Note that the CAT function returned by (gen-cat-function ... :cat +cat02+) is
  different from the one in CIECAM02 since the latter is non-linear.")

(define-colorspace lms (l m s)
  :arg-types (real real real)
  :return-types (double-float double-float double-float))

(define-primary-converter (xyz lms) (x y z &key (illuminant +illum-d65+) (cat +bradford+))
  (declare (optimize (speed 3) (safety 1)))
  "If ILLUMINANT is NIL, the transform is virtually equivalent to that
of illuminant E. "
  (with-ensuring-type double-float (x y z)
    (if illuminant
        (let* ((mat (cat-matrix cat))
               (factor-l (+ (* (illuminant-x illuminant) (aref mat 0 0))
                            (aref mat 0 1)
                            (* (illuminant-z illuminant) (aref mat 0 2))))
               (factor-m (+ (* (illuminant-x illuminant) (aref mat 1 0))
                            (aref mat 1 1)
                            (* (illuminant-z illuminant) (aref mat 1 2))))
               (factor-s (+ (* (illuminant-x illuminant) (aref mat 2 0))
                            (aref mat 2 1)
                            (* (illuminant-z illuminant) (aref mat 2 2)))))
          (multiple-value-bind (l m s)
              (multiply-mat-vec mat x y z)
            (values (/ l factor-l)
                    (/ m factor-m)
                    (/ s factor-s))))
        (multiply-mat-vec (cat-matrix cat) x y z))))

(define-primary-converter (lms xyz) (l m s &key (illuminant +illum-d65+) (cat +bradford+))
  (declare (optimize (speed 3) (safety 1)))
  "If ILLUMINANT is NIL, the transform is virtually equivalent to that
of illuminant E. "
  (with-ensuring-type double-float (l m s)
    (if illuminant
        (let* ((mat (cat-matrix cat))
               (factor-l (+ (* (illuminant-x illuminant) (aref mat 0 0))
                            (aref mat 0 1)
                            (* (illuminant-z illuminant) (aref mat 0 2))))
               (factor-m (+ (* (illuminant-x illuminant) (aref mat 1 0))
                            (aref mat 1 1)
                            (* (illuminant-z illuminant) (aref mat 1 2))))
               (factor-s (+ (* (illuminant-x illuminant) (aref mat 2 0))
                            (aref mat 2 1)
                            (* (illuminant-z illuminant) (aref mat 2 2)))))
          (multiply-mat-vec (cat-inv-matrix cat)
                            (* l factor-l)
                            (* m factor-m)
                            (* s factor-s)))
        (multiply-mat-vec (cat-inv-matrix cat) l m s))))
  
(defun calc-cat-matrix  (from-illuminant to-illuminant &optional (cat +bradford+))
  "Returns a 3*3 chromatic adaptation matrix between FROM-ILLUMINANT
and TO-ILLUMINANT in XYZ space."
  (declare (optimize (speed 3) (safety 1)))
  (let ((tmatrix (cat-matrix cat)))
    (multiple-value-bind (source-L source-M source-S)
        (multiply-mat-vec tmatrix
                          (illuminant-x from-illuminant)
                          1d0
                          (illuminant-z from-illuminant))
      (multiple-value-bind (dest-L dest-M dest-S)
          (multiply-mat-vec tmatrix
                            (illuminant-x to-illuminant)
                            1d0
                            (illuminant-z to-illuminant))
        (let ((L-ratio (/ dest-L source-L))
              (M-ratio (/ dest-M source-M))
              (S-ratio (/ dest-S source-S))
              (matrix1 (make-array '(3 3) :element-type 'double-float)))
          (declare (dynamic-extent matrix1))
          (setf (aref matrix1 0 0) (* L-ratio (aref tmatrix 0 0)))
          (setf (aref matrix1 0 1) (* L-ratio (aref tmatrix 0 1)))
          (setf (aref matrix1 0 2) (* L-ratio (aref tmatrix 0 2)))
          (setf (aref matrix1 1 0) (* M-ratio (aref tmatrix 1 0)))
          (setf (aref matrix1 1 1) (* M-ratio (aref tmatrix 1 1)))
          (setf (aref matrix1 1 2) (* M-ratio (aref tmatrix 1 2)))
          (setf (aref matrix1 2 0) (* S-ratio (aref tmatrix 2 0)))
          (setf (aref matrix1 2 1) (* S-ratio (aref tmatrix 2 1)))
          (setf (aref matrix1 2 2) (* S-ratio (aref tmatrix 2 2)))
          (multiply-mat-mat (cat-inv-matrix cat)
                            matrix1))))))

(declaim (ftype (function * (function * (values double-float double-float double-float &optional))) gen-cat-function))
(defun gen-cat-function (from-illuminant to-illuminant &key (cat +bradford+))
  "Returns a chromatic adaptation function.
 (funcall (gen-cat-function +illum-a+ +illum-e+) 0.9504d0 1.0d0 1.0889d0)
=> 0.9999700272441295d0
0.999998887365445d0
0.9999997282885571d0 ; transformed white point"
  (declare (optimize (speed 3) (safety 1)))
  (let ((mat (calc-cat-matrix from-illuminant to-illuminant cat)))
    #'(lambda (x y z)
        (with-ensuring-type double-float (x y z)
          (multiply-mat-vec mat x y z)))))

(defmacro define-cat-function (name from-illuminant to-illuminant &key (cat '+bradford+))
  "DEFINE-macro of GEN-CAT-FUNCTION.
 (define-cat-function d65-to-e +illum-d65+ +illum-e+)
 (d65-to-e 0.9504d0 1.0d0 1.0889d0)
;; => 0.9999700272441295d0
;; 0.999998887365445d0
;; 0.9999997282885571d0"
  (unless (and (symbolp from-illuminant)
               (symbolp to-illuminant))
    (error "FROM-ILLUMINANT and TO-ILLUMINANT must be symbols"))
  `(progn
     (declaim (inline ,name)
              (ftype (function * (values double-float double-float double-float &optional))
                     ,name))
     (defun ,name (x y z)
       (declare (optimize (speed 3) (safety 1)))
       (let ((mat (load-time-value
                   (calc-cat-matrix ,from-illuminant ,to-illuminant ,cat)
                   t)))
         (with-ensuring-type double-float (x y z)
           (multiply-mat-vec mat x y z))))))

(declaim (inline calc-cat-matrix-for-lrgb))
(defun calc-cat-matrix-for-lrgb (from-rgbspace to-rgbspace &optional (cat +bradford+))
  "Linear transformation: LRGB -> XYZ -> XYZ -> LRGB"
  (multiply-matrices (rgbspace-from-xyz-matrix to-rgbspace)
                     (calc-cat-matrix (rgbspace-illuminant from-rgbspace)
                                      (rgbspace-illuminant to-rgbspace)
                                      cat)
                     (rgbspace-to-xyz-matrix from-rgbspace)))

(defun gen-rgbspace-changer (from-rgbspace to-rgbspace &key (target :lrgb) (cat +bradford+))
  "Returns a function for changing RGB working space.

 (funcall (gen-rgbspace-changer +srgb+ +adobe+ :target :rgb) 0 1 0)
;; => 0.5649506908657044d0
;; 1.0d0
;; 0.2344342037422755d0
;; change from sRGB to Adobe RGB.

TARGET ::= :LRGB | :RGB | :QRGB | :RGBPACK

Note about clamping:
LRGB case: no clamping;
RGB case: no clamping;
QRGB case: with clamping;
RGBPACK case: with clamping."
  (declare (optimize (speed 3) (safety 1)))
  (let ((mat (calc-cat-matrix-for-lrgb from-rgbspace to-rgbspace cat)))
    (ecase target
      (:lrgb #'(lambda (lr lg lb)
                 (multiply-mat-vec mat (float lr 1d0) (float lg 1d0) (float lb 1d0))))
      (:rgb #'(lambda (r g b)
                (multiple-value-call #'lrgb-to-rgb
                  (multiple-value-call #'multiply-mat-vec
                    mat
                    (rgb-to-lrgb (float r 1d0) (float g 1d0) (float b 1d0)
                                 :rgbspace from-rgbspace))
                  :rgbspace to-rgbspace)))
      (:qrgb #'(lambda (qr qg qb)
                 (multiple-value-call #'lrgb-to-qrgb
                   (multiple-value-call #'multiply-mat-vec
                     mat
                     (qrgb-to-lrgb qr qg qb :rgbspace from-rgbspace))
                   :rgbspace to-rgbspace)))
      (:rgbpack #'(lambda (int)
                (multiple-value-call #'lrgb-to-rgbpack
                  (multiple-value-call #'multiply-mat-vec
                    mat
                    (rgbpack-to-lrgb int :rgbspace from-rgbspace))
                  :rgbspace to-rgbspace))))))

;; I have moved this function here as it requires GEN-CAT-FUNCTION.
(defun copy-rgbspace (rgbspace &key (illuminant nil) (bit-per-channel nil) (cat +bradford+))
  "Returns a new RGBSPACE with different standard illuminant and/or
bit-per-channel. All the parameters are properly recalculated. If both
are nil, it is just a copier."
  (destructuring-bind (new-xr new-yr new-xg new-yg new-xb new-yb)
      (if illuminant
          (let ((ca-func (gen-cat-function (rgbspace-illuminant rgbspace)
                                           illuminant
                                           :cat cat)))
            (labels ((get-new-xy (r g b)
                       (multiple-value-bind (small-x small-y y)
                           (multiple-value-call #'xyz-to-xyy
                             (multiple-value-call ca-func
                               (lrgb-to-xyz r g b :rgbspace rgbspace)))
                         (declare (ignore y))
                         (list small-x small-y))))
              (append (get-new-xy 1 0 0)
                      (get-new-xy 0 1 0)
                      (get-new-xy 0 0 1))))
          (list (rgbspace-xr rgbspace) (rgbspace-yr rgbspace)
                (rgbspace-xg rgbspace) (rgbspace-yg rgbspace)
                (rgbspace-xb rgbspace) (rgbspace-yb rgbspace)))
    (make-rgbspace new-xr new-yr new-xg new-yg new-xb new-yb
                   :illuminant (or illuminant (rgbspace-illuminant rgbspace))
                   :linearizer (rgbspace-linearizer rgbspace)
                   :delinearizer (rgbspace-delinearizer rgbspace)
                   :lmin (rgbspace-lmin rgbspace)
                   :lmax (rgbspace-lmax rgbspace)
                   :bit-per-channel (or bit-per-channel (rgbspace-bit-per-channel rgbspace))
                   :force-normal (rgbspace-normal rgbspace))))
