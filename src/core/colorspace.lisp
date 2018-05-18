;;;
;;; Meta-definition of color space
;;; (currently not used)
;;;

(in-package :dufy-core)

(defstruct colorspace
  "illuminant::= illuminant | rgbspace | nil
clamp::= :always-clamped | :clampable | nil
"
  (term nil :type symbol)
  (args nil :type list)
  (arg-types nil :type list)
  (illuminant 'illuminant :type symbol)
  (clamp nil :type symbol)
  (neighbors nil :type list))

(defparameter *colorspace-table* (make-hash-table))

(defun colorspace= (space1 space2)
  (eql (colorspace-term space1)
       (colorspace-term space2)))

(defmacro define-colorspace (term args &key (illuminant :illuminant) clamp)
  (let ((key (make-keyword term)))
    `(setf (gethash ',key *colorspace-table*)
           (make-colorspace :term ',(make-keyword key)
                            :args ',(mapcar #'(lambda (x)
                                                (if (consp x)
                                                    (first x)
                                                    x))
                                            args)
                            :arg-types ',(mapcar #'(lambda (x)
                                                     (if (consp x)
                                                         (second x)
                                                         t))
                                                 args)
                            :illuminant ,illuminant
                            :clamp ,clamp
                            :neighbors nil))))

(defun get-colorspace (term)
  (gethash term *colorspace-table*))
(defun get-neighbors (term)
  (colorspace-neighbors (gethash term *colorspace-table*)))
(defun get-args (term)
  (colorspace-args (gethash term *colorspace-table*)))
(defun get-arg-types (term)
  (colorspace-arg-types (gethash term *colorspace-table*)))
(defun get-clamp (term)
  (colorspace-clamp (gethash term *colorspace-table*)))
(defun get-illuminant (term)
  (colorspace-illuminant (gethash term *colorspace-table*)))

(defun print-colorspace-table ()
  (format t "#<HASH-TABLE ~%")
  (maphash-values #'(lambda (val) (format t "~S~%" val))
                  *colorspace-table*)
  (format t ">~%"))

(defparameter *primary-converter-table* (make-hash-table :test 'equal))

(defun gen-converter-name (from-term to-term)
  (intern (format nil "~A-TO-~A" from-term to-term) *package*))

(defun get-key-args (lambda-list)
  (mapcar #'(lambda (x) (make-keyword (if (consp x)
                                          (first x)
                                          x)))
          (cdr (member '&key lambda-list))))

(defstruct (primary-converter (:constructor make-primary-converter
                                  (from-term to-term lambda-list
                                   &aux (key-args (get-key-args lambda-list))
                                     (fsymbol (gen-converter-name from-term to-term)))))
  (from-term nil :type symbol)
  (to-term nil :type symbol)
  (fsymbol nil :type symbol)
  (lambda-list nil :type list)
  (key-args nil :type list))

(defun add-primary-converter (from-term to-term lambda-list)
  (let ((from-space (get-colorspace from-term))
        (to-space (get-colorspace to-term)))
    (assert (and from-space to-space))
    (pushnew to-term (colorspace-neighbors from-space))
    (setf (gethash (list from-term to-term) *primary-converter-table*)
          (make-primary-converter from-term to-term lambda-list))))

(defun get-primary-converter (from-term to-term)
  (gethash (list from-term to-term) *primary-converter-table*))
(defun get-primary-converter-fsymbol (from-term to-term)
  (primary-converter-fsymbol (gethash (list from-term to-term)
                                      *primary-converter-table*)))

(defun print-primary-converter-table ()
  (format t "#<HASH-TABLE ~%")
  (maphash-values #'(lambda (val) (format t "~S~%" val))
                  *primary-converter-table*)
  (format t ">~%"))

(defstruct queue list tail)
(defun enqueue (obj queue)
  (with-slots (list tail) queue
    (if (null list)
        (setf tail (list obj)
              list tail)
        (setf (cdr tail) (list obj)
              tail (cdr tail))))
  queue)
(defun dequeue (queue)
  (pop (queue-list queue)))

(defun get-converter-chain (begin-term dest-term)
  "Returns the shortest path in the converter graph with BFS."
  (let ((visited (make-hash-table))
        (path-q (enqueue (list begin-term) (make-queue))))
    (loop for path = (dequeue path-q)
          for current-term = (car path)
          do (when (null current-term)
               (error "No route found: from ~S to ~S" begin-term dest-term))
             (if (eql current-term dest-term)
                 (return (reverse path))
                 (unless (nth-value 1 (ensure-gethash current-term visited t))
                   (dolist (term (get-neighbors current-term))
                     (enqueue (cons term path) path-q)))))))


(define-colorspace xyz ((x double-float)
                        (y double-float)
                        (z double-float)))
(define-colorspace xyy ((small-x double-float)
                        (small-y double-float)
                        (y double-float)))
(define-colorspace lab ((lstar double-float)
                        (astar double-float)
                        (bstar double-float)))
(define-colorspace lchab ((lstar double-foat)
                          (cstarab double-float)
                          (hab double-float)))
(define-colorspace luv ((lstar double-float)
                        (ustar double-float)
                        (vstar double-float)))
(define-colorspace lchuv ((lstar double-float)
                          (cstaruv double-float)
                          (huv double-float)))
(define-colorspace rgb ((r double-float)
                        (g double-float)
                        (b double-float))
  :illuminant :rgbspace)
(define-colorspace lrgb ((lr double-float)
                         (lg double-float)
                         (lb double-float))
  :illuminant :rgbspace)
(define-colorspace qrgb ((qr integer)
                         (qg integer)
                         (qb integer))
  :illuminant :rgbspace :clamp :clampable)
(define-colorspace int ((int integer))
  :illuminant :rgbspace :clamp :always-clamped)
(define-colorspace hsv ((hue double-float)
                        (sat double-float)
                        (val double-float))
  :illuminant :rgbspace)
(define-colorspace hsl ((hue double-float)
                        (sat double-float)
                        (lum double-float))
  :illuminant :rgbspace)
(define-colorspace spectrum ((spectrum spectrum-function)))
(define-colorspace lms ((l double-float)
                        (m double-float)
                        (s double-float)))

;; (add-primary-converter :xyz :xyy)
;; (add-primary-converter :xyz :lms)
;; (add-primary-converter :xyz :spectrum)
;; (add-primary-converter :xyz :lrgb)
;; (add-primary-converter :lrgb :rgb)
;; (add-primary-converter :rgb :qrgb)
;; (add-primary-converter :qrgb :int)
;; (add-primary-converter :xyz :lab)
;; (add-primary-converter :lab :lchab)
;; (add-primary-converter :xyz :luv)
;; (add-primary-converter :luv :lchuv)
;; (add-primary-converter :hsv :rgb)
;; (add-primary-converter :hsl :rgb)


(defmacro define-primary-converter (begin-term dest-term args &body body)
  "Defines FOO-TOO-BAR function as a primary converter."
  (assert (and (symbolp begin-term) (symbolp dest-term) (listp args)))
  (let* ((begin-term (make-keyword begin-term))
         (dest-term (make-keyword dest-term))
         (fname (gen-converter-name begin-term dest-term)))
    `(progn
       (declaim (inline ,fname)
                (ftype (function * (values ,@(get-arg-types dest-term) &optional)) ,fname))
       (defun ,fname ,(append (get-args begin-term) args)
         ,@body)
       (add-primary-converter ,begin-term ,dest-term
                              ',args))))


(defun converter-clamp-p (term1 term2)
  (and (not (eql (get-clamp term1) :always-clamped))
       (eql (get-clamp term2) :clampable)))

(defun global-clamp-p (terms)
  (converter-clamp-p (car (last terms 2))
                     (car (last terms))))
(defun get-local-illuminant-key (term1 term2)
  (let* ((conv (get-primary-converter term1 term2))
         (key-args (primary-converter-key-args conv)))
    (or (find :rgbspace key-args)
        (find :illuminant key-args))))

(defun get-global-illuminant-key (terms)
  (let ((illum-keys (loop for (term1 term2) on terms
                          until (null term2)
                          collect (get-local-illuminant-key term1 term2))))
    (or (find :rgbspace illum-keys)
        (find :illuminant illum-keys))))

(defun need-rgbspace-to-illuminant-p (terms)
  (let ((illum-keys (loop for (term1 term2) on terms
                          until (null term2)
                          collect (get-local-illuminant-key term1 term2))))
    (and (find :rgbspace illum-keys)
         (find :illuminant illum-keys))))


(defun sane-symbol (symb)
  (intern (symbol-name symb) *package*))

(defun gen-global-key-args (terms)
  (remove nil
          (list
           (let ((illum-key (get-global-illuminant-key terms)))
             (case illum-key
               (:rgbspace (list (sane-symbol illum-key) '+srgb+))
               (:illuminant (list (sane-symbol illum-key) '+illum-d65+))))
           (when (global-clamp-p terms)
             (list (sane-symbol :clamp) t)))))

(defun gen-last-key-args (terms)
  (when (global-clamp-p terms)
    (list :clamp (sane-symbol :clamp))))

(defun expand-key-args (arg-lst)
  (mappend #'(lambda (key)
               (list key (sane-symbol key)))
           arg-lst))

(defun gen-local-key-args (term1 term2)
  (expand-key-args
   (remove nil
           (list (get-local-illuminant-key term1 term2)))))

(defmacro defconverter (begin-term dest-term &key (fname (gen-converter-name begin-term dest-term)))
  (let* ((begin-term (make-keyword begin-term))
         (dest-term (make-keyword dest-term))
         (global-fname fname)
         (chain (get-converter-chain begin-term dest-term))
         (global-key-args (gen-global-key-args chain))
         (last-key-args (gen-last-key-args chain)))
    (assert (>= (length chain) 3)
            (chain)
            "The length of converters path is ~a. It should be greater than 3."
            (length chain))
    (labels ((expand (term-lst code)
               (if (null (cdr term-lst))
                   code
                   (let* ((term1 (first term-lst))
                          (term2 (second term-lst))
                          (name (get-primary-converter-fsymbol term1 term2)))
                     (cond ((null code)
                            (expand (cdr term-lst) ; first conversion
                                    `(,name ,@(get-args term1)
                                            ,@(gen-local-key-args term1 term2))))
                           ((eql term2 dest-term) ; last conversion
                            (expand (cdr term-lst)
                                    `(multiple-value-call #',name
                                       ,code
                                       ,@(gen-local-key-args term1 term2)
                                       ,@last-key-args)))
                           (t (expand (cdr term-lst) ; intermediate conversion
                                      `(multiple-value-call #',name
                                         ,code
                                         ,@(gen-local-key-args term1 term2)))))))))
      `(progn
         (declaim (inline ,global-fname)
                  (ftype (function * (values ,@(get-arg-types (lastcar chain)) &optional)) ,global-fname))
         (defun ,global-fname (,@(get-args begin-term)
                               &key ,@global-key-args)
           (declare (optimize (speed 3) (safety 1)))
           ,(if (need-rgbspace-to-illuminant-p chain)
                `(let ((,(sane-symbol 'illuminant)
                         (dufy-core:rgbspace-illuminant ,(sane-symbol 'rgbspace))))
                   (declare (ignorable ,(sane-symbol 'illuminant)))
                   ,(expand chain nil))
                (expand chain nil)))))))

;; (defconverter xyy lrgb)
