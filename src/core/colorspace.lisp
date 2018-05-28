;;;
;;; Meta-definition of color space
;;;

(in-package :dufy-core)

(defun sane-symbol (symb)
  (intern (symbol-name symb) *package*))

(defstruct colorspace
  (term nil :type symbol)
  (args nil :type list)
  (arg-types nil :type list)
  (clamp nil :type symbol)
  (neighbors nil :type list))

;; (defstruct colorspace
;;   "illuminant::= :illuminant | :rgbspace | [symbol of predefined illuminant]
;; clamp::= :always-clamped | :clampable | nil
;; "
;;   (term nil :type symbol)
;;   (args nil :type list)
;;   (arg-types nil :type list)
;;   (illuminant :illuminant :type symbol)
;;   (clamp nil :type symbol)
;;   (neighbors nil :type list))

(defparameter *colorspace-table* (make-hash-table))

(defun colorspace= (space1 space2)
  (eql (colorspace-term space1)
       (colorspace-term space2)))

(defmacro define-colorspace (term args &key clamp)
  (let ((key (make-keyword term)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,key *colorspace-table*)
             (make-colorspace :term ,key
                              :args ',(mapcar (compose #'first #'ensure-list)
                                              args)
                              :arg-types ',(mapcar (compose #'second #'ensure-list)
                                                   args)
                              :clamp ,clamp
                              :neighbors nil)))))

(defun get-colorspace (term)
  (gethash term *colorspace-table*))
(defun get-neighbors (term)
  (colorspace-neighbors (gethash term *colorspace-table*)))
(defun get-clamp (term)
  (colorspace-clamp (gethash term *colorspace-table*)))
(defun get-args (term &optional (package nil))
  (mapcar (if package
              #'(lambda (x) (intern (symbol-name x) package))
              #'identity)
          (colorspace-args (gethash term *colorspace-table*))))
(defun get-arg-types (term)
  (colorspace-arg-types (gethash term *colorspace-table*)))

(defun print-colorspace-table ()
  (format t "~%#<HASH-TABLE ~%")
  (maphash-values #'(lambda (val) (format t "~S~%" val))
                  *colorspace-table*)
  (format t ">"))



;;;
;;; Primary converter
;;;

(defun gen-converter-name (from-term to-term)
  (intern (format nil "~A-TO-~A" from-term to-term) *package*))

(defun extract-key-args-with-init (lambda-list)
  (cdr (member '&key lambda-list)))

(defun extract-key-args (lambda-list)
  (mapcar #'(lambda (x) (make-keyword (car (ensure-list x))))
          (extract-key-args-with-init lambda-list)))

(defstruct (primary-converter
            (:constructor make-primary-converter
                (from-term to-term lambda-list
                 &key
                   (name (gen-converter-name from-term to-term))
                   (forced-bindings nil)
                 &aux
                   (key-args-with-init (extract-key-args-with-init lambda-list))
                   (key-args (extract-key-args lambda-list)))))
  (from-term nil :type symbol)
  (to-term nil :type symbol)
  (name nil :type symbol)
  (key-args-with-init nil :type list)
  (key-args nil :type list)
  (forced-bindings nil :type list))

(defparameter *primary-converter-table* (make-hash-table :test 'equal))

(defun add-primary-converter (from-term to-term lambda-list &key (name (gen-converter-name from-term to-term)) (forced-bindings nil))
  (let ((from-space (get-colorspace from-term))
        (to-space (get-colorspace to-term)))
    (assert (and from-space to-space))
    (setf (gethash (list from-term to-term) *primary-converter-table*)
          (make-primary-converter from-term to-term lambda-list
                                  :name name
                                  :forced-bindings forced-bindings))
    (pushnew to-term (colorspace-neighbors from-space))))

(defun get-primary-converter (from-term to-term)
  (gethash (list from-term to-term) *primary-converter-table*))
(defun get-primary-converter-name (from-term to-term)
  (primary-converter-name (gethash (list from-term to-term)
                                    *primary-converter-table*)))
(defun get-key-args-with-init (from-term to-term)
  (primary-converter-key-args-with-init (gethash (list from-term to-term)
                                                 *primary-converter-table*)))
(defun get-key-args (from-term to-term)
  (primary-converter-key-args (gethash (list from-term to-term)
                                       *primary-converter-table*)))
(defun get-forced-bindings (from-term to-term)
  (primary-converter-forced-bindings (gethash (list from-term to-term)
                                              *primary-converter-table*)))

(defun print-primary-converter-table ()
  (format t "%#<HASH-TABLE ~%")
  (maphash-values #'(lambda (val) (format t "~S~%" val))
                  *primary-converter-table*)
  (format t ">"))


;; simple queue structure
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
  "Finds the shortest path in the graph of color spaces with BFS."
  (let ((visited (make-hash-table))
        (path-queue (enqueue (list begin-term) (make-queue))))
    (loop for path = (dequeue path-queue)
          for current-term = (car path)
          do (when (null current-term)
               (error "No route found: from ~S to ~S" begin-term dest-term))
             (if (eql current-term dest-term)
                 (return (nreverse path))
                 (unless (nth-value 1 (ensure-gethash current-term visited t))
                   (dolist (term (get-neighbors current-term))
                     (enqueue (cons term path) path-queue)))))))


(defmacro define-primary-converter ((begin-term dest-term &key (name (gen-converter-name begin-term dest-term)) (forced-bindings nil)) key-args &body body)
  "Defines FOO-TO-BAR function as a primary converter."
  (check-type begin-term symbol)
  (check-type dest-term symbol)
  (check-type key-args list)
  (check-type forced-bindings list)
  (let* ((begin-term (make-keyword begin-term))
         (dest-term (make-keyword dest-term))
         (lambda-list (append (get-args begin-term *package*)
                              (if key-args (cons '&key key-args) nil))))
    `(progn
       (declaim (inline ,name)
                (ftype (function * (values ,@(get-arg-types dest-term) &optional)) ,name))
       (defun ,name ,lambda-list
         ,@body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (add-primary-converter ,begin-term ,dest-term
                                ',lambda-list
                                :name ',name
                                :forced-bindings ',forced-bindings)))))


(defun converter-clamp-p (term1 term2)
  (and (not (eql (get-clamp term1) :always-clamped))
       (eql (get-clamp term2) :clampable)))

(defun global-clamp-arg-p (terms)
  (converter-clamp-p (car (last terms 2))
                     (car (last terms))))

(defun get-local-illuminant-key (term1 term2)
  (let* ((conv (get-primary-converter term1 term2))
         (key-args (primary-converter-key-args conv)))
    (or (find :rgbspace key-args)
        (find :illuminant key-args))))

(defun get-global-illuminant-key (terms)
  "Determines the required illuminant object for a given converter chain"
  (let ((conv-illum-keys (loop for (term1 term2) on terms
                               until (null term2)
                               collect (get-local-illuminant-key term1 term2))))
    (or (find :rgbspace conv-illum-keys)
        (find :illuminant conv-illum-keys))))

(defun need-rgbspace-to-illuminant-p (terms)
  (let ((illum-keys (loop for (term1 term2) on terms
                          until (null term2)
                          collect (get-local-illuminant-key term1 term2))))
    (and (find :rgbspace illum-keys)
         (find :illuminant illum-keys))))

(defun get-init-form (arg term1 term2)
  "Returns the default value of an argument of the converter from
term1 to term2"
  (funcall #'(lambda (x)
               (if (consp x) (second x) x))
           (find-if #'(lambda (x)
                        (if (consp x)
                            (string= arg (car x))
                            (string= arg x)))
                    (get-key-args-with-init term1 term2))))

(defun collect-key-args (terms &key (exclude-list nil))
  (remove-if #'(lambda (x) (member x exclude-list :test #'key-arg=))
             (delete-duplicates
              (loop for (term1 term2) on terms
                    until (null term2)
                    append (get-key-args term1 term2)))))

(defun key-arg= (key1 key2)
  (string= (car (ensure-list key1))
           (car (ensure-list key2))))

(defun collect-key-args-with-init (terms &key (exclude-list nil))
  (delete-if #'(lambda (x) (member x exclude-list :test #'key-arg=))
             (delete-duplicates
              (loop for (term1 term2) on terms
                    until (null term2)
                    append (mapcar #'(lambda (key)
                                       (list (sane-symbol key)
                                             (get-init-form key term1 term2)))
                                   (get-key-args term1 term2)))
              :test #'key-arg=)))

(defun collect-forced-bindings (terms)
  (delete-duplicates
   (loop for (term1 term2) on terms
         until (null term2)
         append (get-forced-bindings term1 term2))
   :key (compose #'car #'ensure-list)
   :test #'string=))
(defun collect-forced-bounded-args (terms)
  (mapcar (compose #'car #'ensure-list)
          (collect-forced-bindings terms)))
(defun collect-forced-bounded-key-args (terms)
  (mapcar #'make-keyword
          (collect-forced-bounded-args terms)))

(defun find-duplicates (lst &key (key #'identity) (test #'eql))
  (if (null lst)
      nil
      (or (member (car lst) (cdr lst) :test test :key key)
          (find-duplicates (cdr lst)))))

(defun gen-global-key-args (terms)
  (let ((exclude-lst (cons :clamp
                           (collect-forced-bounded-key-args terms))))
    `(,@(when (global-clamp-arg-p terms)
          `((,(sane-symbol :clamp) t)))
      ,@(collect-key-args-with-init terms :exclude-list exclude-lst))))

(defun expand-key-args (arg-lst)
  (mappend #'(lambda (key)
               (list key (sane-symbol key)))
           arg-lst))

(defun gen-last-key-args (terms)
  `(,@(gen-local-key-args (car (last terms 2)) (lastcar terms))
    ,@(when (global-clamp-arg-p terms) '(:clamp clamp))))

(defun gen-local-key-args (term1 term2)
  (expand-key-args
   (collect-key-args (list term1 term2)
                     :exclude-list '(:clamp))))

(defmacro defconverter (begin-term dest-term &key (name (gen-converter-name begin-term dest-term)) (exclude-args nil) (documentation nil))
  "Defines a converter function from BEGIN-TERM to DEST-TERM automatically."
  (let* ((begin-term (make-keyword begin-term))
         (dest-term (make-keyword dest-term))
         (global-name name)
         (chain (get-converter-chain begin-term dest-term))
         (global-key-args (gen-global-key-args chain))
         (global-illuminant (get-global-illuminant-key chain)))
    (assert (>= (length chain) 3)
            (chain)
            "The length of converters path is ~a. It should be greater than 1."
            (- (length chain) 1))
    (labels ((expand-forced-bindings (code)
               (let ((bindings (collect-forced-bindings chain)))
                 (if bindings
                     `(symbol-macrolet ,bindings ,code)
                     code)))
             (expand (term-lst code)
               (if (null (cdr term-lst))
                   code
                   (let* ((term1 (first term-lst))
                          (term2 (second term-lst))
                          (name (get-primary-converter-name term1 term2)))
                     (cond ((null code)
                            (expand (cdr term-lst) ; first conversion
                                    `(,name ,@(get-args term1 *package*)
                                            ,@(gen-local-key-args term1 term2))))
                           ((eql term2 dest-term) ; last conversion
                            (expand-forced-bindings
                             `(multiple-value-call #',name
                                ,code
                                ,@(gen-last-key-args chain))))
                           (t (expand (cdr term-lst) ; intermediate conversion
                                      `(multiple-value-call #',name
                                         ,code
                                         ,@(gen-local-key-args term1 term2)))))))))
      `(progn
         (declaim #+dufy/inline(inline ,global-name)
                  (ftype (function * (values ,@(get-arg-types (lastcar chain)) &optional)) ,global-name))
         (defun ,global-name (,@(get-args begin-term *package*)
                              ,@(when global-key-args
                                  `(&key ,@global-key-args)))
           (declare (optimize (speed 3) (safety 1)))
           ,@(ensure-list documentation)
           ,(case global-illuminant
              (:rgbspace
               `(let ((,(sane-symbol 'illuminant)
                        (dufy-core:rgbspace-illuminant ,(sane-symbol 'rgbspace))))
                  (declare (ignorable ,(sane-symbol 'illuminant)))
                  ,(expand chain nil)))
              (:illuminant (expand chain nil))
              (nil (expand chain nil))
              (otherwise (expand chain nil))))))))
