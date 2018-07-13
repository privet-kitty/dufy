;;;
;;; Meta-definition of color space
;;;

(in-package :dufy-core)

(defstruct colorspace
  (name nil :type symbol)
  ;; arguments of COLORSPACE-to- functions
  (args nil :type list)
  ;; argument types of COLORSPACE-to- functions (used for type declaration)
  (arg-types nil :type list)
  ;; return type of -to-COLORSPACE functions (used for type declaration)
  (return-types nil :type list) 
  (documentation nil :type (or null string))
  ;; adjacent color spaces linked with primary converters
  (neighbors nil :type list))

(defparameter *colorspace-table* (make-hash-table))

(defun colorspace= (space1 space2)
  (eql (colorspace-name space1)
       (colorspace-name space2)))

(defmacro define-colorspace (name args &key arg-types return-types documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *colorspace-table*)
           (make-colorspace :name ',name
                            :args ',args
                            :arg-types ',arg-types
                            :return-types ',return-types
                            :documentation ,documentation
                            :neighbors nil))))

(defun ensure-colorspace (thing)
  (if (typep thing 'colorspace)
      thing
      (or (gethash thing *colorspace-table*)
          (error "No such color space: ~A" thing))))
(defun ensure-colorspace-name (thing)
  (etypecase thing
    (symbol thing)
    (colorspace (colorspace-name thing))))

(defun get-neighbors (name)
  (colorspace-neighbors (ensure-colorspace name)))
(defun (setf get-neighbors) (val name)
  (setf (colorspace-neighbors (ensure-colorspace name)) val))
(defun get-args (name &key (package nil) (suffix ""))
  (mapcar #'(lambda (x) (intern (format nil "~A~A" x suffix)
                                (or package (symbol-package x))))
          (colorspace-args (ensure-colorspace name))))
(defun get-arg-types (name)
  (colorspace-arg-types (ensure-colorspace name)))
(defun get-return-types (name)
  (colorspace-return-types (ensure-colorspace name)))

(defun print-hash-table (hash)
  (format t "~%#<HASH-TABLE ~%")
  (maphash-values #'(lambda (val) (format t "~S~%" val)) hash)
  (format t ">"))
(defun print-colorspace-table ()
  (print-hash-table *colorspace-table*))



;;;
;;; Primary converter between two color spaces
;;;

(defstruct (primary-converter (:constructor %make-primary-converter))
  "In the beginning is the primary converter, which is conceptually an
arc that connects two color spaces. All other converters are generated
by linking them."
  (from-colorspace nil :type symbol)
  (to-colorspace nil :type symbol)
  (name nil :type symbol)
  (key-args nil :type list)
  (allow-other-keys nil :type boolean) ; currently not used
  (aux-args nil :type list))

(defparameter *primary-converter-table* (make-hash-table :test 'equal))

(defun gen-converter-name (from-colorspace to-colorspace)
  (intern (format nil "~A-TO-~A"
                  (ensure-colorspace-name from-colorspace)
                  (ensure-colorspace-name to-colorspace))
          *package*))

(defun lambda-list= (lambda-list1 lambda-list2)
  (if (null lambda-list1)
      (if (null lambda-list2) t nil)
      (if (null lambda-list2)
          nil
          (and (string= (car lambda-list1) (car lambda-list2))
               (lambda-list= (cdr lambda-list1) (cdr lambda-list2))))))

(defun find-duplicates (lst &key (key #'identity) (test #'eql))
  (if (null lst)
      nil
      (or (member (funcall key (car lst)) (cdr lst) :test test :key key)
          (find-duplicates (cdr lst)))))

(defun duplicate-aux-and-key-p (key-args aux-args)
  (find-duplicates (append (mapcar #'caar key-args)
                           (mapcar #'car aux-args))))

(defun make-primary-converter (from-colorspace to-colorspace lambda-list &key (name (gen-converter-name from-colorspace to-colorspace)))
  (multiple-value-bind (required optional rest keyword allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or optional rest)
      (error "Primary converter cannot take either &optional or &rest arguments."))
    (when (duplicate-aux-and-key-p keyword aux)
      (error "Duplicated names in &key and &aux arguments are not allowed."))
    (unless (lambda-list= required (get-args from-colorspace))
      (simple-style-warning "Given lambda list ~A isn't equal to the ARGS ~A of color space ~A"
                            lambda-list (get-args from-colorspace) from-colorspace))
    (%make-primary-converter :from-colorspace from-colorspace
                             :to-colorspace to-colorspace
                             :name name
                             :key-args keyword
                             :allow-other-keys allow-other-keys
                             :aux-args aux)))

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

(defun find-converter-path (from-colorspace to-colorspace &key (if-does-not-exist :error))
  "Finds the shortest path in the graph of color spaces with BFS."
  (let ((visited (make-hash-table))
        (path-queue (enqueue (list from-colorspace) (make-queue))))
    (loop for path = (dequeue path-queue)
          for current-colorspace = (car path)
          do (when (null current-colorspace)
               (ecase if-does-not-exist
                 (:error
                  (error "No route found: from ~S to ~S" from-colorspace to-colorspace))
                 ((nil) (return nil))))
             (if (eql current-colorspace to-colorspace)
                 (return (nreverse path))
                 (unless (nth-value 1 (ensure-gethash current-colorspace visited t))
                   (dolist (term (get-neighbors current-colorspace))
                     (enqueue (cons term path) path-queue)))))))

(defun add-primary-converter (from-colorspace to-colorspace lambda-list &key (name (gen-converter-name from-colorspace to-colorspace)))
  "Registers a primary converter to *PRIMARY-CONVERTER-TABLE*"
  (let ((path (find-converter-path from-colorspace to-colorspace :if-does-not-exist nil)))
    (when (> (length path) 2)
      (error "A path from ~a to ~a already exists: ~a" from-colorspace to-colorspace path)))
  (setf (gethash (list from-colorspace to-colorspace) *primary-converter-table*)
        (make-primary-converter from-colorspace to-colorspace lambda-list
                                :name name))
  (pushnew to-colorspace (get-neighbors from-colorspace))
  name)

(defun get-primary-converter (from-colorspace to-colorspace)
  (or (gethash (list from-colorspace to-colorspace) *primary-converter-table*)
      (error "No primary converter found: from ~A to ~A" from-colorspace to-colorspace)))
(defun get-primary-converter-name (from-colorspace to-colorspace)
  (primary-converter-name (get-primary-converter from-colorspace to-colorspace)))
(defun get-key-args (from-colorspace to-colorspace)
  (primary-converter-key-args (get-primary-converter from-colorspace to-colorspace)))
(defun get-key-arg-key-names (from-colorspace to-colorspace)
  (mapcar #'caar (get-key-args from-colorspace to-colorspace)))
(defun get-key-arg-names (from-colorspace to-colorspace)
  (mapcar #'cadar (get-key-args from-colorspace to-colorspace)))
(defun get-aux-args (from-colorspace to-colorspace)
  (primary-converter-aux-args (get-primary-converter from-colorspace to-colorspace)))
(defun get-aux-names (from-colorspace to-colorspace)
  (mapcar #'car (get-aux-args from-colorspace to-colorspace)))
(defun get-allow-other-keys (from-colorspace to-colorspace)
  (primary-converter-allow-other-keys (get-primary-converter from-colorspace to-colorspace)))

(defun print-primary-converter-table ()
  (print-hash-table *primary-converter-table*))


(defmacro define-primary-converter ((from-colorspace to-colorspace &key (name (gen-converter-name from-colorspace to-colorspace))) lambda-list &body body)
  "Defines FOO-TO-BAR function as a primary converter. Only &key
arguments (without supplied-p-parameter) and &aux arguments are
allowed. "
  (check-type from-colorspace symbol)
  (check-type to-colorspace symbol)
  (check-type lambda-list list)
  `(progn
     (declaim (inline ,name)
              (ftype (function * (values ,@(get-return-types to-colorspace) &optional)) ,name))
     (defun ,name ,lambda-list
       (declare ,@(mapcar #'(lambda (typ arg) (list typ arg))
                          (get-arg-types from-colorspace)
                          lambda-list))
       ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (add-primary-converter ',from-colorspace ',to-colorspace
                              ',lambda-list
                              :name ',name))))


;;;
;;; Codes for linking primary converters
;;;

(defun global-allow-other-keys-p (chain)
  (loop for (term1 term2) on chain
        until (null term2)
        do (when (get-allow-other-keys term1 term2)
             (return t))
        finally (return nil)))

(defun collect-key-args (chain &key (exclude-list nil))
  (remove-if #'(lambda (x) (member x exclude-list :test #'key-arg=))
             (delete-duplicates
              (loop for (term1 term2) on chain
                    until (null term2)
                    append (get-key-args term1 term2))
              :key #'caar)))
(defun collect-key-arg-names (chain)
  (mapcar #'cadar (collect-key-args chain)))
(defun collect-key-arg-key-names (chain)
  (mapcar #'caar (collect-key-args chain)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun collect-aux-args (chain)
  (loop for (term1 term2) on chain
        until (null term2)
        append (get-aux-args term1 term2) into res
        finally (aif (find-duplicates res :test #'string= :key #'car)
                     (error "Duplicated &aux arguments are not allowed: ~A" it)
                     (return res))))

(defun collect-aux-arg-names (chain)
  (mapcar #'car (collect-aux-args chain)))

(defun key-arg= (key1 key2)
  (string= (car (ensure-list key1))
           (car (ensure-list key2))))

(defun gen-global-key-args (chain &optional exclude-args)
  (mapcar #'(lambda (x) (list (cadar x) (second x)))
          (delete-if #'(lambda (x) (member (caar x) exclude-args :test #'string=))
                     (collect-key-args chain))))

(defun gen-global-aux-args (chain)
  (collect-aux-args chain))

(defun gen-local-key-args (term1 term2 &optional exclude-args)
  (mappend #'car
           (delete-if #'(lambda (x) (member (caar x) exclude-args :test #'string=))
                      (collect-key-args (list term1 term2)))))

(defun gen-global-args (chain &key exclude-args extra-key-args (with-aux t) (dimension 1) (extra-suffix ""))
  (check-type dimension (integer 1))
  (let ((global-key-args (gen-global-key-args chain exclude-args))
        (global-aux-args (gen-global-aux-args chain)))
    ;; If duplicates between &key and &aux arguments exist, &aux takes priority.
    (dolist (x global-aux-args)
      (setf global-key-args
            (delete (car x) global-key-args :key #'car :test #'string=)))
    (append (if (= dimension 1)
                (get-args (car chain) :suffix extra-suffix)
                (loop for d from 1 to dimension
                      append (get-args (car chain)
                                       :suffix (format nil "~A~A" d extra-suffix))))
            (when global-key-args
              `(&key ,@global-key-args
                     ,@(mapcar #'(lambda (x) (list (cadar x) (second x)))
                               extra-key-args)))
            (when (and global-aux-args with-aux)
              `(&aux ,@global-aux-args)))))

(defun gen-passed-global-args (chain &key exclude-args (suffix ""))
  (multiple-value-bind (required optional rest keyword allow-other-keys aux)
      (parse-ordinary-lambda-list (gen-global-args chain
                                                   :exclude-args exclude-args
                                                   :extra-suffix suffix
                                                   :with-aux nil))
    (declare (ignore optional rest allow-other-keys aux))
    (append required
            (mappend #'car keyword))))

(defun expand-conversion-form (chain &key exclude-args)
  (labels ((expand (term-lst code)
              (if (null (cdr term-lst))
                  code
                  (let* ((term1 (first term-lst))
                         (term2 (second term-lst))
                         (name (get-primary-converter-name term1 term2)))
                    (cond ((null term2) code) ; end
                          ((null code)
                           (expand (cdr term-lst) ; first conversion
                                   `(,name ,@(get-args term1)
                                           ,@(gen-local-key-args term1 term2 exclude-args))))
                          (t (expand (cdr term-lst) ; intermediate conversion
                                     `(multiple-value-call #',name
                                        ,code
                                        ,@(gen-local-key-args term1 term2 exclude-args)))))))))
    (expand chain nil)))

(defmacro defconverter (from-colorspace to-colorspace &key (name (gen-converter-name from-colorspace to-colorspace)) (exclude-args nil) (documentation nil))
  "Generates and defines a converter function from FROM-COLORSPACE to
TO-COLORSPACE automatically with linking primary converters."
  (let* ((chain (find-converter-path from-colorspace to-colorspace))
         (global-args (gen-global-args chain :exclude-args exclude-args)))
    `(progn
       (declaim #+dufy/inline (inline ,name)
                (ftype (function * (values ,@(get-return-types (lastcar chain)) &optional)) ,name))
       (defun ,name ,global-args
         (declare (optimize (speed 3) (safety 1))
                  (ignorable ,@(collect-aux-arg-names chain))
                  ,@(mapcar #'(lambda (typ arg) (list typ arg))
                            (get-arg-types from-colorspace)
                            global-args))
         ,@(ensure-list documentation)
         ,(expand-conversion-form chain :exclude-args exclude-args)))))

(defmacro let-converter (definitions &body body)
  "local version of defconverter

definitions ::= (definition*)
definition ::= (name from-colorspace to-colorspace &key exclude-args)

Example:
 (let-converter ((rgbpack-to-lchab rgbpack lchab)
                 (lchab-to-rgbpack lchab rgbpack :exclude-args (clamp)))
  (format t \"~X~%\" (multiple-value-call #'lchab-to-rgbpack
                       (rgbpack-to-lchab #xAABBCC))))   
;; AABBCC
;; => NIL"
  (let ((name-lst nil)
        (return-types-lst nil))
    `(labels
         ,(loop
            for def in definitions
            collect
            (destructuring-bind (name from-colorspace to-colorspace &key (exclude-args nil)) def
              (let* ((chain (find-converter-path from-colorspace to-colorspace))
                     (global-args (gen-global-args chain :exclude-args exclude-args)))
                (push name name-lst)
                (push (get-return-types (lastcar chain)) return-types-lst)
                `(,name ,global-args
                        (declare (optimize (speed 3) (safety 1))
                                 (ignorable ,@(collect-aux-arg-names chain))
                                 ,@(mapcar #'(lambda (typ arg) (list typ arg))
                                           (get-arg-types from-colorspace)
                                           global-args))
                        ,(expand-conversion-form chain :exclude-args exclude-args)))))
       (declare #+dufy/inline(inline ,@name-lst)
                ,@(loop for return-types in return-types-lst
                        for name in name-lst
                        collect `(ftype (function * (values ,@return-types &optional)) ,name)))
       ,@body)))



;;;
;;; Functional on color spaces
;;; 

(defstruct (functional (:constructor %make-functional))
  "The functional here is e.g. color difference, luminance or color
temperature. If you define a functional for a color space with
DEFINE-FUNCTIONAL, you can extend it to other color spaces with
EXTEND-FUNCTIONAL."
  (fname nil :type symbol) ; function name
  (term nil :type symbol) ; symbol used for looking up
  (colorspace nil :type symbol)
  (dimension 1 :type (integer 1)) ; e.g. dimension of a color difference functional is two
  (key-args nil :type list)
  (allow-other-keys nil :type boolean) ; currently not used
  (aux-args nil :type list))

(defun make-functional (fname colorspace lambda-list &key (term fname))
  (multiple-value-bind (required optional rest keyword allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or optional rest)
      (error "Primary converter cannot take either &optional or &rest arguments."))
    (multiple-value-bind (dim rem) (floor (length required)
                                          (length (get-args colorspace)))
      (unless (= rem 0)
        (error "The number of the required args ~A must be multiples of the number of ARGS ~A of color space ~A"
               required (get-args colorspace) colorspace))
      (%make-functional :fname fname
                        :term term
                        :colorspace colorspace
                        :dimension dim
                        :key-args keyword
                        :aux-args aux
                        :allow-other-keys allow-other-keys))))

(defparameter *functional-table* (make-hash-table))

(defun print-functional-table ()
  (print-hash-table *functional-table*))

(defun add-functional (fname colorspace lambda-list &key (term fname))
  "Registers a functional to *functional-table*"
  (setf (gethash term *functional-table*)
        (make-functional fname colorspace lambda-list
                         :term term)))

(defun get-functional (term)
  (or (gethash term *functional-table*)
      (error "No functional found: ~A" term)))

(defun circularize (lst)
  (let ((res (copy-list lst)))
    (setf (cdr (last res)) res)
    res))

(defmacro define-functional ((fname colorspace &key (term fname)) lambda-list &body body)
  `(progn
     (declaim (inline ,fname))
     (defun ,fname ,lambda-list
       (declare ,@(mapcar #'(lambda (typ arg) (list typ arg))
                          (circularize (get-arg-types colorspace))
                          (parse-ordinary-lambda-list lambda-list)))
       ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (add-functional ',fname ',colorspace ',lambda-list :term ',term))))

(defmacro extend-functional (term colorspace &key (fname (intern (format nil "~A-~A" (ensure-colorspace-name colorspace) term))) (exclude-args nil) (documentation nil))
  "Generates and defines a functional on another color space. The name
of the function is [COLORSPACE]-[TERM] if FNAME is not given."
  (let* ((functional (get-functional term))
         (func-name (functional-fname functional))
         (extra-key-args (functional-key-args functional))
         (dimension (functional-dimension functional))
         (to-colorspace (functional-colorspace functional))
         (from-colorspace colorspace)
         (chain (find-converter-path from-colorspace to-colorspace))
         (transform (gensym (string (gen-converter-name from-colorspace to-colorspace)))))
    `(defun ,fname ,(gen-global-args chain
                     :exclude-args exclude-args
                     :extra-key-args extra-key-args
                     :with-aux nil
                     :dimension dimension)
       (declare (optimize (speed 3) (safety 1)))
       ,@(ensure-list documentation)
       (let-converter ((,transform ,from-colorspace ,to-colorspace
                                   :exclude-args ,exclude-args))
         (multiple-value-call #',func-name
           ,@(if (= dimension 1)
                 `((,transform ,@(gen-passed-global-args chain :exclude-args exclude-args)))
                 (loop for d from 1 to dimension
                       collect `(,transform ,@(gen-passed-global-args chain :exclude-args exclude-args :suffix d))))
           ,@(mappend #'car extra-key-args))))))
