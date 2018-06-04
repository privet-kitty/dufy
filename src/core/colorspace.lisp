;;;
;;; Meta-definition of color space
;;;

(in-package :dufy-core)

(defun sane-symbol (symb)
  (intern (symbol-name symb) *package*))

(defstruct colorspace
  "A colorspace object is used as a vertex of converters graph.
clamp::= :clampable | :always-clamped | nil"
  (name nil :type symbol)
  (args nil :type list)
  (arg-types nil :type list)
  (clamp nil :type symbol)
  (documentation nil :type (or null string))
  (neighbors nil :type list))

(defparameter *colorspace-table* (make-hash-table))

(defun colorspace= (space1 space2)
  (eql (colorspace-name space1)
       (colorspace-name space2)))

(defmacro define-colorspace (name args &key clamp documentation)
  (let ((key (make-keyword name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,key *colorspace-table*)
             (make-colorspace :name ,key
                              :args ',(mapcar (compose #'first #'ensure-list)
                                              args)
                              :arg-types ',(mapcar (compose #'second #'ensure-list)
                                                   args)
                              :clamp ,clamp
                              :documentation ,documentation
                              :neighbors nil)))))

(defun ensure-colorspace (thing)
  (if (typep thing 'colorspace)
      thing
      (or (gethash thing *colorspace-table*)
          (error "No such color space: ~A" thing))))
(defun ensure-colorspace-name (thing)
  (etypecase thing
    (symbol (make-keyword thing))
    (colorspace (colorspace-name thing))))

(defun get-neighbors (name)
  (colorspace-neighbors (ensure-colorspace name)))
(defun (setf get-neighbors) (val name)
  (setf (colorspace-neighbors (ensure-colorspace name)) val))
(defun get-clamp (name)
  (colorspace-clamp (ensure-colorspace name)))
(defun get-args (name &optional (package nil))
  (mapcar (if package
              #'(lambda (x) (intern (symbol-name x) package))
              #'identity)
          (colorspace-args (ensure-colorspace name))))
(defun get-arg-types (name)
  (colorspace-arg-types (ensure-colorspace name)))

(defun print-colorspace-table ()
  (format t "~%#<HASH-TABLE ~%")
  (maphash-values #'(lambda (val) (format t "~S~%" val))
                  *colorspace-table*)
  (format t ">"))



;;;
;;; Primary converter
;;;

(defun gen-converter-name (from-colorspace to-colorspace)
  (intern (format nil "~A-TO-~A"
                  (ensure-colorspace-name from-colorspace)
                  (ensure-colorspace-name to-colorspace))
          *package*))

(defun extract-key-args-with-init (lambda-list)
  (mapcar #'(lambda (node)
              (list (cadar node) (second node)))
          (nth-value 3 (parse-ordinary-lambda-list lambda-list))))

(defun extract-key-args (lambda-list)
  (mapcar #'(lambda (x) (make-keyword (car (ensure-list x))))
          (extract-key-args-with-init lambda-list)))

(defstruct (primary-converter (:constructor %make-primary-converter))
  "PRIMARY-CONVERTER object is an edge that connects two colorspaces."
  (from-colorspace nil :type symbol)
  (to-colorspace nil :type symbol)
  (name nil :type symbol)
  (key-args nil :type list)
  (allow-other-keys nil :type boolean) ; currently not used
  (aux-args nil :type list))

(defparameter *primary-converter-table* (make-hash-table :test 'equal))

(defun lambda-list= (lambda-list1 lambda-list2)
  (if (null lambda-list1)
      (if (null lambda-list2) t nil)
      (if (null lambda-list2)
          nil
          (and (string= (car lambda-list1) (car lambda-list2))
               (lambda-list= (cdr lambda-list1) (cdr lambda-list2))))))

(defun make-primary-converter (from-colorspace to-colorspace lambda-list &key (name (gen-converter-name from-colorspace to-colorspace)))
  (multiple-value-bind (required optional rest keyword allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or optional rest)
      (error "primary converter cannot take either &optional or &rest arguments."))
    (unless (lambda-list= required (get-args from-colorspace))
      (simple-style-warning "given lambda list ~A isn't equal to the ARGS ~A of color space ~A"
                            lambda-list (get-args from-colorspace) from-colorspace))
    (%make-primary-converter :from-colorspace from-colorspace
                             :to-colorspace to-colorspace
                             :name name
                             :key-args keyword
                             :allow-other-keys allow-other-keys
                             :aux-args aux)))

(defun add-primary-converter (from-colorspace to-colorspace lambda-list &key (name (gen-converter-name from-colorspace to-colorspace)))
  (setf (gethash (list from-colorspace to-colorspace) *primary-converter-table*)
        (make-primary-converter from-colorspace to-colorspace lambda-list
                                :name name))
  (pushnew to-colorspace (get-neighbors from-colorspace)))

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

(defun get-converter-chain (from-colorspace to-colorspace)
  "Finds the shortest path in the graph of color spaces with BFS."
  (let ((visited (make-hash-table))
        (path-queue (enqueue (list from-colorspace) (make-queue))))
    (loop for path = (dequeue path-queue)
          for current-colorspace = (car path)
          do (when (null current-colorspace)
               (error "No route found: from ~S to ~S" from-colorspace to-colorspace))
             (if (eql current-colorspace to-colorspace)
                 (return (nreverse path))
                 (unless (nth-value 1 (ensure-gethash current-colorspace visited t))
                   (dolist (term (get-neighbors current-colorspace))
                     (enqueue (cons term path) path-queue)))))))


(defmacro define-primary-converter ((from-colorspace to-colorspace &key (name (gen-converter-name from-colorspace to-colorspace))) lambda-list &body body)
  "Defines FOO-TO-BAR function as a primary converter."
  (check-type from-colorspace symbol)
  (check-type to-colorspace symbol)
  (check-type lambda-list list)
  (let* ((from-colorspace (make-keyword from-colorspace))
         (to-colorspace (make-keyword to-colorspace)))
    `(progn
       (declaim (inline ,name)
                (ftype (function * (values ,@(get-arg-types to-colorspace) &optional)) ,name))
       (defun ,name ,lambda-list
         ,@body)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (add-primary-converter ,from-colorspace ,to-colorspace
                                ',lambda-list
                                :name ',name)))))


(defun converter-clamp-p (term1 term2)
  (and (not (eql (get-clamp term1) :always-clamped))
       (eql (get-clamp term2) :clampable)))

(defun global-clamp-arg-p (terms)
  (converter-clamp-p (car (last terms 2))
                     (car (last terms))))

(defun global-allow-other-keys-p (terms)
  (loop for (term1 term2) on terms
        until (null term2)
        do (when (get-allow-other-keys term1 term2)
             (return t))
        finally (return nil)))

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
  (funcall #'second
           (find-if #'(lambda (x)
                        (if (keywordp arg)
                            (eql arg (caar x))
                            (string= arg (cadar x))))
                    (get-key-args term1 term2))))

(defun collect-key-args (terms &key (exclude-list nil))
  (remove-if #'(lambda (x) (member x exclude-list :test #'key-arg=))
             (delete-duplicates
              (loop for (term1 term2) on terms
                    until (null term2)
                    append (get-key-args term1 term2))
              :key #'caar)))
(defun collect-key-arg-names (terms)
  (mapcar #'cadar (collect-key-args terms)))
(defun collect-key-arg-key-names (terms)
  (mapcar #'caar (collect-key-args terms)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defun find-duplicates (lst &key (key #'identity) (test #'eql))
  (if (null lst)
      nil
      (or (member (car lst) (cdr lst) :test test :key key)
          (find-duplicates (cdr lst)))))

(defun collect-aux-args (terms)
  (loop for (term1 term2) on terms
        until (null term2)
        append (get-aux-args term1 term2) into res
        finally (aif (find-duplicates res :test #'(lambda (x y)
                                                    (string= (car x) (car y))))
                     (error "Duplicated &aux arguments found: ~A" it)
                     (return res))))

(defun collect-aux-arg-names (terms)
  (mapcar #'car (collect-aux-args terms)))

(defun key-arg= (key1 key2)
  (string= (car (ensure-list key1))
           (car (ensure-list key2))))

(defun gen-global-key-args (terms &optional exclude-args)
  (mapcar #'(lambda (x) (list (cadar x) (second x)))
          (delete-if #'(lambda (x) (member (caar x) exclude-args :test #'string=))
                     (collect-key-args terms))))

(defun gen-global-aux-args (terms)
  (collect-aux-args terms))

(defun gen-local-key-args (term1 term2 &optional exclude-args)
  (mappend #'car
           (delete-if #'(lambda (x) (member (caar x) exclude-args :test #'string=))
                      (collect-key-args (list term1 term2)))))

(defun gen-global-args (terms &optional exclude-args)
  (let ((global-key-args (gen-global-key-args terms exclude-args))
        (global-aux-args (gen-global-aux-args terms)))
    ;; If duplicates between &key and &aux arguments exist, &aux takes priority.
    (dolist (x global-aux-args)
      (setf global-key-args
            (delete (car x) global-key-args :key #'car :test #'string=)))
    (append (get-args (car terms))
            (when global-key-args
              `(&key ,@global-key-args))
            (when global-aux-args
              `(&aux ,@global-aux-args)))))

(defun expand-conversion-form (terms &key exclude-args)
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
    (expand terms nil)))

(defmacro defconverter (from-colorspace to-colorspace &key (name (gen-converter-name from-colorspace to-colorspace)) (exclude-args nil) (documentation nil))
  "Defines a converter function from FROM-COLORSPACE to TO-COLORSPACE automatically."
  (let* ((from-colorspace (make-keyword from-colorspace))
         (to-colorspace (make-keyword to-colorspace))
         (chain (get-converter-chain from-colorspace to-colorspace)))
    (when (<= (length chain) 2)
      (error "The length of converters path is ~a. It should be greater than 1."
             (- (length chain) 1)))
    `(progn
       (declaim #+dufy/inline (inline ,name)
                (ftype (function * (values ,@(get-arg-types (lastcar chain)) &optional)) ,name))
       (defun ,name ,(gen-global-args chain exclude-args)
         (declare (optimize (speed 3) (safety 1))
                  (ignorable ,@(collect-aux-arg-names chain)))
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
        (arg-types-lst nil))
    `(labels
         ,(loop
            for def in definitions
            collect
            (destructuring-bind (name from-colorspace to-colorspace &key (exclude-args nil)) def
              (let* ((from-colorspace (make-keyword from-colorspace))
                     (to-colorspace (make-keyword to-colorspace))
                     (chain (get-converter-chain from-colorspace to-colorspace)))
                (when (<= (length chain) 2)
                  (error "The length of converters path from ~a to ~a is ~a. It should be greater than 1."
                         from-colorspace to-colorspace (- (length chain) 1)))
                (push name name-lst)
                (push (get-arg-types (lastcar chain)) arg-types-lst)
                `(,name ,(gen-global-args chain exclude-args)
                        (declare (optimize (speed 3) (safety 1))
                                 (ignorable ,@(collect-aux-arg-names chain)))
                        ,(expand-conversion-form chain :exclude-args exclude-args)))))
       (declare (inline ,@name-lst)
                ,@(loop for arg-types in arg-types-lst
                        for name in name-lst
                        collect `(ftype (function * (values ,@arg-types &optional)) ,name)))
       ,@body)))


(defstruct (functional (:constructor %make-functional))
  (name nil :type symbol)
  (term nil :type symbol)
  (colorspace nil :type symbol)
  (key-args nil :type list)
  (allow-other-keys nil :type boolean) ; currently not used
  (aux-args nil :type list))

(defun make-functional (name colorspace lambda-list &key (term name))
  (multiple-value-bind (required optional rest keyword allow-other-keys aux)
      (parse-ordinary-lambda-list lambda-list)
    (when (or optional rest)
      (error "primary converter cannot take either &optional or &rest arguments."))
    (unless (length= required (get-args colorspace))
      (error "given lambda list ~A isn't consistent with the ARGS ~A of color space ~A"
             lambda-list (get-args colorspace) colorspace))
    (%make-functional :name name
                      :term term
                      :colorspace colorspace
                      :key-args keyword
                      :aux-args aux
                      :allow-other-keys allow-other-keys)))

(defparameter *functional-table* (make-hash-table))

(defun add-functional (name colorspace lambda-list &key (term name))
  (setf (gethash term *functional-table*)
        (make-functional name colorspace lambda-list
                         :term term)))

(defmacro define-functional ((name colorspace &key (term name)) lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list
       ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (add-functional ',name ',colorspace ',lambda-list :term ',term))))


;; (define-functional (rgb-test :rgb :term test) (r g b)
;;   (+ r g b))
