(in-package :io.github.lionell.binary)

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defun slot->defclass-slot (spec)
  "Ex. (major-version u1) ->
       (major-version :initarg :major-version :accessor major-version)"
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream)
  "Ex. (major-version u1) stream ->
       (setf major-version (read-value 'u1 stream))"
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
  "Ex. (major-version u1) stream ->
       (write-value 'u1 stream major-version)"
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun normalize-slot-spec (spec)
  "Normalize (name type) -> (name (type))"
  (list (first spec) (mklist (second spec))))

(defun mklist (x) (if (listp x) x (list x)))

(defun define-class (name slots)
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))

(defun define-read-value (name slots)
  (with-gensyms (type-var object-var stream-var)
                `(defmethod read-value ((,type-var (eql ',name)) ,stream-var &key)
                   (let ((,object-var (make-instance ',name)))
                     (with-slots ,(mapcar #'first slots) ,object-var
                       ,@(mapcar #'(lambda (x) (slot->read-value x stream-var)) slots))
                     ,object-var))))

(defun define-write-value (name slots)
  (with-gensyms (type-var object-var stream-var)
                `(defmethod write-value ((,type-var (eql ',name)) ,stream-var ,object-var &key)
                   (with-slots ,(mapcar #'first slots) ,object-var
                     ,@(mapcar #'(lambda (x) (slot->write-value x stream-var)) slots))
                   ,object-var)))

(defmacro define-binary-class (name slots)
  (let ((name (eval name))
        (slots (eval slots)))
    `(progn
       ,(define-class name slots)
       ,(define-read-value name slots)
       ,(define-write-value name slots))))
