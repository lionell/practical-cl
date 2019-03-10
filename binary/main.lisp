(in-package :io.github.lionell.binary)

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun define-class (name slots)
  `(defclass ,name ()
     ,(mapcar #'slot->defclass-slot slots)))

(defgeneric read-value (type stream &key)
  (:documentation "Read a value of the given type from the stream."))

(defun mklist (x) (if (listp x) x (list x)))

(defun normalize-slot-spec (spec)
  "Normalize (name type) -> (name (type))"
  (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun define-read-value (name slots)
  (with-gensyms (type-var object-var stream-var)
                `(defmethod read-value ((,type-var (eql ',name)) ,stream-var &key)
                   (let ((,object-var (make-instance ',name)))
                     (with-slots ,(mapcar #'first slots) ,object-var
                       ,@(mapcar #'(lambda (x) (slot->read-value x stream-var)) slots))
                     ,object-var))))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the given type to the stream."))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun define-write-value (name slots)
  (with-gensyms (type-var object-var stream-var)
                `(defmethod write-value ((,type-var (eql ',name)) ,stream-var ,object-var &key)
                   (with-slots ,(mapcar #'first slots) ,object-var
                     ,@(mapcar #'(lambda (x) (slot->write-value x stream-var)) slots))
                   ,object-var)))

(defmacro define-binary-class (name slots)
  `(progn
     ,(define-class name slots)
     ,(define-read-value name slots)
     ,(define-write-value name slots)))
