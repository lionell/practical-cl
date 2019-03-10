(defpackage :io.github.lionell.spam-asd
  (:use :cl :asdf))

(in-package :io.github.lionell.spam-asd)

(defsystem :spam
  :author "Ruslan Sakevych"
  :description "Spam classifier"
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "main" :depends-on ("package"))))
