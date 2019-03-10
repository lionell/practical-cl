(defpackage :io.github.lionell.spam
  (:use :cl :cl-ppcre :uiop)
  (:export
   #:add-file-to-corpus
   #:add-directory-to-corpus
   #:classify
   #:train-from-corpus
   #:test-from-corpus
   #:test-classifier
   #:analyze-results))
