(require :asdf)

(load "cl-ppcre/cl-ppcre.asd")
(load "spam.asd")

(asdf:make :spam)

(in-package :io.github.lionell.spam)

(add-directory-to-corpus "./spam/" 'spam *corpus*)
(add-directory-to-corpus "./ham/" 'ham *corpus*)

(format t "To train and test on the corpus run:~%")
(format t "(in-package :io.github.lionell.spam)~%")
(format t "(analyze-results (test-classifier *corpus* 0.3))")

;; (analyze-results (test-classifier *corpus* 0.3))
