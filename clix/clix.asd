
(asdf:defsystem :clix
  :description "My personal common lisp utilities"
  :author "Tony Fischetti"
  :license "GPL-3"
  :depends-on (:cl-fad :cl-ppcre :parse-float :alexandria)
  :components ((:file "clix")))
