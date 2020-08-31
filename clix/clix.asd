
(asdf:defsystem :clix
  :description "My personal common lisp utilities"
  :author "Tony Fischetti"
  :license "GPL-3"
  :depends-on (:cl-fad
               :cl-ppcre
               :parse-float
               :alexandria
               :drakma
               :cxml
               :xpath
               :plump
               :lquery
               :quri
               :html-entities
               :cl-json
               :yason)
  :components ((:file "clix")))
