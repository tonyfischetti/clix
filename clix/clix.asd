
(asdf:defsystem :clix
  :description "My personal common lisp utilities"
  :author "Tony Fischetti"
  :license "GPL-3"
  :depends-on (
               ; portable pathname library
               ; https://edicl.github.io/cl-fad/
               :cl-fad

               ; ya tu sabes
               ; https://edicl.github.io/cl-ppcre/
               :cl-ppcre

               ; http://quickdocs.org/parse-float/
               :parse-float

               ; the venerable alexadria
               ; https://gitlab.common-lisp.net/alexandria/alexandria
               :alexandria

               ; HTTP client of choicee
               ; https://edicl.github.io/drakma/
               :drakma

               ; XML parser of choice
               ; https://common-lisp.net/project/cxml/
               :cxml
            
               ; Plexippus XPATH library
               ; https://common-lisp.net/project/plexippus-xpath/
               :xpath

               ; lenient HTML parser
               ; https://github.com/Shinmera/plump
               :plump

               ; dope jquery like thing for plump
               ; https://github.com/Shinmera/plump
               :lquery

               ; URI encoding/decoding
               ; https://github.com/fukamachi/quri
               :quri

               ; awesome json parser
               ; https://github.com/phmarek/yason
               :yason)

  :components ((:file "clix")))
