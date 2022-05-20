#!/bin/bash

curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"
ln -s ~/.lisp/.sbclrc ~/.sbclrc
ln -s ~/.lisp/.clisprc.lisp ~/.clisprc.lisp
ln -s ~/.lisp/.eclrc ~/.eclrc
ln -s ~/.lisp/.clreplrc ~/.clreplrc
git clone https://github.com/tonyfischetti/pluto ~/pluto
ln -s ~/pluto ~/quicklisp/local-projects/pluto
ln -s ~/.lisp/linedit/ ~/quicklisp/local-projects/linedit
ln -s ~/.lisp/other-packages/cl-repl/ ~/quicklisp/local-projects/cl-repl
ln -s ~/.lisp/other-packages/cl-tui/ ~/quicklisp/local-projects/cl-tui
