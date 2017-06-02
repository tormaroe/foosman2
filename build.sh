rm -rf build
mkdir build

sbcl --eval "(ql:quickload :foosman2-web)" \
     --eval "(sb-ext:save-lisp-and-die \
     	       \"build/foosman2.exe\" \
     	       :toplevel #'foosman2-web.server:start-foosman2-daemon \
     	       :executable t)"

cp -r web/static build
cp foosman.config build