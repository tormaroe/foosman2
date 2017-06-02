rm -rf build
mkdir build

sbcl --eval "(ql:quickload :foosman2-web)" \
     --eval "(sb-ext:save-lisp-and-die \
     	       \"build/foosman2.exe\" \
     	       :toplevel #'foosman2-web.server:start-foosman2-daemon \
     	       :executable t)"

mkdir build/web
cp -r web/static build/web
cp foosman2.config build