;;;; clx-truetype.asd

(asdf:defsystem #:clx-truetype
  :serial t
  :description "clx-truetype is pure common lisp solution for antialiased TrueType font rendering using CLX and XRender extension."
  :author "Michael Filonenko <filonenko.mikhail@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:clx
               #:zpb-ttf
               #:cl-vectors
               #:cl-paths-ttf
               #:cl-aa
               #:cl-fad
               #:cl-store
               #:trivial-features)
  :components ((:file "package")
               (:file "clx-utils")
               (:file "font-cache")
               (:file "clx-truetype")))


(asdf:defsystem #:clx-truetype-test
  :serial t
  :description "Testing library for clx-truetype."
  :author "Michael Filonenko <filonenko.mikhail@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (#:clx-truetype)
  :components ((:module test
                :components ((:file "hello-world")))))
