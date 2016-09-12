;;;; clx-freetype2-renderer.asd

(asdf:defsystem #:clx-freetype2-renderer
  :description "clx-freetype2-renderer is a simple package to render
  FreeType2 files using X11 paradigms. It was written for use with the
  StumpWM window manager, but has broader application in X11 based
  programs."
  :author "David Bjergaard <dbjergaard@gmail.com>, "
  :license "MIT"
  :depends-on (#:clx
	       #:cl-fad
	       #:cl-store
               #:cl-freetype2)
  :serial t
  :components ((:file "package")
	       (:file "font-cache")
	       (:file "font")
	       (:file "render")
               (:file "clx-freetype2-renderer")
	       (:file "tests")))

