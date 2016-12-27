;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.

(asdf:defsystem :jewel
  :depends-on (:clx :zpb-ttf :cl-vectors :cl-paths-ttf :cl-aa :cl-fad :iterate :bordeaux-threads
		    :cl-store :trivial-features :cl-aa-misc :xoanon.gui.garnet)
  :components
  ((:file "package")
   (:module jewel
	    :pathname "src/jewel"
	    :depends-on (:package :jewel-lab)
	    :components
	    ((:file "jewel")
	     (:file "define-methods")
	     (:file "drawing")
	     (:file "x")
	     (:file "anti-alias-graphics")))
      (:module jewel-lab
	    :pathname "src/jewel/jewel-lab"
	    :depends-on (:package )
	    :components
	    ((:file "xlib-lab")
	     (:file "jewel-pixmap-lab")
	     (:file "cl-processing-lab")
	     ))))
