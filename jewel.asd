


(asdf:defsystem :jewel
  :depends-on (:clx :zpb-ttf :cl-vectors :cl-paths-ttf :cl-aa :cl-fad
		    :cl-store :trivial-features :cl-aa-misc :xoanon.gui.garnet)
  :components
  ((:file "package")
   (:module jewel
	    :pathname "src/jewel"
	    :depends-on (:package)
	    :components
	    ((:file "gem")
	     (:file "define-methods")
	     (:file "drawing")
	     (:file "x")
	     (:file "anti-alias-graphics")))))
