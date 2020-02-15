
(asdf:defsystem :org.xoanonos.gui.garnet
  :license "MIT-ish (also public domain, see LICENSE)"
  :author "CMU Garnet Team (plus various others, see LICENSE)"
  :description " GUI toolkit (c. 1990 look/feel)"
  :components
  ((:file "package")
   (:module kr
   	    :pathname "src/kr"
   	    :depends-on (package)
   	    :components
	    ((:file "kr-macros")
	     (:file "kr")
	     (:file "constraints" :depends-on (kr))))
   (:module opal-boot
   	    :pathname "src/opal"
   	    :depends-on (kr)
   	    :components
   	    ((:file "types")
	     (:file "defs")
	     ))
   (:module multi-garnet
	    :pathname "multi-garnet"
	    :depends-on (opal-boot)
	    :serial t
	    :components
	    ((:file "sky-blue")
	     (:file "multi-garnet")
	     (:file "scatterplot-alt")
	     ))))
