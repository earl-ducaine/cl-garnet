
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
	     ;; (:file "constraints" :depends-on (kr))
	     ))
   (:module multi-garnet
	    :pathname "multi-garnet"
	    :depends-on (kr)
	    :serial t
	    :components
	    ((:file "sky-blue")
	     (:file "multi-garnet")
	     (:file "scatterplot-alt")))))
