
(asdf:defsystem :org.xoanonos.gui.garnet
  :license "MIT-ish (also public domain, see LICENSE)"
  :author "CMU Garnet Team (plus various others, see LICENSE)"
  :description " GUI toolkit (c. 1990 look/feel)"
  :components
  ((:file "package")
   (:module multi-garnet
	    :pathname "multi-garnet"
	    :serial t
	    :components
	    ((:file "kr-macros")
	     (:file "kr")
	     (:file "sky-blue")
	     (:file "multi-garnet")
	     (:file "scatterplot-alt")))))
