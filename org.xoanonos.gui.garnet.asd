
;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.


;;; Bad practice muddying up asdf package namespace.

(defpackage :org.xoanonos.asdf-app-config
  (:export :*base-directory*
	   :*garnet-load-truename*))


(asdf:defsystem :org.xoanonos.gui.garnet
  :depends-on (alexandria
	       uiop
	       bordeaux-threads
	       ;; trivial-features
	       ;;clx
	       )
  :license "MIT-ish (also public domain, see LICENSE)"
  :author "CMU Garnet Team (plus various others, see LICENSE)"
  :description " GUI toolkit (c. 1990 look/feel)"
  :components
  ((:file "package")
   (:module utils
	    :pathname ""
	    :depends-on (package)
	    :components
	    (
	     (:file "src/utils/general")
	     ))
   (:module kr
   	    :pathname "src/kr"
   	    :depends-on (utils)
   	    :components
	    ((:file "kr-macros")
	     ;; (:file "kr-doc")
	     (:file "kr")
	     (:file "constraints" :depends-on (kr))))
      (:module opal-boot
   	    :pathname "src/opal"
   	    :depends-on (utils kr)
   	    :components
   	    ((:file "exports")
	     (:file "types")
	     (:file "update-constants")
	     (:file "macros")
	     (:file "defs")
	     (:file "new-defs")
	     (:file "utils")
	     (:file "create-instances")))
   (:module opal
   	    :pathname "src/opal"
   	    :depends-on (utils kr opal-boot)
   	    :components
	    ((:file "basics")
	     (:file "aggregates")
	     ))
   (:module aggregadgets
   	    :pathname "src/aggregadgets"
   	    :depends-on (utils kr opal)
   	    :components
	    ((:file "agg-macros")
	     (:file "aggregadgets")))
   (:module multi-garnet
	    :pathname "multi-garnet"
	    :depends-on (aggregadgets)
	    :serial t
	    :components
	    ((:file "package")
	     (:file "sky-blue")
	     (:file "multi-garnet")
	     (:file "scatterplot-alt")
	     ))))
