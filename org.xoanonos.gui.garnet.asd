
;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.


;;; Bad practice muddying up asdf package namespace.

(defpackage :org.xoanonos.asdf-app-config
  (:export :*base-directory*
	   :*garnet-load-truename*))

;; (defparameter org.xoanonos.asdf-app-config:*base-directory*
;;   (make-pathname :name nil :type nil :defaults *load-truename*))

;; (defparameter org.xoanonos.asdf-app-config:*garnet-load-truename*
;;   org.xoanonos.asdf-app-config:*base-directory*)


(asdf:defsystem :org.xoanonos.gui.garnet
  :depends-on (alexandria
	       uiop
	       bordeaux-threads
	       cl-aa
	       cl-aa-misc
	       cl-fad
	       trivial-features
	       clx
	       )
  :license "MIT-ish (also public domain, see LICENSE)"
  :author "CMU Garnet Team (plus various others, see LICENSE)"
  :description " GUI toolkit (c. 1990 look/feel)"
  :components
  ((:file "package")
   (:file "clx-compatability" :depends-on (package))
   (:module utils
	    :pathname ""
	    :depends-on (package clx-compatability)
	    :components
	    ((:file "garnet-loader")
	     (:file "src/utils/general")
	     (:file "src/utils/global")))
   (:module kr
   	    :pathname "src/kr"
   	    :depends-on (utils)
   	    :components
	    ((:file "kr-macros")
	     (:file "kr-doc")
	     (:file "kr")
	     (:file "constraints" :depends-on (kr))))
   (:module gem
   	    :pathname "src/gem"
   	    :depends-on (kr opal-boot)
   	    :components
	    ((:file "gem")
	     (:file "define-methods")
	     (:file "x")))
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
	     (:file "text-fonts")
	     (:file "create-instances")))
   (:module opal
   	    :pathname "src/opal"
   	    :depends-on (utils gem kr opal-boot)
   	    :components
	    ((:file "text-functions")
	     (:file "text")
	     (:file "update-basics")
	     (:file "halftones")
	     (:file "objects")
	     (:file "roundtangles")
	     (:file "basics")
	     (:file "aggregates")
	     (:file "process")
	     (:file "clean-up")
	     (:file "windows")
	     (:file "update")
	     (:file "fast-redraw")
	     (:file "update-window")
	     (:file "multifont")
	     (:file "virtual-aggregates")
	     (:file "pixmaps")
	     (:file "open-and-close")
	     (:file "opal-init")))
   (:module aggregadgets
   	    :pathname "src/aggregadgets"
   	    :depends-on (utils gem kr opal)
   	    :components
	    ((:file "aggregadgets-compiler")
	     (:file "agg-macros")
	     (:file "agg-utils")
	     (:file "aggregadgets")
	     (:file "aggrelists")
	     (:file "add-agg")
	     (:file "agg-fix-slots")
	     (:file "copy-agg")
	     (:file "string-edit")
	     (:file "agg-labels")
	     ))
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
