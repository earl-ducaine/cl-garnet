
;;; This file was written by Earl Ducaine and is released under the
;;; MIT license.


;;; Bad practice muddying up asdf package namespace.

(defpackage :org.xoanonos.asdf-app-config
  (:export :*base-directory*
	   :*garnet-load-truename*))

(defparameter org.xoanonos.asdf-app-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defparameter org.xoanonos.asdf-app-config:*garnet-load-truename*
  org.xoanonos.asdf-app-config:*base-directory*)

(setf (get :garnet-modules :inter) t)
(setf (get :garnet-modules :multifont) t)

(asdf:defsystem :org.xoanonos.gui.garnet
  :depends-on (alexandria
	       uiop
	       bordeaux-threads
	       cl-aa
	       cl-aa-misc
	       cl-fad
	       cl-ppcre
	       cl-store
	       ;; cl-vectors
	       trivial-features
	       clx
	       ;; trivial-dump-core
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
   (:module inter
   	    :pathname "src/inter"
   	    :depends-on (utils gem kr opal)
   	    :components
	    ((:file "garnet-keytrans")
	     (:file "define-mouse-keys")
	     (:file "x-define-keys")
	     (:file "x-inter")
	     (:file "interactors")
	     (:file "accelerators")
	     (:file "animation-process")
	     (:file "i-windows")
	     (:file "menuinter")
	     (:file "movegrowinter")
	     (:file "buttoninter")
	     (:file "twopointinter")
	     (:file "textkeyhandling")
	     (:file "lispkeyhandling")
	     (:file "textinter")
	     (:file "multifont-textinter")
	     (:file "focus-multifont-textinter")
	     (:file "selection-interactor")
	     (:file "angleinter")
	     (:file "animatorinter")))
   (:module ps
   	    :pathname "src/ps"
   	    :depends-on (utils gem kr opal inter)
   	    :components
	    ((:file "ps-compiler")
	     (:file "ps")
	     (:file "ps-multifont")))
   (:module aggregadgets
   	    :pathname "src/aggregadgets"
   	    :depends-on (utils gem kr opal inter ps)
   	    :components
	    ((:file "aggregadgets-compiler")
	     (:file "agg-macros")
	     (:file "agg-utils")
	     (:file "aggregadgets")
	     (:file "aggrelists")
	     (:file "add-agg")
	     (:file "agg-fix-slots")
	     (:file "copy-agg")
	     (:file "save-agg")
	     (:file "string-edit")
	     (:file "agg-labels")
	     (:file "rectangle-conflict-object")
	     (:file "aggregraphs")
	     (:file "scalable-aggregraph")
	     (:file "scalable-aggregraph-image")))
   ;; (:module gadgets
   ;; 	    :pathname "src/gadgets"
   ;; 	    :depends-on (utils gem kr opal inter ps aggregadgets)
   ;; 	    :components
   ;; 	    (;; (:file "gadgets-compiler")
   ;; 	     ;; (:file "GAD-scroll-parts")	; Helper modules containing definitions for
   ;; 	     ;; (:file "GAD-slider-parts")	; scroll bar and slider objects
   ;; 	     ;; (:file "GAD-v-arrows")
   ;; 	     ;; (:file "GAD-v-boxes")
   ;; 	     ;; (:file "GAD-h-arrows")
   ;; 	     ;; (:file "GAD-h-boxes")
   ;; 	     ;; (:file "v-scroll-bar")
   ;; 	     ;; (:file "h-scroll-bar")
   ;; 	     ;; (:file "v-slider")
   ;; 	     ;; (:file "h-slider")
   ;; 	     ;; (:file "trill-device")	; A horizontal slider without the shaft
   ;; 	     ;; (:file "GAD-button-parts")	; Helper module for button and menu objects
   ;; 	     ;; (:file "x-buttons")
   ;; 	     ;; (:file "text-buttons")
   ;; 	     ;; (:file "radio-buttons")
   ;; 	     ;; (:file "error-gadget-utils")
   ;; 	     ;; (:file "error-gadget")
   ;; 	     ;; (:file "scrolling-menu")
   ;; 	     ;; (:file "scrolling-input-string")
   ;; 	     ;; (:file "scrolling-labeled-box")
   ;; 	     ;; (:file "scrolling-unlabeled-box")
   ;; 	     ;; (:file "gauge")		   ; Semi-circular gauge
   ;; 	     ;; (:file "menu")
   ;; 	     ;; (:file "labeled-box")	   ; A box with editable text and a label
   ;; 	     ;; (:file "arrow-line")	   ; A line/arrowhead combination
   ;; 	     ;; (:file "graphics-selection")  ; Selection squares for move-grow interaction
   ;; 	     ;; (:file "option-button")
   ;; 	     ;; (:file "popup-menu-button")
   ;; 	     ;; (:file "save-load-functions")
   ;; 	     ;; (:file "save-gadget")
   ;; 	     ;; (:file "load-gadget")
   ;; 	     ;; (:file "browser-gadget")
   ;; 	     ;; (:file "polyline-functions")
   ;; 	     ;; (:file "polyline-creator")
   ;; 	     ;; (:file "multi-selection")
   ;; 	     ;; (:file "menubar-functions")
   ;; 	     ;; (:file "menubar")
   ;; 	     ;; (:file "scrolling-window-parts")
   ;; 	     ;; (:file "scrolling-window")
   ;; 	     ;; (:file "prop-value-gadgets")
   ;; 	     ;; (:file "prop-sheet")
   ;; 	     ;; (:file "prop-sheet-win")
   ;; 	     ;; (:file "motif-parts")
   ;; 	     ;; (:file "motif-v-scroll-bar")
   ;; 	     ;; (:file "motif-h-scroll-bar")
   ;; 	     ;; (:file "motif-trill-device")
   ;; 	     ;; (:file "motif-slider")
   ;; 	     ;; (:file "motif-text-buttons")
   ;; 	     ;; (:file "motif-check-buttons")
   ;; 	     ;; (:file "motif-radio-buttons")
   ;; 	     ;; (:file "motif-menu")
   ;; 	     ;; (:file "motif-gauge")
   ;; 	     ;; (:file "motif-scrolling-labeled-box")
   ;; 	     ;; (:file "motif-prop-sheet-win")
   ;; 	     ;; (:file "motif-scrolling-window")
   ;; 	     ;; (:file "motif-error-gadget")
   ;; 	     ;; (:file "motif-option-button")
   ;; 	     ;; (:file "motif-scrolling-menu")
   ;; 	     ;; (:file "motif-save-gadget")
   ;; 	     ;; (:file "motif-load-gadget")
   ;; 	     ;; (:file "motif-menubar")
   ;; 	     ;; (:file "multifont-gadget")
   ;; 	     ;; (:file "scrolling-window-multifont")
   ;; 	     ;; (:file "standard-edit")
   ;; 	     ;; (:file "mouseline")
   ;; 	     ))
   (:module lapidary2
	    :pathname "src/lapidary"
	    :depends-on (utils)
	    :components
	    ((:file "lapidary-compiler")
	     (:file "lapidary-functions-loader")
	     ;; (:file "lapidary-functions")
	     ;; (:file "mouse-bindings")
	     ))
   (:module multi-garnet
	    :pathname "multi-garnet"
	    :depends-on (lapidary2)
	    :serial t
	    :components
	    ((:file "package")
	     (:file "sky-blue")
	     (:file "multi-garnet")
	     (:file "scatterplot-alt")
	     ))))
