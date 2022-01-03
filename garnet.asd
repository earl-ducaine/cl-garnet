
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

(asdf:defsystem :garnet
  :depends-on (alexandria
	       uiop
	       bordeaux-threads
	       cl-aa
	       cl-aa-misc
	       cl-fad
	       cl-ppcre
	       cl-store
	       cl-vectors
	       trivial-features
	       clx
	       trivial-dump-core
           trivial-garbage)
  :license "MIT-ish (also public domain, see LICENSE)"
  :author "CMU Garnet Team (plus various others, see LICENSE)"
  :description " GUI toolkit (c. 1990 look/feel)"
  :components
  ((:file "post-processing"
	  :depends-on
	  (opal inter ps aggregadgets gadgets debug protected-eval
		gesture demos garnet-desktop-lab lapidary c32 gilt
		multi-garnet lapidary cl-processing))
   (:file "package")
;   (:file "clx-compatability" :depends-on (package))
   (:module utils
	    :pathname ""
	    :depends-on (package #-(and)clx-compatability)
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
   (:module gadgets
   	    :pathname "src/gadgets"
   	    :depends-on (utils gem kr opal inter ps aggregadgets)
   	    :components
	    ((:file "gadgets-compiler")
	     (:file "GAD-scroll-parts")	; Helper modules containing definitions for
	     (:file "GAD-slider-parts")	; scroll bar and slider objects
	     (:file "GAD-v-arrows")
	     (:file "GAD-v-boxes")
	     (:file "GAD-h-arrows")
	     (:file "GAD-h-boxes")
	     (:file "v-scroll-bar")
	     (:file "h-scroll-bar")
	     (:file "v-slider")
	     (:file "h-slider")
	     (:file "trill-device")	; A horizontal slider without the shaft
	     (:file "GAD-button-parts")	; Helper module for button and menu objects
	     (:file "x-buttons")
	     (:file "text-buttons")
	     (:file "radio-buttons")
	     (:file "error-gadget-utils")
	     (:file "error-gadget")
	     (:file "scrolling-menu")
	     (:file "scrolling-input-string")
	     (:file "scrolling-labeled-box")
	     (:file "scrolling-unlabeled-box")
	     (:file "gauge")		   ; Semi-circular gauge
	     (:file "menu")
	     (:file "labeled-box")	   ; A box with editable text and a label
	     (:file "arrow-line")	   ; A line/arrowhead combination
	     (:file "graphics-selection")  ; Selection squares for move-grow interaction
	     (:file "option-button")
	     (:file "popup-menu-button")
	     (:file "save-load-functions")
	     (:file "save-gadget")
	     (:file "load-gadget")
	     (:file "browser-gadget")
	     (:file "polyline-functions")
	     (:file "polyline-creator")
	     (:file "multi-selection")
	     (:file "menubar-functions")
	     (:file "menubar")
	     (:file "scrolling-window-parts")
	     (:file "scrolling-window")
	     (:file "prop-value-gadgets")
	     (:file "prop-sheet")
	     (:file "prop-sheet-win")
	     (:file "motif-parts")
	     (:file "motif-v-scroll-bar")
	     (:file "motif-h-scroll-bar")
	     (:file "motif-trill-device")
	     (:file "motif-slider")
	     (:file "motif-text-buttons")
	     (:file "motif-check-buttons")
	     (:file "motif-radio-buttons")
	     (:file "motif-menu")
	     (:file "motif-gauge")
	     (:file "motif-scrolling-labeled-box")
	     (:file "motif-prop-sheet-win")
	     (:file "motif-scrolling-window")
	     (:file "motif-error-gadget")
	     (:file "motif-option-button")
	     (:file "motif-scrolling-menu")
	     (:file "motif-save-gadget")
	     (:file "motif-load-gadget")
	     (:file "motif-menubar")
	     (:file "multifont-gadget")
	     (:file "scrolling-window-multifont")
	     (:file "standard-edit")
	     (:file "mouseline")))
   (:module debug
   	    :pathname "src/debug"
   	    :depends-on (utils gem kr opal ps gadgets)
   	    :components
	    ((:file "debug-fns")
	     (:file "objsize")
	     (:file "inspector")
	     (:file "suggest-constants")))
   (:module protected-eval
   	    :pathname "src/protected-eval"
   	    :depends-on (utils gem kr opal ps gadgets debug)
   	    :components
	    ((:file "protected-eval-compiler")
	     (:file "error")
	     (:file "prompter")
	     (:file "protected-eval")
	     (:file "protected-process")
	     (:file "abstract-errors")
	     (:file "garnet-errors")))
   (:module gesture
   	    :pathname "src/gesture"
   	    :depends-on (utils gem kr opal ps gadgets protected-eval)
   	    :components
	    ((:file "gesture-compiler")
	     (:file "features")
	     (:file "matrix")
	     (:file "classify")
	     (:file "gestureinter")
	     (:file "fileio")
	     (:file "train")
	     (:file "agate")))
   (:module demos
   	    :pathname "src/demos"
   	    :depends-on (:utils :gem :kr :opal :ps :gadgets :protected-eval
				:gesture)
   	    :components
	    ((:file "demos")
	     (:file "demo-3d")
	     (:file "demo-angle")
	     (:file "demo-animator")
	     (:file "demo-arith")
	     (:file "demo-array")
	     (:file "garnet-calculator")
	     (:file "demo-virtual-agg")
	     (:file "demo-clock")
	     (:file "demo-editor")
	     (:file "demo-file-browser")
	     (:file "demo-gadgets")
	     (:file "demo-gesture")
	     (:file "demo-graph")
	     (:file "demo-grow")
	     (:file "demo-logo")
	     (:file "demo-manyobjs")
	     (:file "demo-menu")
	     (:file "demo-mode")
	     (:file "demo-motif")
	     (:file "demo-moveline")
	     (:file "demo-multifont")
	     (:file "demo-multiwin")
	     (:file "demo-pixmap")
	     (:file "demo-schema-browser")
	     (:file "demo-scrollbar")
	     (:file "demo-sequence")
	     (:file "demo-text")
	     (:file "demo-truck")
	     (:file "demo-twop")
	     (:file "mge")
	     (:file "demo-othello")
	     (:file "demo-xasperate")
	     (:file "demo-unistrokes")
	     (:file "garnetdraw")
	     (:file "demos-controller")
	     (:file "tour")
	     (:file "tour-transcript")))
   (:module garnet-desktop-lab
	    :pathname "src/contrib/garnet-desktop-lab"
	    :depends-on (demos)
	    :components
	    ((:file "package")
	     (:file "garnet-desktop-lab")
	     (:file "xomax")
	     (:file "app-launcher")))
   (:module lapidary2
   	    :pathname "src/lapidary"
   	    :depends-on (utils)
   	    :components
   	    ((:file "lapidary-compiler")
	     (:file "lapidary-functions-loader")
   	     (:file "lapidary-functions")
   	     (:file "mouse-bindings")))
   (:module c32
   	    :pathname "src/c32"
   	    :depends-on (:gilt)
   	    :components
   	    (
;;	     (:file "c32-compiler")
	     (:file "c32")
	     (:file "c32formula")
	     (:file "c32ref")
	     (:file "pop-up-generalize")
	     (:file "pop-up-copy-formula")
	     (:file "pop-up-ask-object")
	     (:file "pop-up-functions")
	     (:file "c32dialog")
	     (:file "c32-lapidary")))
   (:module gilt
   	    :pathname "src/gilt"
   	    :depends-on (:gadgets)
   	    :components
   	    ((:file "gilt-compiler")
	     (:file "gilt-functions")
	     (:file "path-functions")
	     (:file "filter-functions")
	     (:file "gilt-font-imp")
	     (:file "motif-gilt-font-props")
	     (:file "gilt-gadget-utils")
	     (:file "gilt-gadgets")
	     (:file "motif-gilt-gadgets")
	     (:file "gilt")
	     (:file "motif-gilt-save")
	     (:file "motif-gilt-read")
	     (:file "color-imp")
	     (:file "motif-color-props")
	     (:file "line-imp")
	     (:file "motif-line-props")
	     (:file "fill-imp")
	     (:file "motif-fill-props")
	     (:file "align")
	     (:file "value-control")
	     (:file "enable-control")
	     (:file "error-check")))
   (:module multi-garnet
	    :pathname "multi-garnet"
	    :depends-on (lapidary)
	    :serial t
	    :components
	    ((:file "package")
	     (:file "sky-blue")
	     (:file "multi-garnet")
	     (:file "scatterplot")
	     ;; examples fires off the demo app.
	     ;; (:file "examples")
	     ))
   (:module lapidary
	    :pathname "src/lapidary"
	    :depends-on (lapidary2 gadgets gilt c32)
	    :components
	    ((:file "mouse-bindings")
	     (:file "parameters")
	     (:file "defs")
	     (:file "macros")
	     (:file "constraint-gadget-compiler")
	     (:file "cg-defs")
	     (:file "support-constraints")
	     (:file "custom")
	     (:file "attach-constraints")
	     (:file "support-box-constraints")
	     (:file "box-parts")
	     (:file "box")
	     (:file "line-constraint-defs")
	     (:file "line-constraint-objs")
	     (:file "line-constraint")
	     (:file "set-feedback")
	     (:file "lapidary" :depends-on (box))
	     (:file "dialog-parts2")
	     (:file "event-card")
	     (:file "card")
	     (:file "card1")
	     (:file "start-where")
	     (:file "prompt")
	     (:file "lapidary-objects")
	     (:file "feedback-objs")
	     (:file "support-misc")
	     (:file "support-selection1")
	     (:file "support-selection2")
	     (:file "selection")
	     (:file "create-object")
	     (:file "delete-object")
	     (:file "delete-window")
	     (:file "move-grow")
	     (:file "aggregates")
	     (:file "aggparam")
	     (:file "create-parameters")
	     (:file "properties")
	     (:file "line-imp")
	     (:file "line-props")
	     (:file "fill-imp")
	     (:file "fill-props")
	     (:file "color-imp")
	     (:file "color-props")
	     (:file "shapes")
	     (:file "lap-draw")
	     (:file "support-menu-editor")
	     (:file "new-editor")
	     (:file "text")
	     (:file "text-properties")
	     (:file "gadgetcopy")
	     (:file "save-link-parameters")
	     (:file "lapidary-save")
	     (:file "lapidary-read")
	     (:file "support-save-restore")
	     (:file "save-restore")
	     (:file "add-gadget")
	     (:file "choice-inter")
	     (:file "text-inter")
	     (:file "move-grow-box")
	     (:file "support-move-grow-inter")
	     (:file "move-grow-inter")
	     (:file "angle-inter")
	     (:file "two-point-inter")
	     (:file "support-inter")
	     (:file "by-demo")
	     (:file "interactors")
	     (:file "interactors-menu")))
   (:module contrib
	    :pathname "src/contrib"
	    :depends-on (:debug :lapidary)
	    :components
	    ((:file "aggretrees")
	     (:file "plot-2d")
	     (:file "plotxy")
	     (:file "graph-editor/graph-editor")))
   (:module cl-processing
	    :pathname "src/cl-processing"
	    :depends-on (:debug :lapidary)
	    :components
	    ((:file "package")
	     (:file "main")
	     (:file "console")))
   (:module last
	    :pathname ""
	    :depends-on (:utils :kr :gem :opal :inter :ps :aggregadgets
				:gadgets :debug :protected-eval :gesture
				:demos :garnet-desktop-lab :lapidary2
				:c32 :gilt :multi-garnet :lapidary
				:cl-processing)
	    :components
	    ((:file "post-processing")))))

(asdf:defsystem :garnet/clx-debug
  :depends-on (garnet)
  :license "MIT-ish (also public domain, see LICENSE)"
  :author "CMU Garnet Team (plus various others, see LICENSE)"
  :description "CLX debugging tools"
  :components
  ((:module debug-clx
	    :pathname "debug"
	    :components
	    ((:file "debug")
	     (:file "describe")
	     (:file "event-test")
	     (:file "keytrans")
	     (:file "trace")
	     (:file "util")))))
