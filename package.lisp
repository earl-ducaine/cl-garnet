
(defpackage :garnet-utils
  (:use :common-lisp)
  (:nicknames :gu)
  (:export *garnet-break-key*
	   2pi -2pi
	   add-to-list
	   black
	   directory-p
	   do2lists
	   dolist2
	   m
	   m1
	   pi/2
	   pi3/2
	   safe-functionp
	   short-pi
	   str
	   string+
	   till
	   until
	   verify-binding
	   white
	   while))

(defpackage :kr-debug
  (:use :common-lisp))

(defpackage :kr
  (:use :common-lisp :kr-debug)
  (:export schema
	   create-instance
	   create-prototype
	   create-relation
	   create-schema
	   formula
	   o-formula
	   schema-p
	   relation-p
	   is-a-p
	   has-slot-p
	   formula-p
	   s-value
	   g-value
	   g-cached-value
	   g-local-value
	   gv
	   gvl
	   gv-local
	   get-value
	   get-local-value
	   dovalues
	   doslots
	   define-method
	   kr-send
	   call-prototype-method
	   apply-prototype-method
	   method-trace
	   *print-as-structure*
	   with-constants-disabled
	   with-types-disabled
	   with-demons-disabled
	   with-demon-disabled
	   with-demon-enabled
	   change-formula
	   move-formula
	   recompute-formula
	   copy-formula
	   kr-path
	   mark-as-changed
	   mark-as-invalid
	   ps
	   call-on-ps-slots
	   name-for-schema
	   declare-constant
	   slot-constant-p
	   destroy-slot
	   destroy-schema
	   destroy-constraint
	   def-kr-type
	   g-type
	   s-type
	   check-slot-type
	   kr-boolean
	   get-slot-doc
	   set-slot-doc
	   get-type-documentation
	   set-type-documentation
	   get-type-definition
	   get-declarations
	   get-slot-declarations
	   g-formula-value
	   s-formula-value
	   self-old-value))

(defpackage :jewel
  (:use :common-lisp :kr :kr-debug)
  (:export *exposure-event-mask*
	   *fixed-font-family*
	   *large-font-point-size*
	   *large-font-size*
	   *medium-font-point-size*
	   *medium-font-size*
	   *sans-serif-font-family*
	   *serif-font-family*
	   *small-font-point-size*
	   *small-font-size*
	   *very-large-font-point-size*
	   *very-large-font-size*
	   *black*
	   *color-screen-p*
	   *function-alist*
	   *read-write-colormap-cells-p*
	   *screen-height*
	   *screen-width*
	   *update-lock*
	   *white*
	   device-info
	   copy-display-info
	   default-font-from-file
	   display-info
	   display-info-display
	   display-info-filling-style-gc
	   display-info-line-style-gc
	   display-info-root-window
	   display-info-screen
	   init-device
	   make-display-info))

(defpackage :gem
  (:use :common-lisp :kr :kr-debug)
  (:export *exposure-event-mask*
	   *fixed-font-family*
	   *large-font-point-size*
	   *large-font-size*
	   *medium-font-point-size*
	   *medium-font-size*
	   *sans-serif-font-family*
	   *serif-font-family*
	   *small-font-point-size*
	   *small-font-size*
	   *very-large-font-point-size*
	   *very-large-font-size*
	   *black*
	   *color-screen-p*
	   *function-alist*
	   *read-write-colormap-cells-p*
	   *screen-height*
	   *screen-width*
	   *update-lock*
	   *white*
	   device-info
	   copy-display-info
	   default-font-from-file
	   display-info
	   display-info-display
	   display-info-filling-style-gc
	   display-info-line-style-gc
	   display-info-root-window
	   display-info-screen
	   init-device
	   make-display-info)
  (:import-from :garnet-utils :black)
  (:import-from :garnet-utils :white))

(defpackage :interactors
  (:use :common-lisp :kr)
  (:nicknames :inter)
  (:import-from :garnet-utils *garnet-break-key*)
  (:export *garnet-break-key*
	   *left-button* *trans-from-file*
	   ;; for animation
	   start-animator Stop-Animator abort-animator animator-interactor
	   animator-wrap animator-bounce Reset-All-Timer-Processes
	   ;; entering and leaving main event loop
	   main-event-loop
	   exit-main-event-loop
	   ;; waiting for an interaction to complete
	   Interaction-Complete Wait-Interaction-Complete
	   ;;explicit control of interactors
	   Change-Active Start-Interactor Abort-Interactor Stop-Interactor
	   ;; Called by KR when :active or :window changes:
	   Notice-Interactor-Slot-Changed
	   ;; support for multiple priority levels
	   priority-level normal-priority-level high-priority-level
	   running-priority-level priority-level-list
	   ;; the next ones are for debugging
	   Reset-Inter-Levels Print-Inter-Levels Print-Inter-Windows
	   trace-inter untrace-inter *debug-next-inter* Do-All-Interactors
	   ;; interactor event structure (copy of X's event structure)
	   *Current-Event*
	   event-x
	   event-y
	   event-char
	   event-code
	   event-mousep
	   event-downp
	   event-window
	   event-timestamp
	   make-event
	   ;; for controlling double clicks
	   *double-click-time*
	   ;; key translations for text-inter
	   Bind-Key Unbind-Key Unbind-All-Keys Set-Default-Key-Translations
	   ;;transcripting functions
	   Transcript-Events-To-File Close-Transcript
	   Transcript-Events-From-File
	   ;; useful utility functions
	   Clip-And-Map Beep Insert-Text-Into-String Warp-Pointer
	   Pop-Up-Win-And-Start-Interactor
	   ;; functions for dealing with selection for button and menu
	   Return-Final-Selection-Objs DeSelectObj SelectObj
	   ;; the various exported interactor types
	   interactor interactor-window button-interactor text-interactor
	   two-point-interactor move-grow-interactor menu-interactor
	   angle-interactor scroll-wheel-interactor
	   ;; Export these if debugging is enabled.
	   #+garnet-debug *int-debug* #+garnet-debug *int-trace*
	   ))

(defpackage :demo
  (:use :common-lisp :kr))

(defpackage :garnet-user
  (:use common-lisp kr garnet-utils)
  (:export go-demos))

(defpackage :garnet-truetype
  (:nicknames :garnet-xft)
  (:use :cl :kr)
  (:export
   :font
   :font-family
   :font-subfamily
   :font-size
   :font-underline
   :font-strikethrough
   :font-overline
   :font-background
   :font-foreground
   :font-overwrite-gcontext
   :cache-font-file
   :*font-dirs*
   :drawable-screen
   :font-ascent
   :font-descent
   :text-bounding-box
   :xmin
   :ymin
   :xmax
   :ymax
   :screen-default-dpi
   :screen-dpi
   :draw-text
   :draw-text-line
   :get-font-families
   :get-font-subfamilies
   :text-height
   :text-width
   :text-line-bounding-box
   :text-line-width
   :text-line-height
   :font-line-gap
   :baseline-to-baseline
   :font-antialias
   :font-lines-height
   :cache-fonts
   :font-equal)
  (:documentation "Package contains API for TrueType text rendering
                   using CLX, XRender.  Glyphs information is obtained
                   by ZPB-TTF. Font rasterization is made by
                   CL-VECTORS."))

(defpackage :garnet-gadgets
  (:use :common-lisp :kr)
  (:nicknames :gg)
  (:export
   :mouseline-go
   :mouseline-stop))

(defpackage :test
  (:use :common-lisp))

(defpackage :opal
  (:use :common-lisp :kr)
  (:import-from :garnet-utils :black)
  (:import-from :garnet-utils :white))

(defpackage :garnet-debug
  (:use :common-lisp :kr :opal) (:nicknames :gd))

(defpackage :gilt
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :c32
  (:use common-lisp kr))

(defpackage :lapidary
  (:use common-lisp kr)
  (:export do-go do-stop fix-it clean-up))

(defpackage :agate
  (:use :common-lisp :kr))

(defpackage :demo-3d
  (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :xomax
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-multiwin
  (:use kr common-lisp)
  (:export do-go do-stop))

(defpackage :demo-multifont
  (:use common-lisp kr)
  (:export do-go do-stop))

(defpackage :demo-animator (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-angle (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-othello (:use :kr :common-lisp) (:nicknames :doth)
	    (:export do-go do-stop start-game stop-game set-score))

(defpackage :demo-pixmap (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-arith (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-schema-browser (:use :common-lisp :kr)
	    (:export do-go do-stop schema-browser schema-browser-win
		     schema-browser-top-agg))
(defpackage :demo-array (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-scrollbar (:use :common-lisp :kr)
	    (:export do-go do-stop
		     mac-obj mac-go mac-stop
		     open-obj open-go open-stop
		     next-obj next-go next-stop
		     motif-obj motif-go motif-stop))
(defpackage :demo-clock (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-sequence (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-editor (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-text (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-file-browser (:use :common-lisp :kr)
	    (:export do-go do-stop file-browser file-browser-win
		     file-browser-top-agg))
(defpackage :demo-truck (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-gadgets (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-twop (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-gesture (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-unistrokes
  (:use :common-lisp :kr :inter)
  (:export do-go do-stop))

(defpackage :demo-graph (:use :common-lisp :kr)
	    (:export do-go do-stop schema-graph
		     demo-graph-error-gadget root-box relayout
		     demo-graph-win))

(defpackage :demo-virtual-agg
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-grow (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-xasperate
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-logo
  (:use :common-lisp :kr)
  (:export do-go do-stop re-animate))

(defpackage :demos-controller
  (:use :common-lisp :kr)
  (:export do-go do-stop message))

(defpackage :demo-manyobjs
  (:use :common-lisp :kr)
  (:export do-go do-stop move))

(defpackage :demo-menu (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :garnet-calculator (:use :common-lisp :kr)
	    (:export start-calc stop-calc do-go do-stop))

(defpackage :demo-mode (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :garnetdraw (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-motif (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :mge (:use :common-lisp :kr)
	    (:export do-go do-stop
		     create-piece destroy-piece destroy-all-pieces
		     go-initialize editor-show-window))

(defpackage :demo-moveline (:use :kr :common-lisp) (:export do-go do-stop))
