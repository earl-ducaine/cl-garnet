;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Designed and implemented by Brad Myers

#|
============================================================
Change log:
     1/05/95 Andrew Mickish - Switched order of parameters to garnet-pathnames
               for bitmaps and pixmaps.
     8/27/93 Andrew Mickish - Put formulas in :image slots of pixmaps so that
              they can be recomputed during opal:reconnect-garnet
     7/01/93 Andrew Mickish - Changed values of :loaded slots to keywords;
              Called load-extra-motif-gadgets for Lucid
     5/26/93 Andrew Mickish - Added constant declarations
     4/23/93 Andrew Mickish - Made HourGlass cursor standard; added
              :do-not-dump-objects to MOTIF-TRILL-DEVICE, MOTIF-SCROLLING-MENU
     3/18/93 Brad Myers - known-as-type.  Moved popup funcs to gadget files.
     2/17/92 Brad Myers - motif- and gilt-gadgets are just the gadget windows
                       - support for pop-up gadgets, like menubars
    12/01/92 Andrew Mickish - Added :constant declaration to all maker
               functions (and removed it from Create-New-Gadget).
    11/05/92 Andrew Mickish - Added filter buttons to main menu; omitted
               interactors from palette gadgets and added :maker functions
    08/19/92 Andrew Mickish - Bitmap pathnames now use namestring function
      4/4/92 Brad Myers - new specialrun and build forms
                          Add color as a property of the OK gadgets
    03/25/92 Andrew Mickish - Removed :slots-to-copy list from TYPE-BITMAP
               because :image is already copied during copy-gadget; changed
               :properties-slots list of TYPE-BITMAP to add Invalid-Pathname-p.
    03/01/92 Brad Myers - make gadgets in palette window be constant
    02/09/92 Brad Myers - made more constants, and made more props be
               multiple choice; moved common functions to gilt-gadget-utils
    01/06/91 Andrew Mickish - Instead of adding type names to
               opal:*standard-names* with setf in this file, the names now
               appear in the defparameter in save-agg.lisp.
    11/23/91 Andrew Mickish - Added :background-color
    10/08/91 Andrew Mickish - Added fast-redraw to "Selected Object" gadget
    07/18/91 Andrew Mickish - Removed :xor from "Selected Object" field
    05/14/91 Andrew Mickish - Added :text-inter slot to motif-scrolling-
               labeled-boxes
    05/09/91 Andrew Mickish - Added :active-p properties to type schemas
    04/11/91 Brad Myers - Fixed bug in function-for-ok-name for save
    04/02/91 Andrew Mickish - Added comments to :accelerators, :inactive-items
               property slots of type-motif-menu
    03/28/91 Andrew Mickish - Changed :button-diameter property to :button-width
    03/27/91 Andrew Mickish - Removed :fixed-width-size and :fixed-height-size
               from property lists
    03/19/91 Andrew Mickish - Removed :item-to-string-function from
               TYPE-MOTIF-MENU because writing this value out to a file
               causes a "#k<" error when read back in
    03/19/91 Andrew Mickish - Set :function-for-ok-name's :active-p slot
               instead of :export-p's :active-p in Show-Save-Dialog
    03/17/91 Andrew Mickish - Fixed :active-p slots of main-menu labeled boxes
    03/14/91 Andrew Mickish - Changed :filling-style of Gray-Out
    03/13/91 Osamu Hashimoto - Moved Show-Save-Dialog & Show-Read-Dialog here
                               from gilt.lisp
    03/12/91 Osamu Hashimoto - Changed grayout from rect-covering to :active-p
    03/11/91 Osamu Hashimoto - Removed labeled-box and changed scrolling-text-box
                               to motif-scrolling-labeled-box
    03/07/91 Osamu Hashimoto - Moved *prop-sheet* here from gilt.lisp
    03/04/91 Osamu Hashimoto - Moved Make-Main-Menu from gilt.lisp
    03/04/91 Andrew Mickish - Updated properties slots according to manual
    02/28/91 Andrew Mickish - Added :min-width and :min-height to gadgets
               that grow
    02/27/91 Andrew Mickish - Moved *load-file* here from gilt.lisp;
               Moved IB-WINDOW here from gilt.lisp
    01/28/91 Andrew Mickish - Converted to Motif gadgets
    11/13/90 Brad Myers - Split from gilt.lisp
============================================================
|#


(in-package "GILT")


(declaim (special *work-win*))


;; load the gadgets needed for the palette that are not needed for
;; Gilt itself.
(defun load-extra-motif-gadgets ()
  ;; the commented out ones are used as pixmaps, so not needed unless used
  (dolist (pair '(
		  #+lucid (:motif-gauge "motif-gauge-loader")
		  #+lucid (:motif-scrolling-menu "motif-scrolling-menu-loader")
		  #+lucid (:motif-option-button "motif-option-button-loader")
		  (:motif-trill-device "motif-trill-device-loader")
		  ))		    
    (unless (get :garnet-modules (car pair))
      (common-lisp-user::garnet-load (concatenate 'string "gadgets:" (cadr pair))))))

;; The Lucid compiler performs some kind of optimization that replaces
;; the quoted gadget references with the constant values, and it will
;; complain if the compiled references are read without the gadgets being
;; defined.  You could take this out if you replaced the affected
;; gadgets with bitmaps.
#+lucid
(load-extra-motif-gadgets)
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now define the gadgets;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Make-Motif-Palette-Window ()
  (if (and *motif-ib-win*
	   (schema-p *motif-ib-win*))
      (progn
	(s-value *motif-ib-win* :visible T)
	(setq *ib-win* *motif-ib-win*)
	(s-value *work-win* :ib-win *ib-win*)
	*motif-ib-win*)

      ;; else have to create it
      (let ((win (create-instance NIL inter:interactor-window
		   (:left 550)(:top 300)(:width 484)(:height 385)
		   (:title "Gilt Motif Gadgets")
		   (:background-color opal:motif-gray))))

	(setq *ib-win* win)
	(opal:update *ib-win*)
	(when *work-win*
	  (s-value *work-win* :ib-win *ib-win*)
	  (opal:update *work-win*))
	(opal:With-HourGlass-Cursor
	  (s-value win :aggregate (create-instance NIL ib-objs
				    (:constant :visible)
				    (:widget-set :motif)
				    (:string "Motif Gadgets")))
	  (setq *motif-ib-win* win)
	  (load-extra-motif-gadgets)
	  (add-motif-gadgets win))
	win)))

(defun add-motif-gadgets (ib-win)
  (let ((agg (g-value ib-win :aggregate :selectable-objs)))

    (opal:add-components agg
			 
       (create-instance NIL opal:pixmap
	 (:image (o-formula (Get-Gilt-Pixmap "motif-menubar.xpm")))
	 (:loaded T)  ;; menubars are needed for gilt itself
	 
	 (:left 14)(:top 29)
	 (:constant T)

	 (:maker '((create-instance NIL garnet-gadgets:motif-menubar
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(15 29 NIL NIL))
			(:left (formula leftform))
			(:top (formula topform))
			(:items '(("File" NIL
				   (("Open...")("New")("Close")("Print")))
				 ("Edit" NIL
				   (("Cut")("Copy")("Paste")("Delete") ) )
				 ("Other" NIL
				   (("sub-label1")("sub-label2")))))))))

       (create-instance NIL garnet-gadgets:MOTIF-TEXT-BUTTON-PANEL
	    (:left 12)(:top 66)
	    (:constant T :value :keyboard-selection-p)
	    (:items '("Label1" "Label2" "Label3"))
	    (:loaded T)
	    (:interactors
	     `((:press :omit)
	       (:key :omit)))
	    (:maker '((create-instance NIL gg:MOTIF-TEXT-BUTTON-PANEL
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(12 66 NIL NIL))
			(:left (formula leftform))
			(:top (formula topform))
			(:items '("Label1" "Label2" "Label3"))
			))))
       
       (create-instance NIL garnet-gadgets:MOTIF-CHECK-BUTTON-PANEL
	    (:constant T :value :keyboard-selection-p)
	    (:left 92)(:top 85)
	    (:items '("Label1" "Label2" "Label3"))
	    (:loaded T)
	    (:interactors
	     `((:press :omit)
	       (:key :omit)))
	    (:maker '((create-instance NIL gg:MOTIF-CHECK-BUTTON-PANEL
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
		       (:constant T)
		       (:box '(92 85 NIL NIL))
		       (:left (formula leftform))
		       (:top (formula topform))
		       (:items '("Label1" "Label2" "Label3"))))))

       (create-instance NIL garnet-gadgets:MOTIF-RADIO-BUTTON-PANEL
	    (:left 90)(:top 168)
	    (:constant T :value :keyboard-selection-p)
	    (:items '("Label1" "Label2" "Label3"))
	    (:loaded T)
	    (:interactors
	     `((:press :omit)
	       (:key :omit)))
	    (:maker '((create-instance NIL gg:MOTIF-RADIO-BUTTON-PANEL
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
		       (:constant T)
		       (:box '(90 168 NIL NIL))
		       (:left (formula leftform))
		       (:top (formula topform))
		       (:items '("Label1" "Label2" "Label3"))))))

       (create-instance NIL garnet-gadgets:MOTIF-MENU
	 (:constant T :value :keyboard-selection-p)
	 (:items '("Label1" "Label2" "Label3"))
	 (:left 12)(:top 164)
	 (:loaded T)
	 (:interactors
	  `((:press :omit)
	    (:accel :omit)
	    (:key :omit)))
	 (:maker '((create-instance NIL gg:MOTIF-MENU
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		     (:constant T)
		     (:box '(12 164 NIL NIL))
		     (:left (formula leftform))
		     (:top (formula topform))
		     (:items '("Label1" "Label2" "Label3"))))))

       (create-instance NIL opal:pixmap
	 (:image (o-formula (Get-Gilt-Pixmap "motif-scroll-menu.xpm")))
	 (:loaded :motif-scrolling-menu)
	 (:load-file "motif-scrolling-menu-loader")
	 (:constant T)
	 (:left 12)(:top 253)
	 (:maker '((create-instance NIL garnet-gadgets::motif-scrolling-menu
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		     (:do-not-dump-objects :me)
		     (:constant T)
		     (:box '(12 253 NIL NIL))
		     (:left (formula leftform))
		     (:top (formula topform))))))

       (create-instance NIL opal:pixmap
	 (:image (o-formula (Get-Gilt-Pixmap "motif-option-button.xpm")))
	 (:loaded :motif-option-button)
	 (:load-file "motif-option-button-loader")
	 (:constant T)
	 (:left 117)(:top 340)
	 (:maker '((create-instance NIL GARNET-GADGETS::MOTIF-OPTION-BUTTON
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		     (:constant T)
		     (:box '(117 340 NIL NIL))
		     (:left (formula leftform))
		     (:top (formula topform))))))
       
       (create-instance NIL garnet-gadgets:MOTIF-TEXT-BUTTON-PANEL
	    (:Ok-Cancel-p T)
	    (:constant T :value :keyboard-selection-p)
	    (:left 113)(:top 254)
	    (:direction :horizontal)
	    (:items '("OK" "Cancel"))
	    (:text-offset 5)
	    (:loaded T)
	    (:interactors
	     `((:press :omit)
	       (:key :omit)))
	    (:maker '((create-instance NIL gg:MOTIF-TEXT-BUTTON-PANEL
			:declare ((:parameters T :known-as :except :items)
				  (:Type (known-as-type :known-as)))
			(:Ok-Cancel-p T)
			(:constant T)
			(:box '(113 254 NIL NIL))
			(:left (formula leftform))
			(:top (formula topform))
			(:direction :horizontal)
			(:items '("OK" "Cancel"))
			(:text-offset 5)
			(:final-feedback-p NIL)
			(:select-function 'OKCancel-Function)))))

       (create-instance NIL garnet-gadgets:MOTIF-TEXT-BUTTON-PANEL
	 (:constant T :value :keyboard-selection-p)
	 (:left 113)(:top 290)
	 (:direction :horizontal)
	 (:items '("OK" "Apply" "Cancel"))
	 (:text-offset 5)
	 (:loaded T)
	 (:interactors
	  `((:press :omit)
	    (:key :omit)))
	    (:maker '((create-instance NIL gg:MOTIF-TEXT-BUTTON-PANEL
			:declare ((:parameters T :known-as :except :items)
				  (:Type (known-as-type :known-as)))
			(:Ok-Cancel-p T)
			(:constant T)
			(:box '(113 290 NIL NIL))
			(:left (formula leftform))
			(:top (formula topform))
			(:direction :horizontal)
			(:items '("OK" "Apply" "Cancel"))
			(:text-offset 5)
			(:final-feedback-p NIL)
			(:select-function 'OKCancel-Function)))))

       (create-instance NIL garnet-gadgets:MOTIF-V-SCROLL-BAR
	 (:constant T :value :keyboard-selection-p)
	 (:left 168)(:top 45)(:height 200)
	 (:loaded T)
	 (:min-height 40)
	 (:parts
	  `(:border :bounding-area :indicator
	    (:up-arrow :modify (:interactors ((:trill :omit))))
	    (:down-arrow :modify (:interactors ((:trill :omit))))
	    :sel-box))
	 (:interactors
	  `((:slide :omit)
	    (:jump :omit)
	    (:key :omit)))
	 (:maker '((create-instance NIL gg:MOTIF-V-SCROLL-BAR
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		       (:do-not-dump-objects :me)
		       (:constant T)
		       (:min-height 40)
		       (:box '(170 45 NIL 200))
		       (:left (formula leftform))
		       (:top (formula topform))
		       (:height (formula heightform))
		       (:grow-p T)))))

       (create-instance NIL garnet-gadgets:motif-trill-device
	 (:constant T :value :keyboard-selection-p)
	 (:left 251)(:top 74)(:width 80)(:height 25)
	 (:loaded T)
	 (:min-height 20)
	 (:min-width 60)
	 (:parts
	  `((:h-bar :modify (:parts :border :BOUNDING-AREA :INDICATOR
			     (:left-arrow :modify
					  (:interactors ((:trill :omit))))
			     (:right-arrow :modify
					  (:interactors ((:trill :omit))))
			     :sel-box)
	     (:interactors ((:slide :omit)(:jump :omit)
			    (:key :omit))))
	    :feedback-text))
	 (:interactors
	  `((:feedback-inter :omit)))
	 (:maker '((create-instance NIL garnet-gadgets:motif-trill-device
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		       (:do-not-dump-objects :me)
		       (:constant T)
		       (:box '(251 74 80 25 ))
		       (:left (formula leftform))
		       (:top (formula topform))
		       (:width (formula widthform))
		       (:height (formula heightform))
		       (:grow-p T)))))
       
       (create-instance NIL garnet-gadgets:MOTIF-H-SCROLL-BAR	
	 (:constant T :value :keyboard-selection-p)
	 (:left 248)(:top 45)(:width 200)
	 (:loaded T)
	 (:min-width 40)
	 (:parts
	  `(:border :bounding-area :indicator
	    (:left-arrow :modify (:interactors ((:trill :omit))))
	    (:right-arrow :modify (:interactors ((:trill :omit))))
	    :sel-box))
	 (:interactors
	  `((:slide :omit)
	    (:jump :omit)
	    (:key :omit)))
	 (:maker '((create-instance NIL gg:MOTIF-H-SCROLL-BAR
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		     (:do-not-dump-objects :me)
		     (:constant T)
		     (:min-width 40)
		     (:box '(248 45 200 NIL))
		     (:left (formula leftform))
		     (:top (formula topform))
		     (:width (formula widthform))
		     (:grow-p T)))))

       (create-instance NIL garnet-gadgets:MOTIF-SLIDER
	    (:constant T :value :keyboard-selection-p)
	    (:left 193)(:top 45)(:height 200)
	    (:loaded T)
	    (:min-height 40)
	    (:parts
	     `(:border :bounding-area :indicator
	       :indicator-highlight-bar :indicator-shadow-bar
	       :text
	       (:up-arrow :modify (:interactors ((:trill :omit))))
	       (:down-arrow :modify (:interactors ((:trill :omit))))
	       :sel-box))
	    (:interactors
	     `((:slide :omit)
	       (:jump :omit)
	       (:key :omit)))
	    (:maker '((create-instance NIL gg:MOTIF-SLIDER
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
		       (:do-not-dump-objects :me)
		       (:constant T)
		       (:box '(191 45 NIL 200))
		       (:min-height 40)
		       (:left (formula leftform))
		       (:top (formula topform))
		       (:height (formula heightform))
		       (:grow-p T)))))

       (create-instance NIL opal:pixmap
	    (:constant T)
	    (:left 346)(:top 74)
	    (:image (o-formula (Get-Gilt-Pixmap "motif-gauge.xpm")))
	    (:loaded :motif-gauge)
	    (:load-file "motif-gauge-loader")
	    (:min-width 102)
	    (:maker '((create-instance NIL gg::MOTIF-GAUGE
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
;		       (:do-not-dump-objects :me)
		       (:constant T)
		       (:box '(376 96 102 NIL))
		       (:left (formula leftform))
		       (:top (formula topform))
		       (:width (formula widthform))
		       (:grow-p T) (:val-1 10)(:val-2 0)
		       (:num-marks 6) (:title "Title")
		       (:value-feedback-p NIL)
		       (:int-feedback-p NIL)))))
	   
       (create-instance NIL opal:text
	    (:constant T)
	    (:left 316) (:top 306)
	    (:string "Text")
	    (:loaded T)
	    (:maker '((create-instance NIL opal:text
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(316 287 NIL NIL)) 
			(:left (formula leftform))(:top (formula topform))
			;; :point-to-leaf needed for text-interactor
			(:point-to-leaf 'Fake-Point-to-Leaf)
			(:string "Text")))))

       (create-instance NIL opal:Multifont-Text
	    (:constant :left :top :fast-redraw-p :draw-function :line-style
		       :fill-background-p :visible)
	    (:left 366) (:top 295)
	    (:loaded T)
	    (:initial-Text `((("Multi" . ,(opal:get-standard-font
					      NIL NIL NIL))
				 ("Font," . ,(opal:get-standard-font
					      NIL NIL :large)))
				 (("multi-" . ,(opal:get-standard-font
						:serif NIL NIL))
				  ("line " . ,(opal:get-standard-font
					       :serif :italic NIL))
				  ("text" . ,(opal:get-standard-font
					      NIL :bold NIL)))))
	    (:maker `((create-instance NIL opal:Multifont-Text
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(366 284 NIL NIL)) 
			(:left (formula leftform))(:top (formula topform))
			;; :point-to-leaf needed for text-interactor
			(:point-to-leaf 'Fake-Point-to-Leaf)
		        (:initial-Text "MultiFont-Text")))))
			

       (create-instance NIL garnet-gadgets:MOTIF-SCROLLING-LABELED-BOX
	    (:constant T :value :keyboard-selection-p)
	    (:left 252)(:top 168)(:width 200)
	    (:min-width 100)
	    (:label-string "Title:")
	    (:field-string "Scrolling Text Box")
	    (:loaded T)
	    (:parts
	     `(:label-text :frame
	       (:field-text :modify
		(:interactors
		 ((:text-edit :omit))))
	       :sel-box))
	    (:maker '((create-instance NIL gg:MOTIF-SCROLLING-LABELED-BOX
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
		       (:do-not-dump-objects :me)
		       (:constant T)
		       (:min-width 100)
		       (:box '(252 198 200 NIL))
		       (:left (formula leftform))
		       (:top (formula topform))
		       (:width (formula widthform))
		       (:grow-p T) (:min-width 100)
		       (:label-string "Title:")
		       (:field-string "Scrolling Text Box")))))

       (create-instance NIL opal:rectangle
	    (:constant T)
	    (:left 350) (:top 250) (:width 42) (:height 32)
	    (:loaded T)
	    (:maker '((create-instance NIL opal:rectangle
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(384 236 52 32))
			(:grow-p T)
			(:left (formula leftform))(:top (formula topform))
			(:width (formula widthform))
			(:height (formula heightform))))))

       
       (create-instance NIL gg:motif-rect
	    (:constant T)
	    (:left 405) (:top 250) (:width 42) (:height 32)
	    (:loaded T)
	    (:maker '((create-instance NIL gg:motif-rect
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(420 236 52 32))
			(:grow-p T)
			(:left (formula leftform))(:top (formula topform))
			(:width (formula widthform))
			(:height (formula heightform))))))

       (create-instance NIL opal:line
	    (:constant T)
	    (:x1 405) (:y1 203) (:x2 450) (:y2 234)
	    (:loaded T)
	    (:maker '((create-instance NIL opal:line
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:points '(327 238 357 264 ))
			(:line-p T) (:grow-p T)
			(:x1 (o-formula (first (gvl :points))))
			(:y1 (o-formula (second (gvl :points))))
			(:x2 (o-formula (third (gvl :points))))
			(:y2 (o-formula (fourth (gvl :points))))))))

       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:left 350) (:top 203)
	    (:image (o-formula (opal:read-image (gvl :image-name))))
	    (:loaded T)
	    ;; want this to be a string, not a pathname.  *** PROBABLY NEED
	    ;; something different for Apple.
	    (:image-name
	     (namestring (merge-pathnames
			  "giltbitmap.bitmap"
			  common-lisp-user::Garnet-Gilt-Bitmap-PathName)))
	    (:maker '((create-instance NIL opal:bitmap
			:declare ((:parameters T :known-as :image-name)
				  (:Type (known-as-type :known-as)
					 (filename-type :image-name)))
			(:box '(267 237 NIL NIL))
			(:constant T)
			(:left (formula leftform))(:top (formula topform))
			(:image (o-formula (opal:read-image
					    (gvl :image-name))))
			(:image-name
			 (namestring (merge-pathnames
				      "giltbitmap.bitmap"
				      common-lisp-user::Garnet-Gilt-Bitmap-PathName)))))))

       (create-instance NIL opal:pixmap
	    (:constant T)
	    (:left 255) (:top 203)
	    (:image (o-formula (opal:read-xpm-file (gvl :image-name))))
	    (:loaded T)
	    ;; want this to be a string, not a pathname.  *** PROBABLY NEED
	    ;; something different for Apple.
	    (:image-name
	     (namestring (merge-pathnames "garnetlogo.xpm"
					  common-lisp-user::Garnet-Pixmap-Pathname)))
	    (:maker '((create-instance NIL opal:pixmap
			:declare ((:parameters T :known-as :image-name)
				  (:Type (known-as-type :known-as)
					 (filename-type :image-name)))
			(:box '(267 237 NIL NIL))
			(:constant T)
			(:left (formula leftform))(:top (formula topform))
			(:image (o-formula (opal:read-xpm-file
					    (gvl :image-name))))
			(:image-name
			 (namestring (merge-pathnames
				      "garnetlogo.xpm"
				      common-lisp-user::Garnet-Pixmap-Pathname)))))))

       (create-instance NIL opal:text
	  (:constant T)
	  (:left 340) (:top 341)
	  (:string "Motif-Background")
	  (:font (opal:get-standard-font NIL :bold NIL))
	  (:loaded T)
	  (:maker '((create-instance NIL garnet-gadgets:MOTIF-BACKGROUND
		      :declare ((:parameters T :known-as)
				(:Type (known-as-type :known-as)))
		      (:box '(0 0 NIL NIL))
		      (:constant T)
		      (:left 0) (:top 0)
		      (:foreground-color opal:MOTIF-GRAY)
		      (:hit-threshold 3)
		      (:select-outline-only T)))))

       )
    ))
