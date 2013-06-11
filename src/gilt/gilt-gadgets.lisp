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
               for garnetlogo pixmap.
     7/01/93 Andrew Mickish - Changed values for :loaded slots to keywords;
               Called load-extra-garnet-gadgets for Lucid
     5/28/93 Andrew Mickish - Added :value to constant lists
     4/22/93 Andrew Mickish - Made HourGlass cursor standard
     3/18/93 Brad Myers - known-as-type.  Moved popup funcs to gadget files.
      3/8/93 Brad Myers - radically revised to work with a single gilt.
                        - added new popup gadgets, like menubar
    12/01/92 Andrew Mickish - Added :constant declaration to all maker
               functions (and removed it from Create-New-Gadget).
    11/20/92 Andrew Mickish - Omitted all interactors in IB-WIN gadgets.
    09/07/92 Andrew Mickish - Switched order of parameters to merge-pathname
               for giltbitmap bitmap.
    08/19/92 Andrew Mickish - Bitmap pathnames now use namestring function
      4/4/92 Brad Myers - new specialrun and build forms
    03/25/92 Andrew Mickish - Get-Values ---> G-Value
    03/25/92 Andrew Mickish - Added Invalid-Pathname-p filter to
               :properties-slots list of TYPE-BITMAP.
    02/18/92 Brad Myers - new constant definitions, and more type checking
                        - moved common to gilt-gadget-utils
    01/06/92 Andrew Mickish - Instead of adding type names to
               opal:*standard-names* with setf in this file, the names now
               appear in the defparameter in save-agg.lisp.
    04/11/91 Brad Myers - fixed bug in function-for-ok-name for save
    03/27/91 Andrew Mickish - Removed :fixed-width-size and :fixed-height-size
               from property lists
    03/13/91 Osamu Hashimoto - Moved Show-Save-Dialog & Show-Read-Dialog here
                               from gilt.lisp
    03/07/91 Osamu Hashimoto - Moved *prop-sheet* here from gilt.lisp
    03/04/91 Osamu Hashimoto - Moved Make-Main-Menu here from gilt.lisp
    03/04/91 Andrew Mickish - Added :min-width and :min-height to gadgets
               that grow
    02/27/91 Andrew Mickish - Moved *load-file* here from gilt.lisp;
               Moved IB-WINDOW here from gilt.lisp
    02/21/91 Andrew Mickish - Moved IB-OBJS here from gilt.lisp
    02/06/90 Andrew Mickish - Changed gauge slots so that :width can be set
             directly but :radius cannot (due to change in gauge gadget).
    11/13/90 Brad Myers - Split from gilt.lisp
============================================================
|#


(in-package "GILT")

(declaim (special *work-win*))

;; load the gadgets needed for the palette that are not needed for
;; Gilt itself.
(defun load-extra-garnet-gadgets ()
  ;; the commented out ones are used as pixmaps, so not needed unless used
  (dolist (pair '((:text-buttons "text-buttons-loader")
		  (:x-buttons "x-buttons-loader")
		  (:radio-buttons "radio-buttons-loader")
		  (:labeled-box "labeled-box-loader")
		  (:scrolling-labeled-box "scrolling-labeled-box-loader")
		  #+lucid (:scrolling-menu "scrolling-menu-loader")
		  #+lucid (:menu "menu-loader")
		  #+lucid (:h-scroll-bar "h-scroll-loader")
		  #+lucid (:h-slider "h-slider-loader")
		  #+lucid (:v-scroll-bar "v-scroll-loader")
		  #+lucid (:v-slider "v-slider-loader")
		  #+lucid (:gauge "gauge-loader")
		  #+lucid (:trill-device "trill-device-loader")
		  #+lucid (:menubar "menubar-loader")
		  ))
    (unless (get :garnet-modules (car pair))
      (common-lisp-user::garnet-load (concatenate 'string "gadgets:" (cadr pair)))))
  )

;; The Lucid compiler performs some kind of optimization that replaces
;; the quoted gadget references with the constant values, and it will
;; complain if the references are read below without the gadgets being
;; defined.  You could take this out if you replaced the affected
;; gadgets with bitmaps.
#+lucid
(load-extra-garnet-gadgets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Make-Garnet-Palette-Window ()
  (if (and *garnet-ib-win*
	   (schema-p *garnet-ib-win*))
      (progn
	(s-value *garnet-ib-win* :visible T)
	(setq *ib-win* *garnet-ib-win*)
	(s-value *work-win* :ib-win *ib-win*)
	*garnet-ib-win*)

      ;; else have to create it
      (let ((win (create-instance NIL inter:interactor-window
		   (:left 550)(:top 300)(:width 472)(:height 471)
		   (:title "Gilt Gadgets"))))

	(setq *ib-win* win)
	(opal:update *ib-win*)
	(when *work-win*
	  (s-value *work-win* :ib-win *ib-win*)
	  (opal:update *work-win*))
	(opal:With-HourGlass-Cursor
	  (s-value win :aggregate (create-instance NIL ib-objs
				    (:widget-set :garnet)
				    (:string "Garnet Gadgets")))
	  (setq *garnet-ib-win* win)
	  (load-extra-garnet-gadgets)
	  (add-garnet-gadgets win))
	win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now define the gadgets;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-garnet-gadgets (ib-win)
  (let ((agg (g-value ib-win :aggregate :selectable-objs))
	scroll-box)

    (opal:add-components agg

       (create-instance NIL opal:bitmap
	 (:image (Get-Gilt-Bitmap "menubar.bitmap"))
	 (:loaded :menubar)
	 (:load-file "menubar-loader")
	 (:left 14)(:top 29)
	 (:constant T)
	 (:maker '((create-instance NIL garnet-gadgets::menubar
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

       (create-instance NIL garnet-gadgets:text-button-panel
	    (:constant T :value)
	    (:left 10)(:top 57)
	    (:loaded T)
	    (:items '("Label1" "Label2" "Label3"))
	    (:interactors
	     `((:text-button-press :omit)))
	    (:maker '((create-instance NIL gg:text-button-panel
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
;			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(10 57 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:items '("Label1" "Label2" "Label3"))))))

       (create-instance NIL garnet-gadgets:x-button-panel
	    (:constant T :value)
	    (:loaded T)
	    (:left 104)(:top 56)
	    (:items '("Label1" "Label2" "Label3"))
	    (:interactors
	     `((:x-button-press :omit)))
	    (:maker '((create-instance NIL gg:x-button-panel
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
;			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(100 30 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:items '("Label1" "Label2" "Label3"))))))
       (create-instance NIL garnet-gadgets:radio-button-panel
	    (:constant T :value)
	    (:loaded T)
	    (:left 104)(:top 153)
	    (:items '("Label1" "Label2" "Label3"))
	    (:interactors
	     `((:radio-button-press :omit)))
	    (:maker '((create-instance NIL gg:radio-button-panel
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
;			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(100 135 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:items '("Label1" "Label2" "Label3"))))))
       (create-instance NIL opal:bitmap
	    (:image (Get-Gilt-Bitmap "scrolling-menu.bitmap"))
	    (:loaded :scrolling-menu)
	    (:load-file "scrolling-menu-loader")
	    (:constant T)
	    (:left 10)(:top 214)
	    (:maker '((create-instance NIL garnet-gadgets::scrolling-menu
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(10 200 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:items '("Label1" "Label2" "Label3" "Label4"
				  "Label5" "Label6" "Label7" "Label8"))
			(:title "Title")))))
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "okcancel.bitmap"))
	    (:left 10)(:top 430)
	    (:loaded T)
	    (:maker '((create-instance NIL GARNET-GADGETS:TEXT-BUTTON-PANEL
			:declare ((:parameters T :known-as :except :items)
				  (:Type (known-as-type :known-as)))
;	                (:do-not-dump-objects :me)
			(:Ok-Cancel-p T)
			(:constant T)
			(:box '(11 350 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:DIRECTION :HORIZONTAL)
			(:SHADOW-OFFSET 5)
			(:TEXT-OFFSET 2)
			(:final-feedback-p NIL)
			(:GRAY-WIDTH 3)
			(:ITEMS '("OK" "Cancel"))
			(:SELECT-FUNCTION 'OKCancel-Function)))))
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "okapplycancel.bitmap"))
	    (:left 170)(:top 430)
	    (:loaded T)
	    (:maker '((create-instance NIL GARNET-GADGETS:TEXT-BUTTON-PANEL
			:declare ((:parameters T :known-as :except :items)
				  (:Type (known-as-type :known-as)))
;	                (:do-not-dump-objects :me)
			(:Ok-Cancel-p T)
			(:constant T)
			(:box '(11 385 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:DIRECTION :HORIZONTAL)
			(:SHADOW-OFFSET 5)
			(:TEXT-OFFSET 2)
			(:final-feedback-p NIL)
			(:GRAY-WIDTH 3)
			(:ITEMS '("OK" "Apply" "Cancel"))
			(:SELECT-FUNCTION 'OKCancel-Function)))))
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "menu.bitmap"))
	    (:load-file "menu-loader")
	    (:loaded :menu)
	    (:left 106)(:top 252)
	    (:maker '((create-instance NIL garnet-gadgets::menu
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(110 240 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:items '("Label1" "Label2" "Label3"))
			(:title "Title")))))

       (create-instance NIL opal:bitmap
	 (:image (Get-Gilt-Bitmap "option-button.bitmap"))
	 (:loaded :option-button)
	 (:load-file "option-button-loader")
	 (:constant T :value)
	 (:left 21)(:top 360)
	 (:maker '((create-instance NIL GARNET-GADGETS::OPTION-BUTTON
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		     (:constant T)
		     (:box '(117 340 NIL NIL))
		     (:left (formula leftform))
		     (:top (formula topform))))))

       (create-instance NIL opal:bitmap
	 (:image (Get-Gilt-Bitmap "popup-button.bitmap"))
	 (:loaded :popup-menu-button)
	 (:load-file "popup-menu-button-loader")
	 (:constant T)
	 (:left 23)(:top 400)
	 (:maker '((create-instance NIL garnet-gadgets::popup-menu-button
		     :declare ((:parameters T :known-as :select-function)
			       (:Type (known-as-type :known-as)))
		     (:constant T)
		     (:box '(117 340 NIL NIL))
		     (:left (formula leftform))
		     (:top (formula topform))))))

       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "h-scroll-bar.bitmap"))
	    (:load-file "h-scroll-loader")
	    (:loaded :h-scroll-bar)
	    (:left 200)(:top 31)
	    (:min-width 120)
	    (:maker '((create-instance NIL garnet-gadgets::h-scroll-bar
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(185 30 200 NIL))
			(:left (formula leftform))(:top (formula topform))
			(:grow-p T)
			(:width (formula widthform))) 25)))
       (create-instance NIL opal:bitmap
	    (:image (Get-Gilt-Bitmap "h-slider.bitmap"))
	    (:constant T)
	    (:load-file "h-slider-loader")
	    (:loaded :h-slider)
	    (:left 200)(:top 64)
	    (:min-width 130)
	    (:maker '((create-instance NIL garnet-gadgets::h-slider
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(185 65 200 NIL))
			(:left (formula leftform))(:top (formula topform))
			(:grow-p T)
			(:width (formula widthform))
			(:val-2 10)(:num-marks 6)) 3)))
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "v-scroll-bar.bitmap"))
	    (:loaded :v-scroll-bar)
	    (:load-file "v-scroll-loader")
	    (:left 196)(:top 110)
	    (:min-height 120)
	    (:maker '((create-instance NIL garnet-gadgets::v-scroll-bar
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(185 110 NIL 250))
			(:left (formula leftform))(:top (formula topform))
			(:grow-p T)
			(:height (formula heightform)))
		      25))) ;; initial value
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "v-slider.bitmap"))
	    (:loaded :v-slider)
	    (:load-file "v-slider-loader")
	    (:left 233)(:top 110)
	    (:min-height 120)
	    (:maker '((create-instance NIL garnet-gadgets::v-slider
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(225 110 NIL 250))
			(:left (formula leftform))(:top (formula topform))
			(:grow-p T)
			(:height (formula heightform))
			(:val-2 10)(:num-marks 6)) 3)))
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "gauge.bitmap"))
	    (:loaded :gauge)
	    (:load-file "gauge-loader")
	    (:left 295)(:top 110)
	    (:min-width 100)
	    (:maker '((create-instance NIL garnet-gadgets::gauge
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
;	                (:do-not-dump-objects :me)
			(:constant T)
			(:box '(280 110 100 NIL))
			(:left (formula leftform))(:top (formula topform))
			(:width (formula widthform))
			(:int-feedback-p NIL)
			(:val-1 10)(:val-2 0)
			(:num-marks 6)(:title "Title")(:value-feedback-p NIL))
		      3)))
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:image (Get-Gilt-Bitmap "trill-device.bitmap"))
	    (:load-file "trill-device-loader")
	    (:loaded :trill-device)
	    (:left 296)(:top 214)
	    (:min-width 100)
	    (:maker '((create-instance NIL garnet-gadgets::trill-device
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(290 210 NIL NIL))
			(:left (formula leftform))(:top (formula topform)))
		      25)))
       (create-instance NIL garnet-gadgets:labeled-box
	    (:constant T :value)
	    (:left 295)(:top 250)
	    (:loaded :labeled-box)
	    (:min-width 120)
	    (:label-string "Title:")
	    (:value "String")
	    (:interactors
	     `((:text-inter :omit)))
	    (:maker '((create-instance NIL gg:labeled-box
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(280 245 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			(:min-width 120)
			(:label-string "Title:")))))

       (setq scroll-box (create-instance NIL gg:scrolling-labeled-box
	    (:left 296)(:top 280)(:width 120)
	    (:constant T)
	    (:loaded :scrolling-labeled-box)
	    (:min-width 120) (:grow-p T)
	    (:value "Very long string")
	    (:width (formula widthform))
	    (:label-string "Title:")
	    (:parts
	     `(:label-text :frame
	       (:field-text :modify
		(:interactors
		 ((:text-edit :omit))))))
	    (:maker '((create-instance NIL gg:scrolling-labeled-box
			:declare ((:parameters T :known-as :select-function)
				  (:Type (known-as-type :known-as)))
			(:do-not-dump-objects :me)
			(:constant T)
			(:box '(280 275 130 NIL))
			(:left (formula leftform))(:top (formula topform))
			(:width (formula widthform))
			(:label-string "Title:")
			(:min-width 120) (:grow-p T))))))
       (create-instance NIL opal:rectangle
	    (:constant T)
	    (:left 286) (:top 368) (:width 56) (:height 44)
	    (:grow-p T)
	    (:loaded T)
	    (:maker '((create-instance NIL opal:rectangle
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(280 318 50 40))
			(:left (formula leftform))(:top (formula topform))
			(:width (formula widthform))
			(:height (formula heightform))
			(:grow-p T)))))
       (create-instance NIL opal:line
	    (:constant T)
	    (:x1 350) (:y1 411) (:x2 378) (:y2 369)
	    (:loaded T)
	    (:maker '((create-instance NIL opal:line
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:points '(340 318 365 358))
			(:grow-p T)
			(:x1 (o-formula (first (gvl :points))))
			(:y1 (o-formula (second (gvl :points))))
			(:x2 (o-formula (third (gvl :points))))
			(:y2 (o-formula (fourth (gvl :points))))))))
       (create-instance NIL opal:text
	    (:constant T)
	    (:left 295) (:top 317)
	    (:string "Label")
	    (:loaded T)
	    (:maker '((create-instance NIL opal:text
			:declare ((:parameters T :known-as)
				  (:Type (known-as-type :known-as)))
			(:constant T)
			(:box '(375 305 NIL NIL))
			(:left (formula leftform))(:top (formula topform))
			;; :point-to-leaf needed for text-interactor
			(:point-to-leaf 'Fake-Point-to-Leaf)
			(:string "Label")))))
       (create-instance NIL opal:bitmap
	    (:constant T)
	    (:left 414) (:top 355)
	    (:loaded T)
	    (:image (o-formula (opal:read-image (gvl :image-name))))
	    (:image-name
	     (namestring (merge-pathnames "giltbitmap.bitmap"
					  common-lisp-user::Garnet-Gilt-Bitmap-PathName)))
	    (:maker '((create-instance NIL opal:bitmap
			:declare ((:parameters T :known-as :image-name)
				  (:Type (known-as-type :known-as)
					 (filename-type :image-name)))
			(:box '(375 330 NIL NIL))
			(:constant T)
			(:left (formula leftform))(:top (formula topform))
			(:image (o-formula (opal:read-image
					    (gvl :image-name))))
			;; want this to be a string, not a pathname.
			(:image-name
			 (namestring (merge-pathnames "giltbitmap.bitmap"
						      common-lisp-user::Garnet-Gilt-Bitmap-PathName))
			 )))))
       (create-instance NIL opal:Multifont-Text
	    (:constant T)
	    (:left 350) (:top 309)
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

       (create-instance NIL opal:pixmap
	    (:constant T)
	    (:left 391) (:top 396)
	    (:image (o-formula (opal:read-xpm-file (gvl :image-name))))
	    (:loaded T)
	    ;; want this to be a string, not a pathname.
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
       )
    (s-value (g-value scroll-box :field-text :string) :first-vis-char 4)
    ))
