;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;


;;; C32 is a spreadsheet interface for Garnet constraints
;;
;;  Designed and implemented by Brad Myers


;;; * BUGS
;;   - arrows don't move when panel scrolls (so then point to wrong slot)
;;   - arrows should be under mainsel and other feedback objects
;;   - too slow
;;   - when copy a formula, and then generalize, should replace
;;      original with new formula also.
;;   - If a slot or object used a parameter to a function, should
;;      notice it for copy and generalize
;;   - Point-to-mouse object should take into account
;;      *current-formula-win* so the guessed slot changes if change
;;      windows.
;;   - *current-formula-win* should be a queue, so if window deleted when
;;      another is around, second is used



(in-package "C32")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Do-Go Do-Stop)))

;;;*******************************************************************
(defparameter *Current-Selection-Feedback* NIL)	    ; holds the feeback
					            ; obj for the cur selection
(defparameter *Current-Sec-Selection-Feedback* NIL) ; holds the feeback
					            ; obj for the sec (middle button) selection
(defparameter *All-Windows* NIL)		    ; list of all windows, for Quit
(defparameter *Current-Panel-Set* NIL)		    ; the current main panel-set

;;********************************************************************

(defparameter *C32-package* (find-package "COMMON-LISP-USER")
  "This package is used for all READ operations.  It allows the user to
  type values and object names without a package prefix.")

;; List of slots that require confirmation for dangerous operations (such
;; as Delete Slot).
;;
(defparameter *dangerous-slots*
  '(:is-a :is-a-inv :components :parent))

(when gem::*x11-server-available*

  ;; loading gadgets is in c32loader
  (create-instance 'ital-font opal:font (:face :italic))
  (create-instance 'bold-font opal:font (:face :bold))
  (create-instance 'reg-font opal:font)

  (create-instance 'form-icon opal:bitmap
    (:image (opal:read-image (merge-pathnames
			      "formula-icon.bm"
			      common-lisp-user::Garnet-C32-Bitmap-PathName))))

  (create-instance 'inherited-icon opal:bitmap
    (:image (opal:read-image (merge-pathnames
			      "inherited-icon.bm"
			      common-lisp-user::Garnet-C32-Bitmap-Pathname))))

  (defparameter Font-Height (max (opal:string-height ital-font "/")
				 (opal:string-height reg-font "/")))
  (defparameter Font-Char-Width (max (opal:string-width ital-font "/")  ; fixed width
				     (opal:string-width reg-font "/"))) ; fonts

  (defparameter form-icon-width (g-value form-icon :width))
  (defparameter inherited-icon-width (g-value inherited-icon :width))

  (defparameter Label-Width 100)
  (defparameter Max-Value-Width 120)

  (defparameter Label-Num-Chars (floor Label-Width Font-Char-Width))
  (defparameter Label-Side-Width (+ Label-Width form-icon-width inherited-icon-width
				    -4)) ; fudge factor
  (defparameter Icon-At-Right-Offset (+ Label-Side-Width 2 Max-Value-Width))
  (defparameter Full-Item-Width (+ Icon-At-Right-Offset inherited-icon-width 2))

  (defparameter Scroll-Panel-Width (+ 20 Full-Item-Width))
  (defparameter Scroll-Panel-Num-Items 12)
  (defparameter Scroll-Panel-Height (* Scroll-Panel-Num-Items (+ 1 Font-Height))))


(defun Mk-String (val)
  (let ((kr::*print-as-structure* NIL)	;; turn off #k<...> notation
	#-(and)
	(*package* *c32-package*)  ; enable this to suppress package names
	)
    (format NIL "~s" val)))


;; if val is in the keyword package, then put a colon in front.  Clip the
;; string to len characters
(defun Mk-Colon-Str-And-Clip (val len)
  (if (null val)
    ""					; empty
    ;; else generate a string
    (let ((str (String-Capitalize val)))
      (if (keywordp val)
	(setq str (concatenate 'simple-string ":" str)))
      (if (> (length str) len)
	(subseq str 0 len)
	str))))


(defun turn-selections-off ()
  (s-value *Current-Selection-Feedback* :obj-over nil)
  (s-value *Current-Sec-Selection-Feedback* :obj-over nil))



(when gem::*x11-server-available*

  ;; Have an extra type so can look for it from the interactor
  (create-instance 'Value-Scrolling-String garnet-gadgets:scrolling-input-string)

  (create-instance 'c32-label opal:text
    (:string (o-formula
	      (mk-colon-str-and-clip
	       (gvl :parent :slot) label-num-chars)))
    (:left  (o-formula (+ 2 (gvl :parent :left))))
    (:top  (o-formula (gvl :parent :top)))
    (:font (o-formula (if (gvl :parent :inherited-p)
			  ital-font
			  reg-font))))


  (create-instance 'c32-item opal:aggregadget
    (:obj NIL)
    (:slot NIL)
    (:left 0)
    (:top 0)
    (:width Full-Item-Width)
    (:height Font-Height)
    (:visible
     (o-formula (let* ((min-val (gvl :parent :parent :scroll-bar :value))
		       (max-val (+ -1 min-val Scroll-Panel-Num-Items))
		       (index (position (gv :self) (gvl :parent :components))))
		  (and (>= index min-val)
		       (<= index max-val))))
     #+DZG (o-formula (let* ((min-val (gvl :parent :parent
					   :scroll-bar :value))
			     (max-val (+ -1 min-val Scroll-Panel-Num-Items))
			     (index (gvl :rank)))
			(and (>= index min-val)
			     (<= index max-val)))))
    (:value (o-formula (let ((obj (gvl :obj))
			     (slot (gvl :slot)))
			 (when slot (gv obj slot)))))
    (:formula-p (o-formula (let ((obj (gvl :obj))
				 (slot (gvl :slot)))
			     (when slot
			       ;; just to make a dependency
			       (gv obj slot)
			       (formula-p (get-value obj slot))))))
    (:inherited-p (o-formula (let ((obj (gvl :obj))
				   (slot (gvl :slot)))
			       (when slot
				 (gv obj slot) ; just to make a dependency
				 (kr::inherited-p obj slot)))))
    (:parts
     `((:label ,c32-label)
       (:form-icon ,form-icon
					; (:constant T)
		   (:left ,(o-formula (+ (gvl :parent :left)
					 Label-Width)))
		   (:top ,(o-formula (gvl :parent :top)))
		   ;; draw it invisible if not a formula, but still have it there
		   ;; so that it can be selected to create a formula.
		   (:draw-function ,(o-formula (if (gvl :parent :formula-p)
						   :copy
						   :no-op))))
       (:inherited-icon ,inherited-icon
					; (:constant T)
			(:left ,(o-formula
				 (if (gvl :parent :formula-p)
				     ;; then value is inherited so put next to form
				     (+ (gvl :parent :left) Label-Width form-icon-width 1)
				     ;; else no formula, put icon at right
				     (+ (gvl :parent :left) icon-at-right-offset))))
			(:top ,(o-formula (gvl :parent :top)))
			(:visible ,(o-formula (gvl :parent :inherited-p))))
       (:value-str ,Value-Scrolling-String
					; (:constant T)
		   (:pretend-to-be-leaf T)
		   (:value ,(o-formula (if (gvl :parent :slot)
					   (Mk-String (gvl :parent :value))
					   "")))
		   ;; italic if value itself is inherited, so if no formula
		   (:font ,(o-formula (if (and (gvl :parent :inherited-p)
					       (not (gvl :parent :formula-p)))
					  ital-font
					  reg-font)))
		   (:width ,Max-Value-Width)
		   (:selection-function Value-Edited-Func)
		   (:interactors ((:text-edit :omit)))
		   (:left ,(o-formula (if (gvl :parent :slot)
					  ;; Normal slot; value is on the right.
					  (+ (gvl :parent :left) Label-Side-Width)
					  ;; Last (empty) slot: string on the left, for Add
					  ;; Slot command
					  (gvl :parent :left))))
		   (:top ,(o-formula (gvl :parent :top))))
       (:underline ,opal:line
					; (:constant t)
		   (:x1 ,(o-formula (gvl :parent :left)))
		   (:y1 ,(o-formula (1- (opal:gv-bottom (gvl :parent :value-str)))))
		   (:x2 ,(o-formula (opal:gv-right (gvl :parent))))
		   (:y2 ,(o-formula (gvl :y1)))))))
  (defparameter Header-Left-Offset 20)
  (defparameter Header-Height (+ 2 font-height))
  (defparameter Header-Width (+ Full-Item-Width 3))

  (Create-instance 'c32-panel opal:aggregadget
    (:left 0)
    (:top 0)
    (:c32-items NIL)
    (:obj NIL)
    (:max-index (o-formula (- (length (gvl :c32-items)) 1)
			   4))		; initial value
    (:width (+ Header-Width 20))		; header + scrollbar(=20)
    (:height (+ Scroll-Panel-Height Header-height))
    (:parts
     `((:header ,garnet-gadgets:scrolling-input-string
					; (:constant T)
		(:value ,(o-formula (if (gvl :parent :obj)
					(Mk-String (gvl :parent :obj))
					"")))
		(:font ,(o-formula (if (gvl :parent :obj)
				       bold-font
				       reg-font)))
		(:width ,(- Header-Width 2))
		(:selection-function Header-Edited-Func)
		(:left ,(o-formula (+ (gvl :parent :left) Header-Left-Offset
				      (if (gvl :parent :obj) 0 60))))
		(:top ,(o-formula (+ 1 (gvl :parent :top)))))
       (:header-rect ,opal:rectangle
					; (:constant T)
		     (:left ,(o-formula (+ Header-Left-Offset (gvl :parent :left))))
		     (:top ,(o-formula (gvl :parent :top)))
		     (:width ,Header-Width)
		     (:height ,Header-Height))
       (:border ,opal:rectangle
					; (:constant T)
		(:left ,(o-formula (+ Header-Left-Offset (gvl :parent :left))))
		(:top ,(o-formula (+ header-height (gvl :parent :top))))
		(:width ,Header-width)
		(:height ,Scroll-Panel-Height))
       (:scroll-bar ,garnet-gadgets:v-scroll-bar
					; (:constant T)
		    (:left ,(o-formula (gvl :parent :left)))
		    (:top ,(o-formula (gvl :parent :top)))
		    (:height ,(o-formula (+ Scroll-Panel-Height Header-height)))
		    (:val-1 0)
		    (:val-2 ,(o-formula (gvl :parent :max-index)))
		    (:scroll-p ,(o-formula (>= (gvl :val-2) Scroll-Panel-Num-Items)))
		    (:page-incr ,(1- Scroll-Panel-Num-Items))
		    (:indicator-text-p NIL))
       (:aggrel ,opal:aggrelist
		;; items are added explicitly
					; (:constant T)
		(:left ,(o-formula (+ 2 Header-Left-Offset (gvl :parent :left))))
		(:top ,(o-formula (+ 2 header-height (gvl :parent :top))))
		(:v-spacing 1))
       (:vert-line ,opal:line
					; (:constant T)
		   (:x1 ,(o-formula (+ (gvl :parent :left)
				       Header-Left-Offset 8 Label-Side-Width)))
		   (:x2 ,(o-formula (gvl :x1)))
		   (:y1 ,(o-formula (+ header-height
				       (gvl :parent :top))))
		   (:y2 ,(o-formula (opal:gv-bottom (gvl :parent :border)))))
       ;; This is only used by the empty panel
       (:empty-title ,opal:text
					; (:constant T)
		     (:left ,(o-formula (+ (gvl :parent :left) 25)))
		     (:top ,(o-formula (+ (gvl :parent :top) 1)))
		     (:string "Object:")
		     (:font ,bold-font)
		     (:visible ,(o-formula (null (gvl :parent :obj))))))))

  )



(defparameter Extra-C32-Panels NIL)

;; creates a scrolling window panel for the object
;;
(defun Create-Panel-For-Obj (obj left top agg)
  (let ((panel (Pop Extra-C32-Panels)))
    (if (schema-p panel)
	(progn
	  (s-value panel :left left)
	  (s-value panel :top top))
	;; else create a new one
	(setf panel (create-instance NIL c32-panel
		      ;; (:constant T)
		      (:left left) (:top top))))
    (unless obj
      (s-value (g-value panel :header) :value "name"))
    (opal:add-component agg panel)
    (New-Obj-For-Panel obj panel)	; sets all the items
    panel))


;; If an object is shown in only one panel, and the panel is removed, we
;; need to restore the object's original :destroy method, which was modified
;; when we displayed the object in the panel.
;;
(defun restore-destroy-method (panel)
  (let* ((object (g-value panel :obj))
	 (count-and-method (g-local-value object :c32-count))
	 (count (car count-and-method)))
    (when (plusp count)
      (setf (car count-and-method) (decf count))
      (when (zerop count)
	;; Count is down to 0, so last panel for this objects was removed.
	;; Restore the original method.
	(s-value object :destroy-me (cdr count-and-method))))))



(defun Remove-Panel (panel)
  ;; Eliminate the panel.
  (let ((parent (g-value panel :parent)))
    (opal:remove-component parent panel)
    (opal:notice-items-changed parent))  ; dzg
  ;; Make it reusable.
  (push panel Extra-C32-Panels)
  (restore-destroy-method panel))


(defparameter Extra-C32-Items NIL)

(defun Get-C32-item (obj slot)
  (let ((item (pop Extra-C32-Items)))
    (if item
	(progn
	  (s-value item :obj obj)
	  (s-value item :slot slot)
	  item)
	;; else, create a new one
	(create-instance NIL c32-item
	  (:obj obj)
	  (:slot slot)))))


;; This method encapsulates objects' original :destroy-me method.  First, it
;; eliminates the panel(s) that display an object being destroyed.  Then, it
;; calls the original :destroy-me method.
;;
(defun c32-destroy-method (object &optional other)
  (when (schema-p *current-panel-set*)
    (dolist (panel (g-value *current-panel-set* :c32-panels))
      (when (and (schema-p panel)
		 (eq (g-value panel :obj) object))
	;; This is the panel that is displaying the <object>.
	(Remove-Panel panel)
	(return))))
  ;; Now invoke the original destroy-me method
  (let ((method (cdr (g-local-value object :c32-count))))
    (when method
      (funcall method object other))))


;; Assigns a new object to the panel.
;;
(defun New-Obj-For-Panel (obj panel)
  (let ((slots (if obj (append (g-value obj :slots-to-show) (list nil))))
	;; add a special NIL slot at the end of each one as a place-holder.
	(aggrel (g-value panel :aggrel))
	(old-items (g-value panel :c32-items))
	item items)
    (s-value panel :obj obj)
    (dolist (slot slots)
      ;; use up the old items, then use any extra items, then create a new one.
      (setq item (pop old-items))
      (if item
	  (progn
	    (s-value item :obj obj)
	    (s-value item :slot slot))
	  (progn
	    (setq item (Get-C32-item obj slot))
	    (opal:add-component aggrel item)))
      (push item items))
    (when old-items			; some left over
      (dolist (o old-items)
	(opal:remove-component aggrel o))
      (setq Extra-C32-Items (append Extra-C32-Items old-items)))
    (s-value panel :c32-items (reverse items))
    ;; Make sure that all items, even ones that had previously become
    ;; invisible, are displayed.
    (dolist (item items)
      (s-value item :visible T))
    ;; Now install a modified destroy method on the object.  This ensures
    ;; that external changes to the object (i.e., calling Destroy from the
    ;; Lisp listener) will be reflected in the panel.
    (when obj
      (let ((count-and-method (g-local-value obj :c32-count)))
	(unless count-and-method
	  ;; store counter and original destroy method
	  (s-value obj :c32-count (cons 1 (g-value obj :DESTROY-ME)))
	  (s-value obj :DESTROY-ME 'c32-destroy-method))))))



(Defun Add-New-Row (panel slot)
  (let* ((aggrel (g-value panel :aggrel))
	 (item (Get-C32-item (g-value panel :obj) NIL))
	 (items (g-value panel :c32-items))
	 (scroll-bar (g-value panel :scroll-bar))
	 (value (g-value scroll-bar :value))
	 (length (length (g-value aggrel :components))))
    (opal:add-component aggrel item)
    (s-value (car (last items)) :slot slot)
    (s-value panel :c32-items (append items (list item)))
    ;; Restore the scroll bar, to avoid unwanted jumps.  However, make sure
    ;; the empty slot at the bottom remains visible.
    (when (>= (- length value) 12)
      (setf value (- length 11)))
    (s-value scroll-bar :value value)))


;; checks to see if new-value is the string name of an object
;; ** Unwind-protect doesn't work, so don't bother
;;
(defun Careful-Get-Obj (new-value)
  (let* ((*package* *C32-package*)
	 (val (read-from-string (string-upcase new-value))))
    (when (symbolp val)
      (unless (boundp val)
	(if (c32-query (format nil "Object ~S does not exist - create?" val))
	    (create-schema val)
	    (return-from careful-get-obj NIL)))
      (setq val (eval val))
      (when (schema-p val)
	val))))


(defun Value-Edited-Func (gadget newvalue)
  (let* ((c32-item (g-value gadget :parent))
	 (slot (g-value c32-item :slot))
	 (obj (g-value c32-item :obj)))
    (if (null slot)
	(AddSlotAction gadget obj newvalue) ; add a slot
	(when obj			    ; edit a value
	  (if (kr::slot-constant-p obj slot)
	      ;; Constant.
	      (progn
		(recompute-formula (g-value gadget :string :parent) :value)
		(c32error "Slot is constant - not set."))
	      ;; OK.
	      (multiple-value-bind (value success)
		  (C32-Careful-String-Eval newvalue nil nil)
		(if success
		    (s-value obj slot value)
		    ;; Revert to the original string.
		    (recompute-formula gadget :value))))))))


;; The title of a panel was edited.  Switch to a new object, create a new
;; object, or eliminate the panel.
;;
(defun Header-Edited-Func (gadget newvalue)
  (let ((panel (g-value gadget :parent))
	new-obj)
    (if (string= newvalue "")
	(Remove-Panel panel)
	(progn
	  (setq New-Obj (Careful-Get-Obj newvalue))
	  (if New-Obj
	      ;; Valid object.
	      (progn
		(unless (g-value panel :obj)
		  ;; This is the empty panel on the right
		  (create-new-panel-object nil))
		;; Show a new object.
		(s-value panel :obj new-obj)
		(New-Obj-For-Panel new-obj panel)
		;; Turn off selections.
		(turn-selections-off))
	      ;; bad object
	      (let ((obj (g-value gadget :parent :obj)))
		(if obj
		    (s-value gadget :value (format nil "~S" obj)) ; show old value
		    (s-value gadget :value "name"))		  ; empty panel
		(inter:beep)))))))


(when gem::*x11-server-available*

(defparameter Panel-Set-Height (+ 2 Scroll-Panel-Height Header-height))
(defparameter Panel-Set-Total-Height (+ Panel-Set-Height 22))
(defparameter Scroll-Panel-left-Offset 15) ; distance between panels
(defparameter Scroll-Panel-Width-Offset (+ Scroll-Panel-Width
					   Scroll-Panel-left-Offset))
)

;; The scroll-gadget is the panel-set
;;
(defun Create-Panel-Set (obj-list maxwidth left top)
  (setf obj-list (append obj-list (list NIL))) ; add empty panel for new objs
  (let* ((num (length obj-list))
	 (scroll-gadget (create-instance NIL
			    garnet-gadgets:scrolling-window-with-bars
;;;			  (:constant T)
			  (:left left) (:top top)
			  (:title "C32")
			  (:v-scroll-bar-p NIL)
			  (:h-page-incr Scroll-Panel-Width-Offset)
			  (:width maxwidth)
			  (:height Panel-Set-Total-Height)
			  (:total-height Panel-Set-Height)
			  (:total-width (o-formula (+ (gvl :aggrel :width) 2)
						   (+ Header-Width 20)))))
	 aggrel panels panel obj agg)
    (opal:update scroll-gadget)
    (declare-constant scroll-gadget :outer-window)
    (declare-constant (g-value scroll-gadget :outer-window) :scroll-win-gadget)
    (setf agg (g-value scroll-gadget :inner-aggregate))
    (setq aggrel (create-instance NIL opal:aggrelist
;;;		   (:constant T)
		   (:left 2)(:top 2)
		   (:direction :horizontal)
		   (:h-spacing Scroll-Panel-left-Offset)))
    (opal:add-component agg aggrel)
    (dotimes (i num)
      (setq obj (nth i obj-list))
      (setq panel (Create-Panel-For-Obj obj 0 0 ; position is set by aggrelist
					aggrel))
      (push panel panels))
    (s-value scroll-gadget :c32-obj-list obj-list)
    (s-value scroll-gadget :c32-panels (reverse panels))
    (s-value scroll-gadget :aggrel aggrel)
    (opal:update scroll-gadget)
    (Create-Panel-Set-Inters scroll-gadget)
    scroll-gadget))


(defun Add-New-Panel-For-Obj (obj panel-set)
  (let ((aggrel (g-value panel-set :aggrel))
	(panels (g-value panel-set :c32-panels))
	(obj-list (g-value panel-set :c32-obj-list))
	panel)
    (setq panel (Create-Panel-For-Obj obj 0 0 ; position is set by aggrelist
					aggrel))
    ;; add to end of lists
    (s-value panel-set :c32-panels (append panels (list panel)))
    (s-value panel-set :c32-obj-list (append obj-list (list obj)))
    (opal:update panel-set)
    panel))

(when gem::*x11-server-available*

  (create-instance 'form-sel-feedback opal:rectangle
    (:fast-redraw-p T)
    (:draw-function :xor)
    (:filling-style opal:black-fill)
    (:line-style NIL)
    (:obj-over NIL)
    (:visible (o-formula (gvl :obj-over)))
    (:left (o-formula (- (gvl :obj-over :left) 1)))
    (:top (o-formula (- (gvl :obj-over :top) 1)))
    (:width (o-formula (+ (gvl :obj-over :width) 2)))
    (:height (o-formula (+ (gvl :obj-over :height) 2))))

  (create-instance 'line-3 opal:line-style
    (:line-thickness 3))
  (create-instance 'gray-line-3 line-3
    (:stipple (g-value opal:gray-fill :stipple)))

  (create-instance 'outline-feedback form-sel-feedback
    ;; left is fine, others need adjusting
    (:top (o-formula (- (gvl :obj-over :top) 2)))
    (:width (o-formula (+ (gvl :obj-over :width) 1)))
    (:height (o-formula (gvl :obj-over :height)))
    (:filling-style NIL)
    (:line-style line-3))
  (create-instance 'gray-outline-feedback outline-feedback
    (:line-style gray-line-3)))


;; procedure called from the interactor that selects a formula icon
;;
(defun Get-Form-Win-For-Icon (inter form-icon)
  (declare (ignore inter))
  (let* ((c32-item (g-value form-icon :parent))
	 (slot (g-value c32-item :slot))
	 (obj (g-value c32-item :obj))
	 (outer-win (g-value c32-item
			     :window :scroll-win-gadget :outer-window)))
    (when (and obj slot outer-win)
      (let ((left (g-value outer-win :left))
	    (top (+ 3 (opal:bottom outer-win))))
	(Assign-formula-win obj slot left top c32-item)))))


(defun Create-Panel-Set-Inters (panel-set)
  (let* ((window (g-value panel-set :inner-window))
	 (agg (g-value panel-set :inner-aggregate))
	 (textinter (create-instance 'a-panel-inter
			garnet-gadgets::scrolling-input-text-edit
;;;		      (:constant T)
		      (:start-where (list
				     :leaf-element-of agg
				     :type Value-Scrolling-String))
		      (:window window)
		      (:active T)
		      (:operates-on (o-formula (gvl :first-obj-over)))
		      (:waiting-priority inter:high-priority-level)))
	 (feedback (create-instance NIL form-sel-feedback))
	 (mainfeedback (create-instance NIL form-sel-feedback
;;;			 (:constant T)
			 (:left (o-formula (+ 1(gvl :obj-over :parent :left))))
			 (:top (o-formula (- (gvl :obj-over :parent :top) 1)))
			 (:width (o-formula
				  (- (gvl :obj-over :parent :width) 2)))
			 (:height (o-formula
				   (- (gvl :obj-over :parent :height) 2)))))
	 (slot-i-feedback (create-instance NIL outline-feedback))
	 (slot-f-feedback (create-instance NIL gray-outline-feedback))
	 (forminter (create-instance NIL inter:menu-interactor
		      (:start-where (list :leaf-element-of agg
					  :type form-icon))
		      (:window window)
		      (:feedback-obj feedback)
		      (:final-function 'Get-Form-Win-For-Icon)))
	 (mainselectslot (create-instance NIL inter:menu-interactor
			   (:start-where (list :leaf-element-of agg
					       :type c32-label))
			   (:window window)
			   (:start-event :leftdown)
			   (:feedback-obj mainfeedback)
			   (:how-set :toggle) ; allow it to be deselected also
			   (:final-feedback-obj mainfeedback)))
	 (secselectslot (create-instance NIL inter:menu-interactor
			  (:start-where (list :leaf-element-of agg
					      :type c32-item))
			  (:window window)
			  (:start-event :middledown)
			  (:feedback-obj slot-i-feedback)
			  (:how-set :toggle) ; allow it to be deselected also
			  (:final-feedback-obj slot-f-feedback)
			  (:final-function NIL))))

    (opal:add-components agg mainfeedback
			 feedback slot-i-feedback slot-f-feedback)
    (s-value panel-set :value-inter textinter)
    (s-value panel-set :form-inter forminter)
    (s-value panel-set :select-slot-inter secselectslot)
    (s-value panel-set :main-select-slot-inter mainselectslot)
    (setq *Current-Selection-Feedback* mainfeedback)
    (setq *Current-Sec-Selection-Feedback* slot-f-feedback)
    (opal:update window)))




;;; ------------------------------------------------------------------
;;;
;;; Set :SLOTS-TO-SHOW for several Garnet prototype objects.  This slot
;;; tells C32 which slots to display.
;;;


(s-value opal:view-object :slots-to-show '(:left :top :width :height :visible))
(s-value opal:graphical-object :slots-to-show '(:left :top :width :height :visible
			:line-style :filling-style
			:draw-function :window :parent :is-a))
(s-value opal:color :slots-to-show '(:red :green :blue :is-a))
(s-value opal:line-style :slots-to-show '(:line-thickness :line-style
		      :foreground-color :background-color :is-a))
(s-value opal:filling-style :slots-to-show '(:foreground-color :background-color
                      :fill-style :stipple :is-a))
(s-value opal:line :slots-to-show '(:x1 :y1 :x2 :y2 :left :top :width :height :visible
			:line-style :filling-style
			:draw-function :window :parent :is-a))
(s-value opal:roundtangle :slots-to-show '(:left :top :width :height :radius :visible
			:line-style :filling-style
			:draw-function  :window :parent :is-a))
(s-value opal:multipoint :slots-to-show '(:point-list :left :top :width
			:height :radius :visible
			:line-style :filling-style
			:draw-function  :window :parent :is-a))
(s-value opal:font :slots-to-show '(:family :face :size))
(s-value opal:text :slots-to-show '(:string :font :left :top :width :height :visible
			:line-style :fill-background-p :actual-heightp
			:draw-function  :window :parent :is-a))
(s-value opal:multi-text :slots-to-show '(:string :font :left :top :width
			:height :justification :visible
			:line-style :fill-background-p :actual-heightp
			:draw-function :window :parent  :is-a))
(s-value opal:cursor-text :slots-to-show '(:string :cursor-index :font :left :top :width
			:height :visible
			:line-style :fill-background-p :actual-heightp
			:draw-function  :window :parent :is-a))
(s-value opal:cursor-multi-text :slots-to-show '(:string :cursor-index
			:font :left :top :width :height :justification :visible
			:line-style :fill-background-p :actual-heightp
			:draw-function  :window :parent :is-a))
(s-value opal:aggregate :slots-to-show '(:components :left :top :width
						     :height :visible
						     :window :parent :is-a))
(s-value opal::window :slots-to-show '(:left :top :width :height :visible
			:title :icon-title :aggregate :parent
                        :border-width :position-by-hand :cursor :is-a))
(s-value inter:interactor :slots-to-show '(:start-where :running-where :start-event
			:stop-event :abort-event :continuous :feedback-obj
			:running-where :final-function :active :is-a))
(s-value inter:menu-interactor :slots-to-show '(:start-where :running-where :start-event
			:stop-event :abort-event :continuous :feedback-obj
			:final-feedback-obj :how-set
			:running-where :final-function :active :is-a))
(s-value inter:button-interactor :slots-to-show '(:start-where
			:running-where :start-event
			:stop-event :abort-event :continuous :feedback-obj
			:final-feedback-obj :how-set
			:running-where :final-function :active :is-a))
(s-value inter:Move-grow-interactor :slots-to-show '(:start-where
			:running-where :start-event
			:stop-event :abort-event :continuous :feedback-obj
			:line-p :grow-p :obj-to-change :attach-point
			:min-width :min-height :min-length
			:running-where :final-function :active :is-a))
(s-value inter:two-point-interactor :slots-to-show '(:start-where
			:running-where :start-event
			:stop-event :abort-event :continuous :feedback-obj
			:line-p :min-width :min-height :min-length
			:abort-if-too-small :flip-if-change-side
			:running-where :final-function :active :is-a))
(s-value inter:text-interactor :slots-to-show '(:start-where :running-where
			:start-event :stop-event :abort-event :continuous
			:feedback-obj :obj-to-change :cursor-where-press
			:running-where :final-function :active :is-a))


(defvar lapidary-p NIL
  "This variable is T when C32 is being run as part of Lapidary.  In that
  case, several functions in the user interface are somewhat different.")



;;;  USER INTERFACE
;;

(defvar error-gadget-object nil)

;; Display an error message, let the user click on OK to remove it.
;;
(defun C32error (str)
  (unless error-gadget-object
    (setf error-gadget-object		; first time - create it.
	  (create-instance nil garnet-gadgets:error-gadget)))
  (gg:display-error error-gadget-object str))


(defvar query-gadget-object nil)

(defun c32-query (string)
  (unless query-gadget-object
    (setf query-gadget-object		; first time - create it.
	  (create-instance nil garnet-gadgets:query-gadget)))
  (string= "OK"
	   (gg:display-query-and-wait query-gadget-object string)))



;; Returns the c32-item that is selected, or NIL if none.
;;
(defun Get-Selected-Item ()
  (let ((label (g-value *Current-Selection-Feedback* :obj-over)))
    (when label (g-value label :parent))))


;; Returns the c32-item that is secondary (middle button) selected,
;; or NIL if none.
(defun Get-Sec-Selected-Item ()
  (let ((label (g-value *Current-Sec-Selection-Feedback* :obj-over)))
    label))


;; Called by "Point To Object..."
;;
(defun NewColumnForObj (obj)
  (if *Current-Panel-Set*
      (let ((panel (car (last (g-value
			       (car (g-value *Current-Panel-Set*
					     :inner-window :aggregate
					     :components)) :components)))))
	;; This is the empty panel on the right
	(create-new-panel-object nil)
	;; Show the new object.
	(s-value panel :obj obj)
	(New-Obj-For-Panel obj panel)
	(s-value (g-value panel :header) :value (mk-string obj))
	;; Turn off selections.
	(turn-selections-off))
      ;; else create a new one
      (progn
	(setq *Current-Panel-Set* (Create-Panel-Set (list obj) 700 2 2))
	(pushnew (g-value *Current-Panel-Set* :outer-window) *All-windows*))))


(defun PointToObject (gadget sel)
  (declare (ignore gadget sel))
  (Pop-Up-Request-Point-Object #'NewColumnForObj NIL))

(defun ReferencesToSlot (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (if item
	(Create-Trace item *Current-Panel-Set* T)
	(C32Error "No slot selected for ReferencesToSlot"))))

(defun ReferencesFromSlot (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (if item
	(Create-Trace item *Current-Panel-Set* NIL)
	(C32Error "No slot selected for ReferencesFromSlot"))))

(defun ClearReferences (gadget sel)
  (declare (ignore gadget sel))
  (Clear-Traces))


(defun CopyFormula (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item))
	(secselectitem (Get-Sec-Selected-Item)))
    (cond ((null item) (c32error "No primary selection for copyformula"))
	  ((null secselectitem)
	   (c32error "No secondary selection for copyformula"))
	  (T (Start-Copy-Formula secselectitem item)))))

(declaim (special pop-up-functions)) ; object name for functions

(defun DeleteSlot (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (if item
	(let ((obj (g-value item :obj))
	      (slot (g-value item :slot)))
	  (if (and (member slot *dangerous-slots*)
		   (not (c32-query
			 (format
			  nil
			  "Do you really want to delete the ~A slot?"
			  slot))))
	      (return-from DeleteSlot nil))
	  (Destroy-constraint obj slot)
	  (Destroy-slot obj slot)
	  (setf (g-value obj :slots-to-show)
		(delete slot (g-value obj :slots-to-show)))
	  (turn-selections-off)
	  (New-Obj-For-Panel obj (g-value item :parent :parent)))
	(C32Error "No slot selected for DeleteSlot"))))


(defun HideSlot (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (if item
	(let ((obj (g-value item :obj))
	      (slot (g-value item :slot))
;;;	      (aggrel (g-value item :parent))
	      )
	  (when slot
	    (setf (g-value obj :slots-to-show)
		  (delete slot (g-value obj :slots-to-show)))
	    ;; Move the selection to the next item in the panel.
;;;	    (let ((next-item (second (member item
;;;					     (g-value aggrel :components))))
;;;		  (panel (g-value aggrel :parent)))
;;;	      (opal:remove-component aggrel item)
;;;	      (opal:update (g-value next-item :window))
;;;	      (s-value *Current-Selection-Feedback* :obj-over
;;;		       (g-value next-item :label))
;;;	      (s-value panel :c32-items (delete item (g-value panel :c32-items)))
;;;	      (s-value item :visible nil)
;;;	      (pushnew item Extra-C32-Items))
	    (New-Obj-For-Panel obj (g-value item :parent :parent))))
	(C32Error "No slot selected for Hide Slot"))))


;; Returns: a list of all the slots in the <object>, both local and
;; inherited.
;;
(defun all-slots (object)
  (let ((slots nil))
    (doslots (slot object T)
      (push slot slots))
    (nreverse slots)))

;; Make ALL the slots of an object visible.
;;
(defun ShowAllSlots (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (unless item
      (let ((panel nil)
	    (found 0))
	(dolist (p (g-value *current-panel-set* :c32-panels))
	  (when (and (g-value p :parent) (g-value p :obj))
	    (setf panel p)
	    (incf found)))
	(if (= found 1)
	    ;; Only one active panel, so we can use this.
	    (setf item (car (g-value panel :c32-items)))
	    (let ((panels NIL))
	      ;; There is no single panel.  Try with panels that currently show
	      ;; no items.
	      (dolist (p (g-value *current-panel-set* :c32-panels))
		(when (and (g-value p :parent) (null (cdr (g-value p :c32-items))))
		  (pushnew p panels)))
	      (when (= 1 (length panels))
		(setf item (car (g-value (car panels) :c32-items))))))))
    (if item
	(let* ((obj (g-value item :obj))
	       (slots (all-slots obj)))
	  ;; Eliminate internal C32 slots, which we don't want to see.
	  (if (member :slots-to-show slots)
	      (setf slots (delete :slots-to-show slots)))
	  (if (member :C32-count slots)
	      (setf slots (delete :C32-count slots)))
	  (setf (g-value obj :slots-to-show) slots)
	  (New-Obj-For-Panel obj (g-value item :parent :parent)))
	(C32Error "No item selected for Show All Slots"))))



;; Add a new slot.  If slot is already in the C32 panel, error.  Otherwise,
;; add (if not already present in the object), and display.
;;
(defun AddSlotAction (gadget obj string)
  (multiple-value-bind (slot no-error value)
      (C32-Careful-String-Eval string NIL T)
    (declare (ignore no-error))
    (if (stringp slot)
	;; Transform to a keyword
	(setf slot (intern slot (find-package "KEYWORD"))))
    (if (symbolp slot)
      (let ((panel (g-value gadget :parent :parent :parent)))
	(if (not (keywordp slot))
	  (setf slot (read-from-string (format nil ":~A" slot))))
	(when (member slot (g-value obj :slots-to-show))
	  (C32Error (format nil "Slot ~A is already shown!" slot))
	  (return-from AddSlotAction nil))
	(setf (g-value obj :slots-to-show) ; show this slot, too.
	      (append (g-value obj :slots-to-show) (list slot)))
	(unless (has-slot-p obj slot)
	  (unless (kr::inherited-p obj slot)
	    (if value
	      (s-value obj slot value)
	      (let ((inh-v (g-value obj slot)))	;; Try to inherit
		(unless (and inh-v (kr::inherited-p obj slot))
		  ;; Create slot with initial value NIL.
		  (s-value obj slot NIL))))))
	;; Add an item to the panel
	(add-new-row panel slot)
;;;	(New-Obj-For-Panel obj panel)
	(opal:update (g-value panel :window)))
      (C32Error "Please type a symbol or keyword to create a new slot."))))



(defun do-stop ()
  (dolist (win *All-windows*)
    (if (schema-p win)
      (opal:destroy win)))
  (if (and (boundp 'pop-up-functions)
	   (schema-p pop-up-functions)
	   ;; Destroy Function window, if it exists.
	   (schema-p (g-value pop-up-functions :window)))
    (opal:destroy (g-value pop-up-functions :window)))
  (setq *Current-Panel-Set* NIL)
  (setq *all-windows* nil)
  (setf *Current-Selection-Feedback* nil)
  (setf *Current-Sec-Selection-Feedback* nil)
  (setf Extra-C32-Items nil))



(defun QuitFunc (gadget sel)
  (if lapidary-p
    ;; Run the specialized version.
    (lapidary-QuitFunc gadget sel)
    ;; Just get out of the whole thing.
    (progn
      (do-stop)
      #-cmu (inter:exit-main-event-loop))))


(defun set-current-package (gadget value)
  (let ((p (find-package (string-upcase value))))
    (when p
      (setf *c32-package* p)
      ;; Use official package name
      (setf (g-value gadget :value) (package-name p)))))


(defun Create-Main-Menu (left top)
  (let (win agg menu)
    (setq win (create-instance NIL inter:interactor-window
		(:top top) (:left left) (:width 286) (:height 195)
		(:title "C32 Commands")
		(:aggregate (setq agg (create-instance NIL opal:aggregate
					; (:constant :parent :visible T)
					(:visible T))))))
    (setq menu (create-instance NIL garnet-gadgets:text-button-panel
		 (:left 5) (:top 5)
		 (:final-feedback-p NIL)
		 (:rank-margin 5)
		 (:shadow-offset 4) (:text-offset 2) (:gray-width 2)
		 (:font opal:default-font)
		 #+DZG
		 (:constant :parent :visible :text-offset :gray-width
			    :shadow-offset :rank-margin :font T)
		 (:visible T)
		 (:items '(("Point To Object..." PointToObject)
			   ("Slots Using Me" ReferencesToSlot)
			   ("Slots I Use" ReferencesFromSlot)
			   ("Clear References" ClearReferences)
			   ("Delete Slot" DeleteSlot)
			   ("Hide Slot" HideSlot)
			   ("Show All Slots" ShowAllSlots)
			   ("Copy Formula" CopyFormula)
			   ("Quit" QuitFunc)))))
    (with-constants-disabled
	(opal:add-components agg menu))

    (opal:add-component
     agg
     (create-instance nil gg:scrolling-labeled-box
       (:left 10) (:top 170) (:width 270)
       (:label-string "Current package:") (:value "COMMON-LISP-USER")
       (:selection-function 'set-current-package)
;;;    (:constant T)
       ))
    (opal:Update win)
    win))


(defun create-new-panel-object (objects)
  (declare (ignore objects))
  (let ((panel (Add-New-Panel-For-Obj NIL *Current-Panel-Set*)))
    (s-value (g-value panel :header) :value "name")))


(defun do-go (&key (startup-objects NIL) (test-p NIL)
		   #-CMU (start-event-loop-p T))
  (let ((main-win (create-main-menu 500 45))
	c32win agg)
    (push main-win *All-windows*)
    (setq *Current-Panel-Set* NIL)

    (when test-p
      ;; Create a test window
      (create-instance 'w inter:interactor-window
	(:left 850)
        (:top 45) (:width 150) (:height 150)
	(:aggregate (setq agg (create-instance 'demo-agg opal:aggregate))))
      (create-instance 'r opal:rectangle
	(:box '(0 0 0 0))
	(:left (o-formula (first (gvl :box))))
	(:top (o-formula (second (gvl :box)))))
      (create-instance 's opal:text (:string "Tester") (:left 50) (:top 30))
      (create-instance 's2 opal:text (:string "A longer string")
		       (:visible NIL))
      (create-instance 'line1 opal:line
	(:line-style opal:blue-line)
	(:x1 (o-formula (+ (gv r :left) (floor (gv r :width) 2))))
	(:y1 40)(:x2 100)(:y2 10))
      (create-instance 'mybutton garnet-gadgets:text-button
	(:box '(10 100 0 0))
	(:left (o-formula (first (gvl :box))))
	(:top (o-formula (second (gvl :box))))
	(:slots-to-show '(:left :top :width
			  :height :selected :visible
			  :window :parent :is-a)))
      (opal:add-components agg r s s2 line1 mybutton)

      (create-instance 'demo-mover1 inter:move-grow-interactor
	(:start-where `(:in ,mybutton))
	(:window w)
	(:start-event :middledown))
      (create-instance 'demo-mover2 inter:move-grow-interactor
	(:start-where `(:in ,r))
	(:window w)
	(:start-event :middledown))
      (opal:update w)
      (push w *all-windows*))

    (setq *Current-Panel-Set* (Create-Panel-Set startup-objects 640 500 250))
    (setq c32win (g-value *Current-Panel-Set* :outer-window))
    (opal:update c32win)
    (push c32win *all-windows*))
  #-CMU
  (when start-event-loop-p
    (inter:main-event-loop)))
