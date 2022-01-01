;;; -*- mode: lisp; syntax: common-lisp; package: c32; base: 10 -*-
;;
;;
;;  the garnet user interface development environment.       ;;
;;
;;  this code was written as part of the garnet project at           ;;
;;  carnegie mellon university, and has been placed in the public    ;;
;;  domain.                                                          ;;



;;; c32 is a spreadsheet interface for garnet constraints
;;
;;  designed and implemented by brad myers

;;; * bugs
;;   - arrows don't move when panel scrolls (so then point to wrong slot)
;;   - arrows should be under mainsel and other feedback objects
;;   - too slow
;;   - when copy a formula, and then generalize, should replace
;;      original with new formula also.
;;   - if a slot or object used a parameter to a function, should
;;      notice it for copy and generalize
;;   - point-to-mouse object should take into account
;;      *current-formula-win* so the guessed slot changes if change
;;      windows.
;;   - *current-formula-win* should be a queue, so if window deleted when
;;      another is around, second is used



(in-package "C32")

;; (eval-when (:execute :load-toplevel :compile-toplevel)
;;   (export '()))

;; Holds the feeback obj for the current selection
(defparameter *current-selection-feedback* nil)

;; Holds the feeback obj for the second (middle button) selection
(defparameter *current-sec-selection-feedback* nil)

;; List of all windows, for quit
(defparameter *all-windows* nil)

;; The current main panel-set
(defparameter *current-panel-set* nil)


(defparameter *c32-package* (find-package "common-lisp-user")
  "This package is used for all read operations.  it allows the user
   to type values and object names without a package prefix.")

;; List of slots that require confirmation for dangerous operations
;; (such as delete slot).
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
			      common-lisp-user::garnet-c32-bitmap-pathname))))
  (create-instance 'inherited-icon opal:bitmap
    (:image (opal:read-image (merge-pathnames
			      "inherited-icon.bm"
			      common-lisp-user::garnet-c32-bitmap-pathname))))
  (defparameter font-height (max (opal:string-height ital-font "/")
				 (opal:string-height reg-font "/")))
  (defparameter font-char-width (max (opal:string-width ital-font "/")  ; fixed width
				     (opal:string-width reg-font "/"))) ; fonts
  (defparameter form-icon-width (g-value form-icon :width))
  (defparameter inherited-icon-width (g-value inherited-icon :width))
  (defparameter label-width 100)
  (defparameter max-value-width 120)
  (defparameter label-num-chars (floor label-width font-char-width))
  (defparameter label-side-width (+ label-width form-icon-width inherited-icon-width
				    -4)) ; fudge factor
  (defparameter icon-at-right-offset (+ label-side-width 2 max-value-width))
  (defparameter full-item-width (+ icon-at-right-offset inherited-icon-width 2))
  (defparameter scroll-panel-width (+ 20 full-item-width))
  (defparameter scroll-panel-num-items 12)
  (defparameter scroll-panel-height (* scroll-panel-num-items (+ 1 font-height))))

(defun mk-string (val)
  ;; turn off #k<...> notation
  (let ((kr::*print-as-structure* nil))
	(format nil "~s" val)))

;; If val is in the keyword package, then put a colon in front.  clip
;; the string to len characters
(defun mk-colon-str-and-clip (val len)
  (if (null val)
      ;; empty
      ""
      ;; else generate a string
      (let ((str (string-capitalize val)))
	(if (keywordp val)
	    (setq str (concatenate 'simple-string ":" str)))
	(if (> (length str) len)
	    (subseq str 0 len)
	    str))))

(defun turn-selections-off ()
  (s-value *current-selection-feedback* :obj-over nil)
  (s-value *current-sec-selection-feedback* :obj-over nil))

;; Have an extra type so can look for it from the interactor
(create-instance 'value-scrolling-string garnet-gadgets:scrolling-input-string)

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

(defparameter *header-left-offset* 20)
(defparameter *header-height* (+ 2 font-height))
(defparameter *header-width* (+ full-item-width 3))

(create-instance 'c32-panel opal:aggregadget
  (:left 0)
  (:top 0)
  (:c32-items nil)
  (:obj nil)
  (:max-index (o-formula (- (length (gvl :c32-items)) 1)
			 4))		; initial value
  (:width (+ *header-width* 20))		; header + scrollbar(=20)
  (:height (+ scroll-panel-height *header-height*))
  (:parts
   `((:header ,garnet-gadgets:scrolling-input-string
					; (:constant t)
	      (:value ,(o-formula (if (gvl :parent :obj)
				      (mk-string (gvl :parent :obj))
				      "")))
	      (:font ,(o-formula (if (gvl :parent :obj)
				     bold-font
				     reg-font)))
	      (:width ,(- *header-width* 2))
	      (:selection-function header-edited-func)
	      (:left ,(o-formula (+ (gvl :parent :left) *header-left-offset*
				    (if (gvl :parent :obj) 0 60))))
	      (:top ,(o-formula (+ 1 (gvl :parent :top)))))
     (:header-rect ,opal:rectangle
					; (:constant t)
		   (:left ,(o-formula (+ *header-left-offset* (gvl :parent :left))))
		   (:top ,(o-formula (gvl :parent :top)))
		   (:width ,*header-width*)
		   (:height ,*header-height*))
     (:border ,opal:rectangle
					; (:constant t)
	      (:left ,(o-formula (+ *header-left-offset* (gvl :parent :left))))
	      (:top ,(o-formula (+ *header-height* (gvl :parent :top))))
	      (:width ,*header-width*)
	      (:height ,scroll-panel-height))
     (:scroll-bar ,garnet-gadgets:v-scroll-bar
					; (:constant t)
		  (:left ,(o-formula (gvl :parent :left)))
		  (:top ,(o-formula (gvl :parent :top)))
		  (:height ,(o-formula (+ scroll-panel-height *header-height*)))
		  (:val-1 0)
		  (:val-2 ,(o-formula (gvl :parent :max-index)))
		  (:scroll-p ,(o-formula (>= (gvl :val-2) scroll-panel-num-items)))
		  (:page-incr ,(1- scroll-panel-num-items))
		  (:indicator-text-p nil))
     (:aggrel ,opal:aggrelist
	      ;; items are added explicitly
					; (:constant t)
	      (:left ,(o-formula (+ 2 *header-left-offset* (gvl :parent :left))))
	      (:top ,(o-formula (+ 2 *header-height* (gvl :parent :top))))
	      (:v-spacing 1))
     (:vert-line ,opal:line
					; (:constant t)
		 (:x1 ,(o-formula (+ (gvl :parent :left)
				     *header-left-offset* 8 label-side-width)))
		 (:x2 ,(o-formula (gvl :x1)))
		 (:y1 ,(o-formula (+ *header-height*
				     (gvl :parent :top))))
		 (:y2 ,(o-formula (opal:gv-bottom (gvl :parent :border)))))
     ;; this is only used by the empty panel
     (:empty-title ,opal:text
					; (:constant t)
		   (:left ,(o-formula (+ (gvl :parent :left) 25)))
		   (:top ,(o-formula (+ (gvl :parent :top) 1)))
		   (:string "object:")
		   (:font ,bold-font)
		   (:visible ,(o-formula (null (gvl :parent :obj))))))))

(defparameter *extra-c32-panels* nil)

;; Creates a scrolling window panel for the object
(defun create-panel-for-obj (obj left top agg)
  (let ((panel (pop *extra-c32-panels*)))
    (if (schema-p panel)
	(progn
	  (s-value panel :left left)
	  (s-value panel :top top))
	;; else create a new one
	(setf panel (create-instance nil c32-panel
		      ;; (:constant t)
		      (:left left) (:top top))))
    (unless obj
      (s-value (g-value panel :header) :value "name"))
    (opal:add-component agg panel)
    (new-obj-for-panel obj panel)	; sets all the items
    panel))

;; If an object is shown in only one panel, and the panel is removed, we
;; need to restore the object's original :destroy method, which was modified
;; when we displayed the object in the panel.
(defun restore-destroy-method (panel)
  (let* ((object (g-value panel :obj))
	 (count-and-method (g-local-value object :c32-count))
	 (count (car count-and-method)))
    (when (plusp count)
      (setf (car count-and-method) (decf count))
      (when (zerop count)
	;; count is down to 0, so last panel for this objects was removed.
	;; restore the original method.
	(s-value object :destroy-me (cdr count-and-method))))))

(defun remove-panel (panel)
  ;; Eliminate the panel.
  (let ((parent (g-value panel :parent)))
    (opal:remove-component parent panel)
    (opal:notice-items-changed parent))  ; dzg
  ;; Mmake it reusable.
  (push panel *extra-c32-panels*)
  (restore-destroy-method panel))

(defparameter *extra-c32-items* nil)

(defun get-c32-item (obj slot)
  (let ((item (pop *extra-c32-items*)))
    (if item
	(progn
	  (s-value item :obj obj)
	  (s-value item :slot slot)
	  item)
	;; else, create a new one
	(create-instance nil c32-item
	  (:obj obj)
	  (:slot slot)))))


;; Encapsulate objects' original :destroy-me method. First, it
;; eliminates the panel(s) that display an object being destroyed.
;; Then, it calls the original :destroy-me method.
(defun c32-destroy-method (object &optional other)
  (when (schema-p *current-panel-set*)
    (dolist (panel (g-value *current-panel-set* :c32-panels))
      (when (and (schema-p panel)
		 (eq (g-value panel :obj) object))
	;; this is the panel that is displaying the <object>.
	(remove-panel panel)
	(return))))
  ;; now invoke the original destroy-me method
  (let ((method (cdr (g-local-value object :c32-count))))
    (when method
      (funcall method object other))))

;; Assigns a new object to the panel.
(defun new-obj-for-panel (obj panel)
  (let ((slots (if obj (append (g-value obj :slots-to-show) (list nil))))
	;; add a special nil slot at the end of each one as a place-holder.
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
	    (setq item (get-c32-item obj slot))
	    (opal:add-component aggrel item)))
      (push item items))
    (when old-items			; some left over
      (dolist (o old-items)
	(opal:remove-component aggrel o))
      (setq *extra-c32-items* (append *extra-c32-items* old-items)))
    (s-value panel :c32-items (reverse items))
    ;; make sure that all items, even ones that had previously become
    ;; invisible, are displayed.
    (dolist (item items)
      (s-value item :visible t))
    ;; now install a modified destroy method on the object.  this ensures
    ;; that external changes to the object (i.e., calling destroy from the
    ;; lisp listener) will be reflected in the panel.
    (when obj
      (let ((count-and-method (g-local-value obj :c32-count)))
	(unless count-and-method
	  ;; store counter and original destroy method
	  (s-value obj :c32-count (cons 1 (g-value obj :destroy-me)))
	  (s-value obj :destroy-me 'c32-destroy-method))))))

(defun add-new-row (panel slot)
  (let* ((aggrel (g-value panel :aggrel))
	 (item (get-c32-item (g-value panel :obj) nil))
	 (items (g-value panel :c32-items))
	 (scroll-bar (g-value panel :scroll-bar))
	 (value (g-value scroll-bar :value))
	 (length (length (g-value aggrel :components))))
    (opal:add-component aggrel item)
    (s-value (car (last items)) :slot slot)
    (s-value panel :c32-items (append items (list item)))
    ;; restore the scroll bar, to avoid unwanted jumps.  however, make sure
    ;; the empty slot at the bottom remains visible.
    (when (>= (- length value) 12)
      (setf value (- length 11)))
    (s-value scroll-bar :value value)))

;; checks to see if new-value is the string name of an object
;; ** unwind-protect doesn't work, so don't bother
(defun careful-get-obj (new-value)
  (let* ((*package* *c32-package*)
	 (val (read-from-string (string-upcase new-value))))
    (when (symbolp val)
      (unless (boundp val)
	(if (c32-query (format nil "object ~s does not exist - create?" val))
	    (create-schema val)
	    (return-from careful-get-obj nil)))
      (setq val (eval val))
      (when (schema-p val)
	val))))

(defun value-edited-func (gadget newvalue)
  (let* ((c32-item (g-value gadget :parent))
	 (slot (g-value c32-item :slot))
	 (obj (g-value c32-item :obj)))
    (if (null slot)
	(addslotaction gadget obj newvalue) ; add a slot
	(when obj			    ; edit a value
	  (if (kr::slot-constant-p obj slot)
	      ;; constant.
	      (progn
		(recompute-formula (g-value gadget :string :parent) :value)
		(c32error "slot is constant - not set."))
	      ;; ok.
	      (multiple-value-bind (value success)
		  (c32-careful-string-eval newvalue nil nil)
		(if success
		    (s-value obj slot value)
		    ;; revert to the original string.
		    (recompute-formula gadget :value))))))))

;; the title of a panel was edited.  switch to a new object, create a new
;; object, or eliminate the panel.
(defun header-edited-func (gadget newvalue)
  (let ((panel (g-value gadget :parent))
	new-obj)
    (if (string= newvalue "")
	(remove-panel panel)
	(progn
	  (setq new-obj (careful-get-obj newvalue))
	  (if new-obj
	      ;; valid object.
	      (progn
		(unless (g-value panel :obj)
		  ;; this is the empty panel on the right
		  (create-new-panel-object nil))
		;; show a new object.
		(s-value panel :obj new-obj)
		(new-obj-for-panel new-obj panel)
		;; turn off selections.
		(turn-selections-off))
	      ;; bad object
	      (let ((obj (g-value gadget :parent :obj)))
		(if obj
		    (s-value gadget :value (format nil "~s" obj)) ; show old value
		    (s-value gadget :value "name"))		  ; empty panel
		(inter:beep)))))))

(when gem::*x11-server-available*
  (defparameter panel-set-height (+ 2 scroll-panel-height *header-height*))
  (defparameter panel-set-total-height (+ panel-set-height 22))
  (defparameter scroll-panel-left-offset 15) ; distance between panels
  (defparameter scroll-panel-width-offset (+ scroll-panel-width
					     scroll-panel-left-offset)))
;; The scroll-gadget is the panel-set
(defun create-panel-set (obj-list maxwidth left top)
  (setf obj-list (append obj-list (list nil))) ; add empty panel for new objs
  (let* ((num (length obj-list))
	 (scroll-gadget (create-instance nil
			    garnet-gadgets:scrolling-window-with-bars
;;;			  (:constant t)
			  (:left left) (:top top)
			  (:title "c32")
			  (:v-scroll-bar-p nil)
			  (:h-page-incr scroll-panel-width-offset)
			  (:width maxwidth)
			  (:height panel-set-total-height)
			  (:total-height panel-set-height)
			  (:total-width (o-formula (+ (gvl :aggrel :width) 2)
						   (+ *header-width* 20)))))
	 aggrel panels panel obj agg)
    (opal:update scroll-gadget)
    (declare-constant scroll-gadget :outer-window)
    (declare-constant (g-value scroll-gadget :outer-window) :scroll-win-gadget)
    (setf agg (g-value scroll-gadget :inner-aggregate))
    (setq aggrel (create-instance nil opal:aggrelist
;;;		   (:constant t)
		   (:left 2)(:top 2)
		   (:direction :horizontal)
		   (:h-spacing scroll-panel-left-offset)))
    (opal:add-component agg aggrel)
    (dotimes (i num)
      (setq obj (nth i obj-list))
      (setq panel (create-panel-for-obj obj 0 0 ; position is set by aggrelist
					aggrel))
      (push panel panels))
    (s-value scroll-gadget :c32-obj-list obj-list)
    (s-value scroll-gadget :c32-panels (reverse panels))
    (s-value scroll-gadget :aggrel aggrel)
    (opal:update scroll-gadget)
    (create-panel-set-inters scroll-gadget)
    scroll-gadget))

(defun add-new-panel-for-obj (obj panel-set)
  (let ((aggrel (g-value panel-set :aggrel))
	(panels (g-value panel-set :c32-panels))
	(obj-list (g-value panel-set :c32-obj-list))
	panel)
    (setq panel (create-panel-for-obj obj 0 0 ; position is set by aggrelist
				      aggrel))
    ;; add to end of lists
    (s-value panel-set :c32-panels (append panels (list panel)))
    (s-value panel-set :c32-obj-list (append obj-list (list obj)))
    (opal:update panel-set)
    panel))

(when gem::*x11-server-available*
  (create-instance 'form-sel-feedback opal:rectangle
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:filling-style opal:black-fill)
    (:line-style nil)
    (:obj-over nil)
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
    (:filling-style nil)
    (:line-style line-3))
  (create-instance 'gray-outline-feedback outline-feedback
    (:line-style gray-line-3)))

;; procedure called from the interactor that selects a formula icon
(defun get-form-win-for-icon (inter form-icon)
  (declare (ignore inter))
  (let* ((c32-item (g-value form-icon :parent))
	 (slot (g-value c32-item :slot))
	 (obj (g-value c32-item :obj))
	 (outer-win (g-value c32-item
			     :window :scroll-win-gadget :outer-window)))
    (when (and obj slot outer-win)
      (let ((left (g-value outer-win :left))
	    (top (+ 3 (opal:bottom outer-win))))
	(assign-formula-win obj slot left top c32-item)))))

(defun create-panel-set-inters (panel-set)
  (let* ((window (g-value panel-set :inner-window))
	 (agg (g-value panel-set :inner-aggregate))
	 (textinter (create-instance 'a-panel-inter
			garnet-gadgets::scrolling-input-text-edit
;;;		      (:constant t)
		      (:start-where (list
				     :leaf-element-of agg
				     :type value-scrolling-string))
		      (:window window)
		      (:active t)
		      (:operates-on (o-formula (gvl :first-obj-over)))
		      (:waiting-priority inter:high-priority-level)))
	 (feedback (create-instance nil form-sel-feedback))
	 (mainfeedback (create-instance nil form-sel-feedback
;;;			 (:constant t)
			 (:left (o-formula (+ 1(gvl :obj-over :parent :left))))
			 (:top (o-formula (- (gvl :obj-over :parent :top) 1)))
			 (:width (o-formula
				  (- (gvl :obj-over :parent :width) 2)))
			 (:height (o-formula
				   (- (gvl :obj-over :parent :height) 2)))))
	 (slot-i-feedback (create-instance nil outline-feedback))
	 (slot-f-feedback (create-instance nil gray-outline-feedback))
	 (forminter (create-instance nil inter:menu-interactor
		      (:start-where (list :leaf-element-of agg
					  :type form-icon))
		      (:window window)
		      (:feedback-obj feedback)
		      (:final-function 'get-form-win-for-icon)))
	 (mainselectslot (create-instance nil inter:menu-interactor
			   (:start-where (list :leaf-element-of agg
					       :type c32-label))
			   (:window window)
			   (:start-event :leftdown)
			   (:feedback-obj mainfeedback)
			   (:how-set :toggle) ; allow it to be deselected also
			   (:final-feedback-obj mainfeedback)))
	 (secselectslot (create-instance nil inter:menu-interactor
			  (:start-where (list :leaf-element-of agg
					      :type c32-item))
			  (:window window)
			  (:start-event :middledown)
			  (:feedback-obj slot-i-feedback)
			  (:how-set :toggle) ; allow it to be deselected also
			  (:final-feedback-obj slot-f-feedback)
			  (:final-function nil))))
    (opal:add-components agg mainfeedback
			 feedback slot-i-feedback slot-f-feedback)
    (s-value panel-set :value-inter textinter)
    (s-value panel-set :form-inter forminter)
    (s-value panel-set :select-slot-inter secselectslot)
    (s-value panel-set :main-select-slot-inter mainselectslot)
    (setq *current-selection-feedback* mainfeedback)
    (setq *current-sec-selection-feedback* slot-f-feedback)
    (opal:update window)))


;;; Set :slots-to-show for several garnet prototype objects. This slot
;;; tells c32 which slots to display.

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
(s-value inter:move-grow-interactor :slots-to-show '(:start-where
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

(defvar lapidary-p nil
  "this variable is t when c32 is being run as part of lapidary.  in that
   case, several functions in the user interface are somewhat different.")

;;;  User Interface

(defvar *error-gadget-object* nil)

;; Display an error message, let the user click on ok to remove it.
(defun c32error (str)
  (unless *error-gadget-object*
    ;; first time - create it.
    (setf *error-gadget-object*
	  (create-instance nil garnet-gadgets:error-gadget)))
  (gg:display-error *error-gadget-object* str))

(defvar *query-gadget-object* nil)

(defun c32-query (string)
  (unless *query-gadget-object*
    (setf *query-gadget-object*		; first time - create it.
	  (create-instance nil garnet-gadgets:query-gadget)))
  (string= "ok"
	   (gg:display-query-and-wait *query-gadget-object* string)))

;; Returns the c32-item that is selected, or nil if none.
(defun get-selected-item ()
  (let ((label (g-value *current-selection-feedback* :obj-over)))
    (when label (g-value label :parent))))


;; Returns the c32-item that is secondary (middle button) selected,
;; or nil if none.
(defun get-sec-selected-item ()
  (let ((label (g-value *current-sec-selection-feedback* :obj-over)))
    label))

;; Called by "point to object..."
(defun newcolumnforobj (obj)
  (if *current-panel-set*
      (let ((panel (car (last (g-value
			       (car (g-value *current-panel-set*
					     :inner-window :aggregate
					     :components)) :components)))))
	;; Empty panel on the right
	(create-new-panel-object nil)
	;; Show the new object.
	(s-value panel :obj obj)
	(new-obj-for-panel obj panel)
	(s-value (g-value panel :header) :value (mk-string obj))
	;; Turn off selections.
	(turn-selections-off))
      ;; Else create a new one
      (progn
	(setq *current-panel-set* (create-panel-set (list obj) 700 2 2))
	(pushnew (g-value *current-panel-set* :outer-window) *all-windows*))))

(defun pointtoobject (gadget sel)
  (declare (ignore gadget sel))
  (pop-up-request-point-object #'newcolumnforobj nil))

(defun referencestoslot (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (if item
	(create-trace item *current-panel-set* t)
	(c32error "No slot selected for referencestoslot"))))

(defun referencesfromslot (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (if item
	(create-trace item *current-panel-set* nil)
	(c32error "no slot selected for referencesfromslot"))))

(defun clearreferences (gadget sel)
  (declare (ignore gadget sel))
  (clear-traces))

(defun copyformula (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item))
	(secselectitem (get-sec-selected-item)))
    (cond ((null item) (c32error "no primary selection for copyformula"))
	  ((null secselectitem)
	   (c32error "no secondary selection for copyformula"))
	  (t (start-copy-formula secselectitem item)))))

;; Object name for functions
(declaim (special pop-up-functions))

(defun deleteslot (gadget sel)
  (declare (ignore gadget sel))
  (let ((item (get-selected-item)))
    (if item
	(let ((obj (g-value item :obj))
	      (slot (g-value item :slot)))
	  (if (and (member slot *dangerous-slots*)
		   (not (c32-query
			 (format
			  nil
			  "do you really want to delete the ~a slot?"
			  slot))))
	      (return-from deleteslot nil))
	  (destroy-constraint obj slot)
	  (destroy-slot obj slot)
	  (setf (g-value obj :slots-to-show)
		(delete slot (g-value obj :slots-to-show)))
	  (turn-selections-off)
	  (new-obj-for-panel obj (g-value item :parent :parent)))
	(c32error "no slot selected for deleteslot"))))

(defun hideslot (gadget sel)
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
	    ;; move the selection to the next item in the panel.
;;;	    (let ((next-item (second (member item
;;;					     (g-value aggrel :components))))
;;;		  (panel (g-value aggrel :parent)))
;;;	      (opal:remove-component aggrel item)
;;;	      (opal:update (g-value next-item :window))
;;;	      (s-value *current-selection-feedback* :obj-over
;;;		       (g-value next-item :label))
;;;	      (s-value panel :c32-items (delete item (g-value panel :c32-items)))
;;;	      (s-value item :visible nil)
;;;	      (pushnew item *extra-c32-items*))
	    (new-obj-for-panel obj (g-value item :parent :parent))))
	(c32error "no slot selected for hide slot"))))

;; Returns: a list of all the slots in the <object>, both local and
;; inherited.
(defun all-slots (object)
  (let ((slots nil))
    (doslots (slot object t)
      (push slot slots))
    (nreverse slots)))

;; Make all the slots of an object visible.
(defun showallslots (gadget sel)
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
	    ;; only one active panel, so we can use this.
	    (setf item (car (g-value panel :c32-items)))
	    (let ((panels nil))
	      ;; there is no single panel.  try with panels that currently show
	      ;; no items.
	      (dolist (p (g-value *current-panel-set* :c32-panels))
		(when (and (g-value p :parent) (null (cdr (g-value p :c32-items))))
		  (pushnew p panels)))
	      (when (= 1 (length panels))
		(setf item (car (g-value (car panels) :c32-items))))))))
    (if item
	(let* ((obj (g-value item :obj))
	       (slots (all-slots obj)))
	  ;; eliminate internal c32 slots, which we don't want to see.
	  (if (member :slots-to-show slots)
	      (setf slots (delete :slots-to-show slots)))
	  (if (member :c32-count slots)
	      (setf slots (delete :c32-count slots)))
	  (setf (g-value obj :slots-to-show) slots)
	  (new-obj-for-panel obj (g-value item :parent :parent)))
	(c32error "no item selected for show all slots"))))

;; Add a new slot. If slot is already in the c32 panel,
;; error. Otherwise, add (if not already present in the object), and
;; display.
(defun addslotaction (gadget obj string)
  (multiple-value-bind (slot no-error value)
      (c32-careful-string-eval string nil t)
    (declare (ignore no-error))
    (if (stringp slot)
	;; transform to a keyword
	(setf slot (intern slot (find-package "keyword"))))
    (if (symbolp slot)
	(let ((panel (g-value gadget :parent :parent :parent)))
	  (if (not (keywordp slot))
	      (setf slot (read-from-string (format nil ":~a" slot))))
	  (when (member slot (g-value obj :slots-to-show))
	    (c32error (format nil "slot ~a is already shown!" slot))
	    (return-from addslotaction nil))
	  (setf (g-value obj :slots-to-show) ; show this slot, too.
		(append (g-value obj :slots-to-show) (list slot)))
	  (unless (has-slot-p obj slot)
	    (unless (kr::inherited-p obj slot)
	      (if value
		  (s-value obj slot value)
		  (let ((inh-v (g-value obj slot)))	;; try to inherit
		    (unless (and inh-v (kr::inherited-p obj slot))
		      ;; create slot with initial value nil.
		      (s-value obj slot nil))))))
	  ;; add an item to the panel
	  (add-new-row panel slot)
;;;	(new-obj-for-panel obj panel)
	  (opal:update (g-value panel :window)))
	(c32error "please type a symbol or keyword to create a new slot."))))

(defun do-stop ()
  (dolist (win *all-windows*)
    (if (schema-p win)
	(opal:destroy win)))
  (if (and (boundp 'pop-up-functions)
	   (schema-p pop-up-functions)
	   ;; destroy function window, if it exists.
	   (schema-p (g-value pop-up-functions :window)))
      (opal:destroy (g-value pop-up-functions :window)))
  (setq *current-panel-set* nil)
  (setq *all-windows* nil)
  (setf *current-selection-feedback* nil)
  (setf *current-sec-selection-feedback* nil)
  (setf *extra-c32-items* nil))

(defun quitfunc (gadget sel)
  (if lapidary-p
      ;; run the specialized version.
      (lapidary-quitfunc gadget sel)
      ;; just get out of the whole thing.
      (progn
	(do-stop)
	(inter:exit-main-event-loop))))

(defun set-current-package (gadget value)
  (let ((p (find-package (string-upcase value))))
    (when p
      (setf *c32-package* p)
      ;; use official package name
      (setf (g-value gadget :value) (package-name p)))))

(defun create-main-menu (left top)
  (let (win agg menu)
    (setq win (create-instance nil inter:interactor-window
		(:top top) (:left left) (:width 286) (:height 195)
		(:title "c32 commands")
		(:aggregate (setq agg (create-instance nil opal:aggregate
					; (:constant :parent :visible t)
					(:visible t))))))
    (setq menu (create-instance nil garnet-gadgets:text-button-panel
		 (:left 5) (:top 5)
		 (:final-feedback-p nil)
		 (:rank-margin 5)
		 (:shadow-offset 4) (:text-offset 2) (:gray-width 2)
		 (:font opal:default-font)
		 #+dzg
		 (:constant :parent :visible :text-offset :gray-width
			    :shadow-offset :rank-margin :font t)
		 (:visible t)
		 (:items '(("point to object..." pointtoobject)
			   ("slots using me" referencestoslot)
			   ("slots i use" referencesfromslot)
			   ("clear references" clearreferences)
			   ("delete slot" deleteslot)
			   ("hide slot" hideslot)
			   ("show all slots" showallslots)
			   ("copy formula" copyformula)
			   ("quit" quitfunc)))))
    (with-constants-disabled
      (opal:add-components agg menu))
    (opal:add-component
     agg
     (create-instance nil gg:scrolling-labeled-box
       (:left 10) (:top 170) (:width 270)
       (:label-string "current package:") (:value "common-lisp-user")
       (:selection-function 'set-current-package)
;;;    (:constant t)
       ))
    (opal:update win)
    win))

(defun create-new-panel-object (objects)
  (declare (ignore objects))
  (let ((panel (add-new-panel-for-obj nil *current-panel-set*)))
    (s-value (g-value panel :header) :value "name")))

(defun do-go (&key (startup-objects nil) (test-p nil)
		(start-event-loop-p t))
  (let ((main-win (create-main-menu 500 45))
	c32win agg)
    (push main-win *all-windows*)
    (setq *current-panel-set* nil)
    (when test-p
      ;; create a test window
      (create-instance 'w inter:interactor-window
	(:left 850)
        (:top 45) (:width 150) (:height 150)
	(:aggregate (setq agg (create-instance 'demo-agg opal:aggregate))))
      (create-instance 'r opal:rectangle
	(:box '(0 0 0 0))
	(:left (o-formula (first (gvl :box))))
	(:top (o-formula (second (gvl :box)))))
      (create-instance 's opal:text (:string "tester") (:left 50) (:top 30))
      (create-instance 's2 opal:text (:string "a longer string")
		       (:visible nil))
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
    (setq *current-panel-set* (create-panel-set startup-objects 640 500 250))
    (setq c32win (g-value *current-panel-set* :outer-window))
    (opal:update c32win)
    (push c32win *all-windows*))
  (when start-event-loop-p
    (inter:main-event-loop)))
