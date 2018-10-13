;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;
;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you are using this code or any part of Garnet,
;;; please contact garnet@cs.cmu.edu to be put on the mailing list.
;;;
;;;
;;;  Motif Menubar
;;;
;;;
;;;  Customizable slots:
;;;    1)  Left, top
;;;    3)  Title-font, item-font, accel-font
;;;    5)  Selection-function - a function to be executed when any item is
;;;           selected.  Takes the parameters (gadget menu-item submenu-item).
;;;    6)  Items - A list with the format
;;;           '(("m1" m1func ("m1,1"..."m1,N"))
;;;             ("m2" m2func (("m2,1" m2,1func)...("m2,N" m2,Nfunc)))
;;;             ...)
;;;           Where "mN" is a string or atom that is the title of a menu,
;;;        "mX,Y" is a string or atom in menu X, row Y,
;;;        mNfunc is executed when any item in menu N is selected,
;;;        mX,Yfunc is executed when item mX,Y is selected.
;;;           These two functions take the same parameters as the
;;;        selection-function.
;;;    7)  Accelerators - A list of accelerators, with the format
;;;             '((("s11" key11) .. ("s1N" key1N))
;;;               (("s21" key21) .. ("s2N" key2N))
;;;               ...)
;;;          where sXY is the accelarator string for the Yth submenu-item
;;;          in the Xth bar-item of the menubar; keyXY is the keyboard
;;;          event that starts the accelerator for the Yth submenu-item in
;;;          the Xth bar-item.
;;;    8)  Bar-Above-These-Items - A list of item-objs, with the format
;;;            '((ob11 ... ob1N)
;;;              (ob21 ... ob2N)
;;;              ...)
;;;           where obXY is the object in the Yth submenu-item of the Xth
;;;           bar-item above which a line should appear.
;;;    9)  Min-Menubar-Width - How big the menubar should be.  If it is too
;;;           small, or 0, this slot is ignored.
;;;
;;;  Programming interface (Garnet way):
;;;    1) Create an instance of motif-menubar with a value for the :items slot
;;;    2) Use opal:add-component to put the instance in a window
;;;
;;;  Caveats:
;;;
;;;  New motif-bar-items should be created with the :enable slot set
;;;  to NIL in order to keep their windows from being updated before
;;;  they are added to a menubar.
;;;
;;;  Pull Down Menus Demo:
;;;
;;;  The function below creates a window containing a pull down menu
;;;  and some text objects, as well as accelerators and bars.
;;;  Choosing an item from the pull-down menu will change the font of
;;;  the text objects.
;;;
;;;  To run it, enter (GARNET-GADGETS:motif-menubar-go).
;;;  To stop, enter (GARNET-GADGETS:motif-menubar-stop).
;;;
;;;  Designed and Implemented by Rajan Parthasarathy
;;;  Based heavily upon the GARNET menubar by Andrew Mickish & Pavan Reddy


(in-package :garnet-gadgets)


;;; First function is to display the submenu from gilt, etc.

;; Returns the new window if and only if a different bar-item is to be
;; displayed.  gadget parameter is the top level menubar.
(defun motif-menubar-popup-item (gadget)
  (let (bar-item)
    ;; first, find the motif-bar-item that is under the mouse
    (setq bar-item (opal:point-to-component (g-value gadget :menubar-items)
				      (inter:event-x inter:*current-event*)
				      (inter:event-y inter:*current-event*)))
    (when bar-item
      (dospecialpopupmenubar gadget bar-item))))

;; Returns a window that an instance of motif-submenu can be put into
(defun make-motif-submenu-win (a-bar-item a-submenu)
  (let ((win
	 (create-instance nil inter:interactor-window
	   (:bar-item a-bar-item)
	   (:aggregate a-submenu)
	   (:background-color (o-formula (gvl :bar-item :parent
					      :parent :foreground-color)))
	   (:border-width 0)
	   (:win-submenu a-submenu)
	   (:omit-title-bar-p t)
	   (:save-under t)
	   (:double-buffered-p t)
	   (:visible nil)
	   (:modal-p (o-formula (gv-local :self :bar-item :window :modal-p)))
	   (:height (o-formula
		     (gvl :win-submenu :height)))
	   (:width (o-formula
		    (gvl :win-submenu :width)))
	   (:left (o-formula
		   (let* ((bar-item (gvl :bar-item))
			  (win (gv bar-item :window))
			  (left-offset 0))
		     (cond
		       (win
			(do ((parent (gv win :parent) (gv parent :parent)))
			    ((null parent) left-offset)
			  (setf left-offset
				(+ left-offset (gv parent :left)
				   (or (gv parent :left-border-width) 0))))
			(+ (gv bar-item :left) (gv win :left)
			   (gv win :left-border-width) left-offset -5))
		       (t 0)))))
	   (:top (o-formula
		  (let* ((bar-item (gvl :bar-item))
			 (win (gv bar-item :window))
			 (top-offset 0))
		    (cond
		      (win
		       (do ((parent (gv win :parent) (gv parent :parent)))
			   ((null parent) top-offset)
			 (setf top-offset
			       (+ top-offset (gv parent :top)
				  (or (gv parent :top-border-width) 0))))
		       (+ (gv bar-item :top) (gv bar-item :height)
			  (gv win :top) (gv win :top-border-width) top-offset))
		      (t 0))))))))
    (s-value a-submenu :window win)
    win))


;; This object is a text field in the menu bar. An aggrelist of these
;; items makes up the menu bar.
(create-instance 'motif-bar-item motif-gadget-prototype
  (:width (o-formula (gvl :text :width)))
  (:height (o-formula (gvl :text :height)))
					; the mnemonic ":desc" is a description of a submenu.  the top-level :items
					; slot is a list of desc's.
  (:desc (o-formula (nth (gvl :rank) (gvl :parent :parent :items))))
  (:accelerators (o-formula
		  (nth (gvl :rank) (gvl :parent :parent :accelerators))))
  (:bar-above-these-items
   (o-formula (nth (gvl :rank) (gvl :parent :parent :bar-above-these-items))))
  (:menu-obj (o-formula (first (gvl :desc))))
  (:action (o-formula (second (gvl :desc))))
  (:items (o-formula (third (gvl :desc))))
  (:string (o-formula (let ((menu-obj (gvl :menu-obj)))
			(if (stringp menu-obj)
			    menu-obj
			    (string-capitalize
			     (string-trim ":" menu-obj))))))
  (:font (o-formula (gvl :parent :parent :title-font)))
  (:spacing 5)
  (:enabled (o-formula (when (gvl :parent :parent :active-p)
			 (if (= 4 (length (gvl :desc)))
			     (fourth (gvl :desc))
			     t))))
  (:disabled-line-style (o-formula (gvl :parent :parent :disabled-line-style)))
  (:foreground-color (o-formula (gvl :parent :parent :foreground-color)))
  ;; slot :submenu filled by :fix-update-slots with a submenu
  ;; slot :submenu-window set with the window of the submenu
  (:parts
   `((:text ,opal:text
	    (:left ,(o-formula (gvl :parent :left)))
	    (:top ,(o-formula (gvl :parent :top)))
	    (:font ,(o-formula (gvl :parent :font)))
	    (:string ,(o-formula (string (gvl :parent :string))))
	    (:line-style ,(o-formula
			   (if (gvl :parent :enabled)
			       opal:default-line-style
			       (gvl :parent :disabled-line-style))))))))

;; This is the top level motif-menubar gadget -- the big mutha.
;; basically, it contains 3 parts -> a motif-box that represents
;; the menubar, an aggrelist of bar-items, and a feedback object.
;; it also has two interactors -- menubar-select: used for selecting
;; stuff from the menubar, and menubar-accel, which does the
;; accelerator thang.
(create-instance 'motif-menubar motif-gadget-prototype
  :declare ((:parameters :left :top :items :title-font :item-font :accel-font
			 :foreground-color :min-menubar-width :accelerators
			 :bar-above-these-items :accelerator-windows
			 :active-p :selection-function :help-style)
	    (:type ((satisfies check-menubar-items) :items)
		   (list :accelerators :bar-above-these-items)
		   ((or list (satisfies schema-p)) :accelerator-windows)
		   (integer :min-menubar-width)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :title-font :item-font :accel-font)
		   ((is-a-p opal:color) :foreground-color)
		   (kr-boolean :active-p)
		   (kr-boolean :help-style)
		   ((or null function symbol) :selection-function)))
					; customizable slots
  (:left 0)(:top 0)
  (:left-offset 0)
  (:items nil)
  (:title-font (opal:get-standard-font nil :bold nil))
  (:item-font (opal:get-standard-font nil :bold nil))
  (:accel-font (opal:get-standard-font nil :bold nil))
  (:min-menubar-width 0)
  (:accelerators nil)
  (:bar-above-these-items nil)
  (:accelerator-windows (o-formula (gvl :window)))
  (:active-p t)
  ;; rga --- forces final item to be right justified.
  (:help-style nil)  ;; t or nil
  ;; Internal slots
  (:disabled-line-style (o-formula
			 (create-instance nil opal:line-style
			   (:stipple opal::gray-fill-bitmap)
			   (:background-color (gvl :foreground-color)))))
  (:enabled-items
   (o-formula (when (gvl :active-p)
		(let ((enabled-items nil))
		  (dovalues (component (gvl :menubar-items) :components
				       :in-formula t
				       :result enabled-items)
			    (when (gv component :enabled)
			      (push component enabled-items)))))))
  (:list-of-all-enabled-objects
   (o-formula (let ((l (copy-list (gvl :enabled-items))))
		(dolist (baritems (gvl :enabled-items))
		  (setf l (append l (copy-list
				     (gv-local baritems :submenu :enabled-items)))))
		l)))
  (:running-where-list (o-formula (append (gvl :list-of-all-enabled-objects)
					  (gvl :submenu-window-list))))
  (:right (o-formula (+ (gvl :left) (gvl :width))))
  (:destroy-me 'menubar-destroy)
  (:special-popup-func 'motif-menubar-popup-item)
  (:special-popdown-func 'put-down-menubar-popups)
  (:update-slots (cons :visible (g-value opal:aggrelist :update-slots)))
  (:old-visible t)
  ;; slot :submenu-window-list set with list of all windows being used.
  (:parts
   ;; this is the box that appears behind the text and
   ;; represents the menubar
   `((:menubar-box ,motif-box
		   (:left ,(o-formula (gvl :parent :left)))
		   (:top ,(o-formula (gvl :parent :top)))
		   (:height ,(o-formula (+ (gvl :parent :menubar-items :height) 14)))
		   (:width ,(o-formula (max (+ (gvl :parent :menubar-items :width) 18)
					    (gvl :parent :min-menubar-width)))))
     ;; these are the menubar items themselves
     (:menubar-items ,opal:aggrelist
		     (:left ,(o-formula (+ (gvl :parent :left)
					   (gvl :parent :left-offset) 9)))
		     (:top ,(o-formula (+ (gvl :parent :top) 7)))
		     (:items ,(o-formula (gvl :parent :items)))
		     ;; rga -- transmit help style info.
		     (:right-justify-last ,(o-formula (gvl :parent :help-style)))
		     (:justify-width ,(o-formula (- (gvl :parent :width) 18)))
		     (:h-spacing 8)
		     (:direction :horizontal)
		     (:item-prototype ,motif-bar-item)
		     (:selection-function nil)
		     (:h-align :center))
     ;; feedback-obj-topline and bottom-line represent the
     ;; feedback object that appears on the menubar.
     (:feedback-obj-topline ,opal:polyline
			    (:point-list ,(o-formula
					   (if (gvl :prev-baritem)
					       (let* ((ob (gvl :prev-baritem))
						      (left (gv ob :left))
						      (top (gv ob :top))
						      (ht (gv ob :height))
						      (wt (gv ob :width)))
						 (list (- left 4) (+ top ht 3)
						       (- left 4) (- top 2)
						       (+ left wt 4) (- top 2))))))
			    (:prev-baritem ,(o-formula
					     (gvl :parent :menubar-select :prev-baritem)))
			    (:visible ,(o-formula
					(and (gvl :prev-baritem)
					     (gvl :prev-baritem :enabled))))
			    (:line-style ,(o-formula (gv (kr-path 0 :parent) :highlight-line-style)))
			    (:fast-redraw-p :redraw)
			    (:fast-redraw-line-style ,(o-formula (gv (kr-path 0 :parent)
								     :foreground-line-style))))
     (:feedback-obj-bottomline ,opal:polyline
			       (:point-list ,(o-formula
					      (if (gvl :prev-baritem)
						  (let* ((ob (gvl :prev-baritem))
							 (left (gv ob :left))
							 (top (gv ob :top))
							 (ht (gv ob :height))
							 (wt (gv ob :width)))
						    (list (- left 5) (+ top ht 2)
							  (+ left wt 3) (+ top ht 2)
							  (+ left wt 3) (- top 1))))))
			       (:prev-baritem ,(o-formula
						(gvl :parent :feedback-obj-topline :prev-baritem)))
			       (:visible ,(o-formula (gvl :parent :feedback-obj-topline :visible)))
			       (:line-style ,(o-formula (gv (kr-path 0 :parent) :shadow-line-style)))
			       (:fast-redraw-p :redraw)
			       (:fast-redraw-line-style ,(o-formula (gv (kr-path 0 :parent)
									:foreground-line-style))))
     ))
  (:interactors
   ;; the accelerator interactor.  its menus are set to be all the
   ;; menus in the menubar.
   `((:menubar-accel ,motif-menu-accelerator-inter
		     (:active ,(o-formula (let ((o (gv-local :self :operates-on)))
					    (and (gv o :active-p)
						 (gv o :window)))))
		     (:window ,(o-formula (gvl :operates-on :accelerator-windows)))
		     (:menus ,(o-formula
			       (let ((l nil))
				 (dolist (a-baritem (gvl :operates-on :menubar-items :components))
				   (push (gv a-baritem :submenu) l))
				 l))))
     ;; This is for clicking on a bar item, moving out of the
     ;; submenu's window, etc. etc.
     (:menubar-select ,inter:menu-interactor
		      ;;(:stop-event '(:any-leftdown :rightdown))
		      (:active ,(o-formula (gv-local :self :operates-on :window)))
		      (:window ,(o-formula
				 (let ((main-win (gv-local :self :operates-on :window))
				       (submenu-wins (gv-local :self :operates-on
							       :submenu-window-list)))
				   (if main-win
				       (if submenu-wins
					   (cons main-win (copy-list submenu-wins))
					   main-win)))
				 ))
		      (:start-where ,(o-formula (list :list-element-of
						      (gvl :operates-on)
						      :list-of-all-enabled-objects)))
		      (:running-where ,(o-formula (list :list-element-of
							(gvl :operates-on)
							:running-where-list)))
		      (:feedback-obj ,(o-formula (gvl :operates-on :feedback-obj-topline)))
		      (:outside nil)
		      (:start-action
		       ,#'(lambda (inter obj)
			    (call-prototype-method inter obj)
			    (s-value (g-value inter :operates-on) :*bar-item-popped-up nil)))
		      (:outside-action
		       ,#'(lambda (inter outside prev)
			    (call-prototype-method inter outside prev)
			    ;; make sure the top level bar items stay selected since
			    ;; sub-menus are showing
			    (when (is-a-p prev motif-bar-item)
			      (s-value prev :interim-selected t))))
		      (:running-action
		       ,#'(lambda (inter prev new)
			    (let* ((prev-baritem (g-value inter :prev-baritem))
				   (new-is-bar (is-a-p new motif-bar-item))
				   (new-baritem (when new
						  (if new-is-bar
						      new
						      ;; else is a sub-item, get its bar-item
						      (g-local-value new :bar-item)))))
			      (call-prototype-method inter prev new)
			      ;; now, when you get into a submenu window, you want to start
			      ;; that submenu's interactor.
			      (let ((curr-win (inter:event-window inter::*current-event*))
				    (obj-wins (g-local-value inter :operates-on :submenu-window-list)))
				(if  (member curr-win obj-wins :test #'equal)
				     (let* ((submenu (g-local-value prev-baritem :submenu))
					    (first-obj-over
					     (or (g-value submenu :press :remembered-last-object)
						 (first (g-value submenu :menu-item-list :components))))
					    (ce inter::*current-event*)
					    (ev-x (inter:event-x ce))
					    (ev-y (inter:event-y ce))
					    (ev (inter:make-event :window curr-win
								  :code (inter:event-code ce)
								  :char :leftdown
								  :mousep t :downp t
								  :x ev-x
								  :y ev-y
								  :timestamp (inter:event-timestamp ce))))
				       (when (eq (g-local-value prev-baritem :submenu :press :current-state)
						 :start)
					 (inter:start-interactor (g-value submenu :press) ev))
				       ;; for some weird reason, the first item that's been selected
				       ;; in the newly popped up window don't have a feedback object
				       ;; around them.  so here, we explicitly tell it to put a
				       ;; feedback object around the first item it selects, unless
				       ;; that item is disabled.
				       (when (and
					      (opal:point-in-gob first-obj-over ev-x ev-y)
					      (g-value first-obj-over :enabled))
					 (s-value submenu :value (g-value first-obj-over :item-obj))
					 (s-value submenu :value-obj first-obj-over)
					 (s-value first-obj-over :interim-selected t)))
				     (unless (null prev-baritem)
				       (s-value (g-local-value prev-baritem :submenu) :value-obj nil)))
				)
			      ;; keep the interim selected of the bar item so it
			      ;; shows up as highlighted when items in sub-menu selected.
			      (if new-baritem
				  ;; this makes the subwindow not go away when move
				  ;; outside subwindow
				  (unless (eq new-baritem prev-baritem)
				    (s-value inter :prev-baritem new-baritem)
				    (if prev-baritem
					(let ((win (g-local-value prev-baritem :submenu-window))
					      (prev-selected (g-local-value prev-baritem :submenu :press
									    :remembered-last-object)))
					  (s-value prev-baritem :interim-selected nil)
					  (when prev-selected
					    (s-value prev-selected :interim-selected nil))
					  (s-value win :visible nil)
					  (opal:update win)))
				    (if new-baritem
					(let ((win (g-value new-baritem :submenu-window)))
					  (s-value (g-value new-baritem :submenu)
						   :value-obj nil)
					  (s-value win :visible t)
					  (opal:raise-window win)))))  ;; call update
	       ;;; this needs to be after the call-prototype-method,
	       ;;; since that is where the :interim-selected will be
	       ;;; turned off.
			      (unless new-is-bar
				(when new-baritem
				  (s-value new-baritem :interim-selected t))))
			    ))
		      (:abort-action
		       ,#'(lambda (inter last)
			    (declare (ignore last))
			    (let ((prev-baritem (g-value inter :prev-baritem)))
			      (when prev-baritem
				(let ((prev-selected (g-value prev-baritem :submenu :press
							      :remembered-last-object)))
				  (s-value prev-baritem :interim-selected nil)
				  (when prev-selected
				    (s-value prev-selected :interim-selected nil)))
				(let ((win (g-value prev-baritem :submenu-window)))
				  (s-value inter :prev-baritem nil)
				  (s-value win :visible nil)
				  (opal:update win))))))
		      ;; This hides the submenu's window after an item has been selected
		      (:final-function
		       ,#'(lambda (inter last)
			    (declare (ignore last))
			    (when (g-value inter :prev-baritem)
			      (let ((win (g-value inter :prev-baritem :submenu :window)))
				(s-value win :visible nil)
				(opal:update win))
			      (s-value inter :prev-baritem nil))))))))

(s-value motif-menubar :do-not-dump-slots
	 (append '(:submenu-window-list)
		 (g-value motif-menubar :do-not-dump-slots)))

(s-value motif-bar-item :do-not-dump-slots
	 (append '(:submenu :submenu-window)
		 (g-value motif-bar-item :do-not-dump-slots)))

(let ((menubar-select (g-value motif-menubar :menubar-select)))
  (s-value menubar-select :do-not-dump-slots
	   (append '(:prev-baritem)
		   (g-value menubar-select :do-not-dump-slots))))



;; This function gets called when an item has been selected from the
;; submenu. Basically, it hides the submenu's window and calls the
;; appropriate :selection-function and the item's function
(defun submenu-selection-function (submenu submenu-item)
  (let* ((baritem (g-value submenu :bar-item))
	 (bar-action (g-value baritem :action))
	 (bar-obj (g-value baritem :menu-obj))
	 (a-menubar (g-value baritem :parent :parent)))
    (s-value (g-value submenu :window) :visible nil)
    (s-value (g-value a-menubar :menubar-select) :prev-baritem nil)
    (opal:update (g-value submenu :window))
    (if bar-action
	(funcall bar-action a-menubar bar-obj
		 (g-value submenu-item :item-obj)))
    (if (schema-p a-menubar)
	(kr-send a-menubar :selection-function
		 a-menubar bar-obj (g-value submenu-item :item-obj)))))

;; this is a submenu for the motif-menubar (surprise surprise!)
;; its a motif-menu that inherits some slots from the top level
;; menubar.  also, the accelerators for the menubar are given
;; as lists of two items - the string and the key.  but!  the
;; motif-menu wants its accelerators to be lists of three items -
;; the underline, the string, and the key.  so, we have to insert
;; nils for the underlines.  that's what the o-formula in the
;; :accelerators slot does.
;;
;; a submenu-item is just an instance of motif-menu-item.  which
;; means any motif-menu-item can be added to the menubar.

(create-instance 'motif-submenu motif-menu
  (:bar-item nil)
  (:menubar (o-formula (gvl :bar-item :parent :parent)))
  (:foreground-color
   (o-formula (gvl :menubar :foreground-color)))
  (:final-feedback-p nil)
  (:item-font (o-formula (gvl :menubar :item-font)))
  (:accel-font (o-formula (gvl :menubar :accel-font)))
  (:items (o-formula (gvl :bar-item :items)))
  (:keyboard-selection-p nil)
  (:selection-function #'submenu-selection-function)
  (:accelerators (o-formula
		  (let ((bar-acc (gvl :bar-item :accelerators))
			menu-acc temp)
		    (when bar-acc
		      (dolist (a-list bar-acc)
			(setf temp (push nil a-list))
			(setf menu-acc
			      (append menu-acc (list temp)))))
		    menu-acc)))
  (:bar-above-these-items (o-formula (gvl :bar-item :bar-above-these-items)))
  (:text-offset (o-formula (if (gvl :accelerators) 4 0)))
  (:parts
   `(:frame
     (:menu-item-list :modify
      (:item-prototype
       (:modify
	(:enabled t)
	(:active-p ,(o-formula (gvl :enabled)))
	(:get-title-fn menubar-get-title-motif)
	(:set-title-fn menubar-set-title-motif)
	)))
     :bar-list :feedback-obj-topline :feedback-obj-bottomline :sel-box)))

;;; auxiliary function for motif-menubar's :fix-update-slots method.
;;; this function is used to establish the links between a bar-item and its
;;; submenu.  a new submenu is created and put in a new window.  this new
;;; window is added to the :submenu-window-list slot of the interactor in the
;;; top-level menubar.

(defun attach-motif-submenu (menubar-items a-bar-item)
  ; a-bar-item might have been taken from storage, so check to see whether
  ; it has a :submenu and :submenu-window before creating these
  (let* ((new-submenu (or (g-local-value a-bar-item :submenu)
			  (create-instance nil motif-submenu
			    (:bar-item a-bar-item))))
	 (win (or (g-local-value a-bar-item :submenu-window)
		  (make-motif-submenu-win a-bar-item new-submenu)))
	 (a-menubar (g-value menubar-items :parent))
	 (submenu-window-list (g-local-value a-menubar :submenu-window-list)))

    ; if a-bar-item was taken from storage, then its :submenu-window is
    ; already on the :submenu-window-list
    (unless (member win submenu-window-list)
      (s-value a-menubar :submenu-window-list
	       (cons win (copy-list submenu-window-list))))

    ; bookkeeping in case a-bar-item was not taken from storage
    (s-value a-bar-item :submenu new-submenu)
    (s-value a-bar-item :submenu-window win))
  a-bar-item)


;;;    this should be called when the number of items in the menu is changed
;;; manually (without calling add-item or remove-item).  this is basically an
;;; enhanced version of the default fix-update-slots method which reuses old
;;; components.
;;;    when the menubar instance is created, aggrelists creates components
;;; for it, but this function has to be called to create submenus for the
;;; components.  if the number of :items changes, then this function should be
;;; called to both create (or destroy) new components for the aggrelist and
;;; create (or destroy) corresponding submenus.
;;;
(define-method :fix-update-slots motif-menubar (a-menubar)
  ;; will be called when :items or :visible change
  (let ((old-visible (g-value a-menubar :old-visible))
	(new-visible (g-value a-menubar :visible)))
    (if (eq old-visible new-visible)
	;; then items must have changed -- generate new bar-items
	(call-prototype-method a-menubar)
	(progn ;; else visible changed
	  (s-value a-menubar :old-visible new-visible)
	  (unless new-visible
	    (put-down-menubar-popups a-menubar))))))

(define-method :fix-update-slots motif-bar-item (a-bar-item)
  (let ((a-menubar (g-value a-bar-item :parent)))
    (unless (member (g-local-value a-bar-item :submenu-window)
		    (g-local-value a-menubar :submenu-window-list))
      ;; generate new submenu window for the bar-item
      (attach-motif-submenu a-menubar a-bar-item))))


;;;
;;;  demo functions
;;;


#+garnet-test (defparameter *motif-font-to-swap* (create-instance nil opal:font))
#+garnet-test (defvar motif-family-text nil)
#+garnet-test (defvar motif-face-text nil)
#+garnet-test (defvar motif-size-text nil)
#+garnet-test (defvar motif-combo-text nil)


;;; when we want to change an object's font, set the slots of *motif-font-to-swap*,
;;; then set the object to have that font.  (opal does not notice when you
;;; just change the slots of a font.)
;;;
#+garnet-test
(defun motif-change-font (text-obj &key family face size)
  (let ((old-font (g-value text-obj :font))
	(new-font *motif-font-to-swap*))
    (setf *motif-font-to-swap* old-font)
    (if family
	(s-value new-font :family family)
	(s-value new-font :family :fixed))
    (if face
	(s-value new-font :face face)
	(s-value new-font :face :roman))
    (if size
	(s-value new-font :size size)
	(s-value new-font :size :medium))
    ;; the :max-char-as/descent formulas do not depend on the slots just
    ;; modified, so you have to explicitly recompute them when reusing a font
    (recompute-formula new-font :max-char-ascent)
    (recompute-formula new-font :max-char-descent)
    (s-value text-obj :font new-font)))


;;; some functions to call when items are selected
;;;
#+garnet-test
(defun motif-family-fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-family-text slot value)
  (s-value motif-family-text :string (string-downcase value)))
#+garnet-test
(defun motif-face-fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-face-text slot value)
  (s-value motif-face-text :string (string-downcase value)))
#+garnet-test
(defun motif-size-fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-size-text slot value)
  (s-value motif-size-text :string (string-downcase value)))

#+garnet-test
(defun motif-fixed-fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "setting :family slot to :fixed.~%"))
#+garnet-test
(defun motif-serif-fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "setting :family slot to :serif.~%"))
#+garnet-test
(defun motif-sans-serif-fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "setting :family slot to :sans-serif.~%"))


#+garnet-test
(defun motif-menubar-go (&key dont-enter-main-event-loop)

  (create-instance 'motif-menubar-win inter:interactor-window
    (:background-color opal:motif-green)
    #-apple (:left 700)  #+apple (:left 300)
    (:top 45)(:height 360)(:width 600)

    (:aggregate (create-instance 'motif-menubar-top-agg opal:aggregate)))
  (opal:update motif-menubar-win)

  (create-instance 'demo-motif-menubar motif-menubar
    (:foreground-color opal:motif-green)
    (:help-style t)
    (:width (kr:o-formula (kr:gvl :window :width) 600))
    (:items
     '((:family motif-family-fn
	((:fixed motif-fixed-fn)
	 (:serif motif-serif-fn)
	 (:sans-serif motif-sans-serif-fn)))
       (:face motif-face-fn
	((:roman)(:bold)(:italic)(:bold-italic)))
       (:size motif-size-fn
	((:small)(:medium)(:large)(:very-large)))
       (:help nil
	((:help) (:ballons)(:news)(:index)))
       ))
    (:accelerators
     '((("!f" :|meta-f|) ("!e" :|meta-e|) ("!a" :|meta-a|))
       (("!r" :|meta-r|) ("!b" :|meta-b|) ("!i" :|meta-i|) ("!b" :meta-b))
       (("!s" :|meta-s|) ("!m" :|meta-m|) ("!l" :|meta-l|) ("!v" :|meta-v|))))
    (:bar-above-these-items
     '(nil
       nil
       (:large)
       (:news :index)))
    (:selection-function
     #'(lambda (gadget slot value)
	 (declare (ignore gadget slot value))
	 (let ((family (g-value motif-family-text :font :family))
	       (face (g-value motif-face-text :font :face))
	       (size (g-value motif-size-text :font :size)))
	   (motif-change-font motif-combo-text
			:family family :face face :size size)))))

  (create-instance 'motif-menubar-special-popup-inter inter:button-interactor
    (:start-event #\p)
    (:start-where (list :in demo-motif-menubar))
    (:continuous nil)
    (:window motif-menubar-win)
    (:final-function #'(lambda (inter obj)
			 (declare (ignore inter obj))
			 (kr-send demo-motif-menubar :special-popup-func
				  demo-motif-menubar))))

  (opal:add-component motif-menubar-top-agg demo-motif-menubar)
  (opal:update motif-menubar-win)

  (create-instance 'motif-family-text opal:text
     (:left 10)
     (:top 200)
     (:string "fixed")
     (:font (create-instance nil opal:font)))
  (create-instance 'motif-face-text opal:text
     (:left 75)
     (:top 200)
     (:string "roman")
     (:font (create-instance nil opal:font)))
  (create-instance 'motif-size-text opal:text
     (:left 160)
     (:top 200)
     (:string "medium")
     (:font (create-instance nil opal:font)))
  (create-instance 'motif-combo-text opal:text
     (:left 75)
     (:top 230)
     (:string "combo")
     (:font (create-instance nil opal:font)))

  (opal:add-components motif-menubar-top-agg
		       motif-family-text motif-face-text motif-size-text motif-combo-text)

  (opal:update motif-menubar-win)
 (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
 )



;;;
;;;  motif-menubar-stop
;;;

#+garnet-test
(defun motif-menubar-stop ()
  (opal:destroy motif-menubar-win))


;;;
;;;  utility functions used by exported functions
;;;

(defun confirm-motif-menubar (a-menubar)
  (unless (is-a-p a-menubar motif-menubar)
    (error "~s is not an instance of ~s.~%" a-menubar motif-menubar))
  t)

(defun confirm-motif-bar-item (a-bar-item)
  (unless (is-a-p a-bar-item motif-bar-item)
    (error "~s is not an instance of ~s.~%" a-bar-item motif-bar-item))
  t)


(defun confirm-motif-bar-or-submenu-item (menubar-component)
  (unless (or (is-a-p menubar-component motif-bar-item)
	      (is-a-p menubar-component motif-menu-item))
    (error "~s is not an instance of ~s or ~s.~%" menubar-component
	   motif-bar-item motif-menu-item))
  t)

(defun insert-item-at-n (a-list item n)
  (let ((new-list (copy-list a-list))
	(len (1- (length a-list))))

    (when (< len n)
      (dotimes (i (- n len 1))
	(setf new-list
	      (append new-list (list nil)))))

    (append (subseq new-list 0 n) (list item)
	  (subseq new-list n))))

(defun remove-item-at-n (a-list n)
  (if (< n (length a-list))
      (append (subseq a-list 0 n) (subseq a-list (1+ n)))
      a-list))

;;;
;;;  exported functions
;;;

; menubar functions


;
; the parameter item may be either
; 1) an instance of motif-bar-item, or
; 2) a sublist of an :items list
;
; note note note note:  the :accelerator keyword *must* appear before the
; :wheres if you want to add any accelerators
;
; locator should be a sublist of an :items list or an installed bar-item.
; or, you can use the key feature of add-item and make locator the title of
; an installed bar-item:
;    (add-item demo-motif-menubar new-bar :after "bar2" :key #'car)
;
; implementation note:  the reason that we do not just set the :items list
; and let the fix-update-slots method deal with the components is that the
; item parameter can be an actual component to be added, so you should not
; generate a new component via fix-update-slots.
;
(define-method :add-item motif-menubar (a-menubar item &rest args)
  (let* ((a-bar-item (if (is-a-p item motif-bar-item)
			 item
			 (or (let ((old-bi (pop (g-value a-menubar :storage))))
			       (when old-bi
				 (g-value old-bi :desc)  ; initialize formula
				 (s-value old-bi :desc item)
				 old-bi))
			     (make-motif-bar-item :desc item))))
	 (a-list (g-value a-bar-item :parent)) ; parent should be nil
	 where locator key accel)

    ;; with the motif-menubar, you can specify accelerators
    ;; to be added as well.

    (when (eq :accelerators (first args))
      ;; accel will be the user's new accelerator character
      (setf accel (second args))
      (setf args (cddr args)))

    (when a-list
      (error "~s is already installed in ~s.~%" a-bar-item a-list))

    (multiple-value-setq (where locator key) (opal::get-wheres args))

    ; add the bar-item as a component of the menubar
    (let* ((locator-comp (if (is-a-p locator motif-bar-item)
			    locator
			    (get-bar-component a-menubar locator)))
	   (locator-desc (when locator-comp
			   (g-value locator-comp :desc)))
	   (items (copy-list (g-value a-menubar :items)))
	   (desc (g-value a-bar-item :desc))
	   (menubar-items (g-value a-menubar :menubar-items))
	   rank)

      (opal:add-local-component menubar-items
				a-bar-item where locator-comp)
      (setf rank
	    (s-value a-bar-item :rank
		     (position a-bar-item (g-value menubar-items :components))))
      (s-value (g-value a-menubar :menubar-items) :old-items
         (s-value a-menubar
		  :items
		  (opal::insert-item desc items where locator-desc key)))

    ;; adjust the accelerators and bar-above-these-items lists to
    ;; recognize that a new bar-item has been added

    (s-value a-menubar :accelerators
	     (insert-item-at-n (g-value a-menubar :accelerators) accel rank))

    (s-value a-menubar :bar-above-these-items
	     (insert-item-at-n (g-value a-menubar :bar-above-these-items)
			       nil rank))
    )

    ; do additional bookkeeping that attaches the bar-item to the menubar
    (let* ((win (g-value a-bar-item :submenu-window))
	   (top-inter (g-value a-menubar :menubar-select))
	   (cur-wins (g-value top-inter :window)))
      ; make sure win is destroyed along with a-menubar
      (unless (member win (g-local-value a-menubar :submenu-window-list))
	(s-value a-menubar :submenu-window-list
	       (append (list win) (g-local-value a-menubar :submenu-window-list))))
      ; add win to the top-level interactor's :window slot
      (if (listp cur-wins)
	  (append (list win) (g-value top-inter :window))
	  (s-value top-inter :window (list win cur-wins)))
      (mark-as-changed top-inter :window))

    a-bar-item))


;
;
; the a-bar-item parameter can either be
; 1) an instance of motif-bar-item, or
; 2) a sublist of the :items list
;
(define-method :remove-item motif-menubar (a-menubar item)
  (let ((a-bar-item (if (is-a-p item motif-bar-item)
			item
			(get-bar-component a-menubar item)))
	(rank nil))
    (unless (and a-bar-item
		 (eq a-menubar (and
				(g-value a-bar-item :parent)  ;;check if it has a parent
				(g-value a-bar-item :parent :parent)))) ;;get the menubar
      (error "~s does not have ~s as its menubar.~%"
	     a-bar-item a-menubar))
    (setf rank (position a-bar-item (g-value a-menubar :menubar-items :components)))

    ; remove the appropriate accelerators
    (s-value a-menubar :accelerators
	     (remove-item-at-n (g-value a-menubar :accelerators) rank))
    ; remove the appropriate bars
    (s-value a-menubar :bar-above-these-items
	     (remove-item-at-n (g-value a-menubar :bar-above-these-items) rank))

    ; remove the bar-item from the menubar
    (s-value a-menubar :submenu-window-list
	     (remove (g-local-value a-bar-item :submenu-window)
		     (g-local-value a-menubar :submenu-window-list)))
    (opal:remove-local-component (g-value a-menubar :menubar-items) a-bar-item)
    (unless (eq a-bar-item item)
      (push a-bar-item (g-value a-menubar :storage)))

    ; change the top-level :items list
    (let ((old-desc (if (is-a-p a-bar-item motif-bar-item)
			(g-value a-bar-item :desc)
			a-bar-item)))
      (s-value (g-value a-menubar :menubar-items) :old-items
	 (s-value a-menubar :items (remove old-desc (g-value a-menubar :items)
					   :test #'equal))))))

(defun make-motif-menubar ()
  (create-instance nil motif-menubar))

(define-method :menubar-components motif-menubar (a-menubar)
  (g-value a-menubar :menubar-items :components))

(define-method :set-menubar motif-menubar (a-menubar new-menus)
  ; can't use dovalues and destructive operation on :components together
  (let ((components (copy-list (menubar-components a-menubar))))
    (dolist (old-comp components)
      (opal:remove-item (g-value a-menubar :menubar-items) old-comp))
    (dolist (new-menu new-menus)
      (opal:add-item (g-value a-menubar :menubar-items) new-menu))))



; bar-item functions
;
; the item parameter can either be
; 1) an instance of motif-menu-item, or
; 2) a description of a submenu-item: (list string action) where "string" is
;    a string or atom and "action" is a function
;
; note note note note:  the :accelerator keyword *must* appear before the
; :wheres if any accelerators are to be added

(define-method :add-item motif-bar-item (a-bar-item item &rest args)
  (let ((a-menubar
	 (and
	  (g-value a-bar-item :parent)
	  (g-value a-bar-item :parent :parent)))
	(submenu (g-value a-bar-item :submenu))
	(old-desc (or (g-local-value a-bar-item :desc)
		      (copy-list (g-value a-bar-item :desc))))
	where locator key accel rank)

    ;; when adding a submenu item to a bar item, you can also
    ;; provide it with an accelerator
    (when (eq :accelerator (first args))
      ;; accel will be the user's new accelerator character
      (setf accel (second args))
      (setf args (cddr args)))

    (multiple-value-setq (where locator key) (opal::get-wheres args))

    ;; item can be either a submenu-item or a list
    (if (schema-p item)
	(let* ((new-item `(,(g-value item :item-obj)
			   ,(g-value item :action)))
	       (new-sub-desc (opal::insert-item new-item (third old-desc)
						where locator key))
	       (new-desc (list (first old-desc) (second old-desc) new-sub-desc)))

	  (setf rank
		(s-value item :rank
			 (position new-item new-sub-desc :test #'equal)))
	  (if a-menubar
	      (s-value a-menubar :items
		       (substitute new-desc old-desc
				   (or (g-local-value a-menubar :items)
				       (copy-list (g-value a-menubar :items)))
				   :test #'equal))
	      (s-value a-bar-item :desc new-desc))
	  (s-value (g-value submenu :menu-item-list) :old-items new-sub-desc)

	  (kr:with-constants-disabled
	      (opal:add-local-component (g-value submenu :menu-item-list)
					item :at rank))
	  )

	(let* ((new-sub-desc (opal::insert-item item (third old-desc)
						where locator key))
	       (new-desc (list (first old-desc) (second old-desc)
			       new-sub-desc)))

	  (setf rank (position item new-sub-desc
			       :test #'(lambda (x y)
					 (equal x (funcall key y)))))
	  (if a-menubar
	      (s-value a-menubar
		       :items
		       (substitute new-desc old-desc
				   (or (g-local-value a-menubar :items)
				       (copy-list (g-value a-menubar :items)))
				   :test #'equal))
	      (s-value a-bar-item :desc new-desc))
	  (s-value (g-value submenu :menu-item-list) :items new-sub-desc)
	  ))

    ;; now you have to set the :accelerators list of the top-level menubar
    ;; to understand that a new item has been added to the submenu, and if
    ;; necessary, insert the new accelerator into the list.  you do this
    ;; only if a menubar exists

    (when a-menubar
      (let* ((a-list (g-value a-menubar :accelerators))
	     (old-acc-list (nth (g-value submenu :bar-item :rank) a-list)))
	(when old-acc-list
	  (let ((new-acc-list (insert-item-at-n old-acc-list accel rank)))
	    (s-value a-menubar :accelerators
		     (substitute new-acc-list old-acc-list a-list))
	  ))))
    ))


;
;
; the item parameter can be either
; 1) an instance of motif-menu-item, or
; 2) a string or atom
;
(define-method :remove-item motif-bar-item (a-bar-item &optional item
						 &key (key #'opal:no-func))
  (let* ((a-submenu-item (if (is-a-p item motif-menu-item)
			     (get-submenu-component a-bar-item
							  (g-value item :item-obj))
			     (get-submenu-component a-bar-item item)))
	 (submenu (g-value a-bar-item :submenu))
	 (submenu-components (g-value submenu :menu-item-list :components))
	 (rank (if item
		   (position a-submenu-item submenu-components
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   (1- (length submenu-components)))))

    (unless rank
      (error "~s does not have ~s as its bar-item.~%"
	     a-submenu-item a-bar-item))
    ; if the user did not supply an item, then just remove the last component
    (unless a-submenu-item
      (setf a-submenu-item (nth rank submenu-components)))

    (opal:remove-local-component (g-value a-submenu-item :parent) a-submenu-item)

    ; update the :items or :desc list
    (let* ((a-menubar (and
		       (g-value a-bar-item :parent)
		       (g-value a-bar-item :parent :parent)))
	   (old-desc (g-value a-bar-item :desc))
	   (old-sub-desc (third old-desc))
	   (item-obj (g-value a-submenu-item :item-obj))
	   (action (g-value a-submenu-item :action))
	   (new-sub-desc (remove (if action
				     (list item-obj action)
				     (list item-obj))
				 old-sub-desc :test #'equal))
	   (new-desc (substitute new-sub-desc old-sub-desc old-desc
				 :test #'equal)))

      (if a-menubar
	  (s-value (g-value submenu :menu-item-list) :old-items
	     (s-value a-menubar :items
		      (substitute new-desc old-desc (g-value a-menubar :items)
				  :test #'equal)))
	  (s-value a-bar-item :desc new-desc))
      (s-value (g-value submenu :menu-item-list) :old-items new-sub-desc)

      ;; now you have to remove the accelerator from the menubar
      (when a-menubar
	(let* ((accels (g-value a-menubar :accelerators))
	       (a-list (nth (g-value a-bar-item :rank) accels)))
	  (when a-list
	    (let ((new-a-list (remove-item-at-n a-list rank)))
	      (s-value a-menubar :accelerators
		       (substitute new-a-list a-list accels))))))
      )))

(s-value motif-menubar :change-item (g-value opal:aggrelist :change-item))
(s-value motif-menubar :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))
(s-value motif-bar-item :change-item #'menubar-change-item)
(s-value motif-bar-item :remove-nth-item (g-value opal:aggrelist :remove-nth-item))
(s-value motif-bar-item :set-submenu-fn #'set-submenu-fn)
(s-value motif-menubar :get-bar-component-fn #'get-bar-component-fn)


;; returns an instance of a motif-bar-item

(defun make-motif-bar-item (&key desc font title)
  ;; type-check the argument before creating an object for it.
  (when desc (check-bar-item desc))
  (let* ((new-bar-item (create-instance nil motif-bar-item))
	 (new-submenu (create-instance nil motif-submenu
			 (:bar-item new-bar-item)
			 (:font (or font opal:default-font))))
	 (win (make-motif-submenu-win new-bar-item new-submenu)))
    (s-value new-bar-item :submenu new-submenu)
    (s-value new-bar-item :submenu-window win)
    ; put in initial value of :desc slot
    (g-value new-bar-item :desc) ; to initialize the default formula
    (if desc
	(s-value new-bar-item :desc desc)
	(s-value new-bar-item :desc (list title nil nil)))
    new-bar-item))

;; returns the title of a menubar-component, which could
;; be a bar-item or a submenu-item

(defun menubar-get-title-motif (menubar-component)
  (if (is-a-p menubar-component motif-bar-item)
      (g-value menubar-component :menu-obj)
      (g-value menubar-component :item-obj)))

(s-value motif-bar-item :get-title-fn #'menubar-get-title-motif)

; if the menubar-component is installed in a menubar, then the :items list is
; changed.  otherwise, the object's :desc slot is set.
;
(defun menubar-set-title-motif (menubar-component string)
  (cond

    ;; the parameter is a motif-bar-item
    ((is-a-p menubar-component motif-bar-item)
     (let* ((a-bar-item menubar-component)
	    (a-menubar (and
			(g-value a-bar-item :parent)
			(g-value a-bar-item :parent :parent))))
       (cond
	 (a-menubar
	  ; a-bar-item is installed, so set the :items list
	  (rplaca (find (g-value a-bar-item :desc)
			(g-value a-menubar :items) :test #'equal)
		  string)
	  (mark-as-changed a-menubar :items))

	 (t
	  ; not installed, so just set local :desc list
	  (rplaca (g-value a-bar-item :desc) string)
	  (mark-as-changed a-bar-item :desc)))))

    ;; the parameter is a motif-menu-item
    ((or
      (is-a-p menubar-component motif-menu-item))
     (let* ((a-submenu-item menubar-component)
	    (a-submenu-agg (g-value menubar-component :parent :parent)))
       (cond
	 (a-submenu-agg
	  ; a-submenu-item is installed in a bar-item
	  (let* ((a-bar-item (g-value a-submenu-agg :bar-item))
		 (a-menubar (when a-bar-item
			      (g-value a-bar-item :parent :parent))))
	    (cond

	      ; a-submenu-item is installed in a menubar
	      (a-menubar
	       (let* ((old-desc (g-value a-bar-item :desc))
		      (old-items-desc
		       (find old-desc (g-value a-menubar :items))))
		 (dolist (desc (third old-items-desc))
		   (when (and (equal (first desc)
				     (g-value a-submenu-item :item-obj))
			      (equal (second desc)
				     (g-value a-submenu-item :action)))
		     (rplaca desc string)
		     (mark-as-changed a-menubar :items)))))

	      ; a-submenu-item is not installed in a menubar
	      (t
	       (let* ((old-desc (g-value a-bar-item :desc)))
		 (dolist (desc (third old-desc))
		   (when (and (equal (first desc)
				     (g-value a-submenu-item :item-obj))
			      (equal (second desc)
				     (g-value a-submenu-item :action)))
		     (rplaca desc string)
		     (mark-as-changed a-bar-item :items))))))))

	 ; a-submenu-item is not installed in a bar-item
	 (t
	  (g-value a-submenu-item :desc)
	  (s-value a-submenu-item :desc (list string))))))

    ; else, print error message
    (t (error "~s is not an instance of ~s or ~s.~%" menubar-component
	   motif-bar-item motif-menu-item)))
  string)

(s-value motif-bar-item :set-title-fn #'menubar-set-title-motif)

(define-method :submenu-components motif-bar-item (a-bar-item)
  (g-value a-bar-item :submenu :menu-item-list :components))

;
; the args parameter will (optionally) include the where, locator, and key
; parameters that are sent to add-item.  the where refers to the placement
; of the new submenu item among the current submenu items within b-item.
;
(define-method :add-submenu-item motif-menubar (a-menubar b-item s-item &rest args)
  (let* ((a-bar-item (if (is-a-p b-item motif-bar-item)
			 b-item
			 (get-bar-component a-menubar b-item)))
	 where locator key accel)
    (when (eq :accelerator (first args))
      ;; accel will be the user's new accelerator character
      (setf accel (second args))
      (setf args (cddr args)))

    (multiple-value-setq (where locator key) (opal::get-wheres args))
    (opal:add-item a-bar-item s-item :accelerator accel where locator key)))

;
; after looking up the b-item to get a bar-item object, call opal:remove-item.
;
(define-method :remove-submenu-item motif-menubar (a-menubar b-item s-item)
  (let* ((a-bar-item (if (is-a-p b-item motif-bar-item)
			 b-item
			 (get-bar-component a-menubar b-item))))
    (opal:remove-item a-bar-item s-item)))

; new-desc can have two forms:
;   1) a list of motif-menu-item instances, or
;   2) a list of motif-menu-item descriptions, such as
;      ((item1 action1) (item2) (item3 action3)))
;

;; gets the submenu-component corresponding to a given title in a
;; bar-item

(define-method :get-submenu-component motif-bar-item (a-bar-item item)
  (find-if #'(lambda (a-submenu-item)
	       (if (listp item)
		   (equal item (list (g-value a-submenu-item :item-obj)))
		   (equal item (g-value a-submenu-item :item-obj))
		   ))
	   (submenu-components a-bar-item)))

;; Finds a submenu component, given a menubar, the title of the
;; bar-item, and the title of the submenu-item
(define-method :find-submenu-component motif-menubar (a-menubar submenu-title submenu-item)
  (let ((a-bar-item (if (is-a-p submenu-title motif-bar-item)
			submenu-title
			(get-bar-component a-menubar submenu-title))))
    (get-submenu-component a-bar-item submenu-item)))


;; submenu-item functions
;;
;; this function returns an instance of motif-menu-item.  if the :desc key is
;; supplied, then the accompanying parameter should be the string/function or
;; atom/function pair that describes a submenu-item.
(defun make-motif-submenu-item (&key desc (enabled t))
  (let ((new-submenu-item (create-instance nil motif-menu-item
			    (:desc nil)
			    (:item-obj (o-formula (first (gvl :desc))))
			    (:action (o-formula (second (gvl :desc))))
			    (:enabled t)
			    (:active-p (o-formula (gvl :enabled)))
			    (:get-title-fn #'menubar-get-title-motif)
			    (:set-title-fn #'menubar-set-title-motif)
			    (:parts
			     `((:item-text ,motif-menu-text-label-prototype)
			       :accel-text
			       :underline)))))
    (unless (listp desc)
      (error "expected a list description of a submenu-item, but got ~s.~%"
	     desc))
    (when desc
      (s-value new-submenu-item :desc desc))
    (s-value new-submenu-item :enabled enabled)
    new-submenu-item))

;;; Methods for editing the strings of the menubar and submenus
(define-method :new-item-label gg::motif-menubar (obj)
  (let ((val (1+ (or (g-value obj :last-menubar-label-used)
		     (length (g-value obj :items))))))
    (s-value obj :last-menubar-label-used val)
    (list (format nil "label~a" val) nil '(("sub-item1")))))

(define-method :new-item-label gg::motif-bar-item (obj)
  (let ((val (1+ (or (g-value obj :last-bar-item-label-used)
		     (length (g-value obj :items))))))
    (s-value obj :last-bar-item-label-used val)
    (list (format nil "label~a" val))))

(define-method :string-set-func gg::motif-menubar
  (gadget-obj str-obj final-event final-string)
  ;; first test if we are editing the string of a bar-item
  (let ((a-bar-item (g-value str-obj :parent)))
    (if (is-a-p a-bar-item gg::motif-bar-item)
	(let* ((agglist (g-value gadget-obj :menubar-items))
	       (old-desc (g-value a-bar-item :desc))
	       (new-desc (push final-string (cdr (copy-tree old-desc)))))
	  (put-down-menubar-popups gadget-obj)
	  (opal::aggrelist-edit-string-func gadget-obj agglist str-obj
					    final-event new-desc :rank))
	;; it wasn't the string of a bar-item.  it was a submenu item.
	(let ((agglist (g-value str-obj :parent :parent)))
	  (setf a-bar-item (g-value agglist :parent :bar-item))
	  (opal::aggrelist-edit-string-func a-bar-item agglist str-obj
					    final-event (list final-string)
					    :rank)))))

;;; Allow specifying the char accel
(kr:s-value gg::motif-submenu :accelerators
  (kr:o-formula
   (let ((bar-acc (kr:gvl :bar-item :accelerators))
	 menu-acc temp )
     (when bar-acc
       (dolist (a-list bar-acc)
	 (setf temp nil)
	 (when (second a-list)
	   (setf temp (cons (second a-list) temp)))
	 (when (first a-list)
	   (setf temp (cons (first a-list) temp)))
	 (setf temp (cons (third a-list) temp))
	 (setf menu-acc
	   (append menu-acc (list temp)))))
     menu-acc)))

(defun gw-char-equal (c1 c2)
  (and (characterp c1)
       (characterp c2)
       (char-equal c1 c2)))

;;; accel char should be case insensitive
(kr:s-value motif-menu-accel-inter :final-function
  #'(lambda (interactor obj)
      (declare (ignore obj))
      (let* ((gadget (g-value interactor :operates-on))
	     (char (inter:event-char inter:*current-event*))
	     (accel-chars (g-value gadget :local-accel-chars))
	     (rank (position char accel-chars :test #'gw-char-equal)))
	(when (and rank (g-value gadget :keyboard-selection-p))
	  (let* ((selection (nth rank
				 (g-value gadget :menu-item-list :components)))
		 (action (g-value selection :action))
		 (prev-sel (g-value gadget :value-obj)))
	    (when (g-value (nth rank (g-value gadget :menu-item-list :components))
			   :active-p)
	      ;; propagate new selection toward :value slot
	      (s-value gadget :value-obj selection)
	      (s-value selection :selected t)
	      (if (and prev-sel (not (eq prev-sel selection)))
		  (s-value prev-sel :selected nil))
	      ;; make interim feedback flash if no final-feedback
	      (unless (g-value gadget :final-feedback-p)
		(s-value selection :interim-selected t)
		(opal:update (g-value gadget :window))
		(sleep .25)
		(s-value selection :interim-selected nil))
	      ;; global function for all items
	      (kr-send gadget :selection-function gadget selection)
	      ;; local function assigned to item.
	      ;; if this is in a menubar, you have to call the item
	      ;; function with three arguments. otherwise, with 2 args
	      (when action
		(if (and (g-value gadget :bar-item)
			 (boundp 'motif-bar-item)
			 (is-a-p (g-value gadget :bar-item) motif-bar-item))
		    (funcall action
			     ;; the menubar
			     (g-value gadget :bar-item :parent :parent)
			     ;; the bar-item
			     (g-value gadget :bar-item :menu-obj)
			     ;; the item
			     (g-value selection :item-obj))
		  (funcall action gadget (g-value selection :item-obj))))))))))
