;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-

;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.
;;;
;;;
;;;  Scrolling-Input-String
;;;
;;;  Features and operation of the labeled box:
;;;    1)  The Scrolling-Input-String allows left-right-scrollable text to be
;;;        entered.
;;;    2)  Click the left mouse button on the string to edit it, and press
;;;        return to stop editing.
;;;    3)  The top level :value slot is the string currently appearing inside
;;;        the box.  This slot may be set directly and formulae may depend
;;;        on it.
;;;    4)  A function may be specified in the :selection-function slot to be
;;;        executed after the field text has changed (i.e., after the carriage
;;;        return).
;;;    5)  If the string gets to be too large to fit into the specified
;;;        Width, then the string inside is scrolled left and right so the
;;;        cursor is always visible
;;;    6)  Room is left on both sides of the string for a "..." symbol which
;;;        shows whether the string has been scrolled or not.  Therefore, the
;;;        string will not appear exactly at the :left or extend the full
;;;        :width (since room is left for the ...'s)
;;;
;;;  Customizable slots:
;;;    1)  Left, top
;;;    2)  Width - The width of the string area in pixels.
;;;    3)  Value -- The string that will originally appear in the box and that
;;;                 will be changed
;;;    4)  Selection-Function -- Function to be executed after editing text
;;;    5)  Font -- The font of the string **MUST BE FIXED WIDTH ***
;;;    6)  Line-style -- Setting the line style of the string can be used to
;;; 		    change the color
;;;
;;;  Scrolling-Input-String demo:
;;;    This module contains a function which creates a window and a
;;;    Scrolling-Input-String in the window.  To run it, enter
;;;    (GARNET-GADGETS:Scrolling-Input-String-go).
;;;    To stop, enter (GARNET-GADGETS:Scrolling-Input-String-stop).
;;;
;;;  Designed and written by Brad Myers


(in-package :GARNET-GADGETS)


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Scrolling-Input-String Insert-Text-Into-Box Insert-Text-Into-SIS))
  #+garnet-test
  (export '(Scrolling-Input-String-Go Scrolling-Input-String-Stop
	    Scrolling-Input-String-Top-Agg Scrolling-Input-String-Win
	    Scrolling-Input-String-Obj)))

(in-package :INTERACTORS)

;;; new-cursor-index is based on the on-screen string, not the real-string
;;; The last-vis-char and string fields are set by formulas in the object

(defun shift-vis-string (str-obj new-first-vis new-cursor-index)
    (s-value str-obj :first-vis-char new-first-vis)
    (s-value str-obj :cursor-index new-cursor-index))

(defun scroll-set (string-object real-str real-index first-vis-char len)
  (let ((index (- real-index first-vis-char)))
    (s-value string-object :real-string real-str)
    (if (> index len)
	(shift-vis-string string-object (- real-index len) len)
	(shift-vis-string string-object first-vis-char index))))

(defun scrolling-edit-string (an-interactor string-object event)
  (if (or (null event) (not (schema-p string-object)))
      ;; ignore this event and keep editing
      nil
      ;; else
      (let* ((index (g-value string-object :cursor-index))
	     (str (g-value string-object :string))
	     (len (g-value string-object :vis-length))
	     (first-vis-char (g-value string-object :first-vis-char))
	     (last-vis-char (g-value string-object :last-vis-char))
	     (real-str (g-value string-object :real-string))
	     (real-len (length real-str))
	     pos)
	(if (and (event-mousep event) ; then see if want to move cursor
		 (event-downp event)
		 (g-value an-interactor :cursor-where-press)
		 (setq pos (opal:get-cursor-index string-object (event-x event)
						  (event-y event))))
	    ;; then change the cursor position
	    (s-value string-object :cursor-index pos)
	    ;; else use the translation
	    (let ((new-trans-char
		   (translate-key (event-char event) an-interactor)))
	      (when new-trans-char
		(case new-trans-char
		  (:prev-char (if (and (= index 0)(/= first-vis-char 0))
				  ;; then shift string one right, leave index=0
				  (shift-vis-string string-object
						    (1- first-vis-char)
						    index)
				  ;; else move normally
				  (s-value string-object :cursor-index
					   (max 0 (1- index)))))
		  (:next-char (if (and (= index len)
				       (> (1- real-len) last-vis-char))
				  ;; then shift string to left
				  (shift-vis-string string-object
						    (1+ first-vis-char)
						    index)
				  (s-value string-object :cursor-index
					   (min (length str) (1+ index)))))
		  ;; only single lines
		  (:up-line (beep))
		  (:down-line (beep))
		  (:delete-prev-char
		   (let ((real-index (+ index first-vis-char)))
		     (when (> real-index 0)
		       (s-value string-object :real-string
				(remove-char real-str real-index))
		       (if (= index 0)
			   (shift-vis-string string-object
					     (1- first-vis-char)
					     index)
			   ; else just adjust on-screen version
			   (shift-vis-string string-object
					     first-vis-char
					     (1- index))))))
		  (:delete-prev-word
		   (let ((real-index (+ index first-vis-char)))
		     (multiple-value-setq (real-str real-index)
		       (remove-word real-str real-index))
		     (s-value string-object :real-string real-str)
		     (setq index (- real-index first-vis-char))
		     (if (< index 0)
			 (shift-vis-string string-object real-index 0)
			 (shift-vis-string string-object first-vis-char index))))
		  (:delete-next-char
		   (let ((real-index (+ index first-vis-char)))
		     (when (< real-index real-len)
		       (s-value string-object :real-string
				(remove-char real-str (1+ real-index)))
		       (shift-vis-string string-object first-vis-char index))))
		  (:delete-string
		   (s-value string-object :real-string "")
		   (shift-vis-string string-object 0 0))
		  (:kill-line ;; can only be one line
		   (let ((real-index (+ index first-vis-char)))
		     ;; index and visible part will stay the same
		     ;; since only one line, can't ever add to the kill buffer
		     (s-value string-object :real-string
			      (dokillline real-str real-index nil
					  (event-window event)))))
		  ((:beginning-of-line :beginning-of-string)
		   (if (/= first-vis-char 0)
		       (shift-vis-string string-object 0 0)
		       (s-value string-object :cursor-index 0)))
		  ((:end-of-string :end-of-line)
		   (if (/= last-vis-char (1- real-len))
		       (shift-vis-string string-object (- real-len len) len)
		       ; else end of string is visible
		       (s-value string-object :cursor-index
				  (1+ (- last-vis-char first-vis-char)))))
		  (:copy-to-x-cut-buffer ; don't modify string, but copy it to
		   			 ; the x cut buffer
		   (opal:set-x-cut-buffer (event-window event) real-str))
		  (:copy-from-x-cut-buffer
		   (let ((real-index (+ index first-vis-char)))
		     (multiple-value-setq (real-str real-index)
		       (add-x-cut-buffer real-str real-index (event-window event)))
		     (scroll-set string-object real-str real-index
				 first-vis-char len)))
		  (t
		   ;; here might be a keyword, character, string, or
		   ;; function
		   (cond ((event-mousep event) nil) ; ignore these
			 ((and (characterp new-trans-char)
			       (or (graphic-char-p new-trans-char)
				   (eql new-trans-char #\newline)))
			  ; then is a regular character, so add to str
			  (let ((real-index (+ index first-vis-char)))
			    (s-value string-object :real-string
				     (add-char new-trans-char real-str real-index))
			    (if (= index len)
				(shift-vis-string string-object
						  (1+ first-vis-char) len)
				(shift-vis-string string-object
						  first-vis-char (1+ index)))))
			 ;; check if a string
			 ((stringp new-trans-char) ; then insert into string
			  (let ((real-index (+ index first-vis-char)))
			    (s-value string-object :real-string
				     (concatenate 'string
						  (subseq real-str 0 real-index)
						  new-trans-char
						  (subseq real-str real-index)))
			    (setq real-index (+ real-index (length new-trans-char)))
			    (scroll-set string-object real-str real-index
				   first-vis-char len)))
			 ; now check for functions
			 ((if (symbolp new-trans-char) ; check if a function,
			      		  ; need all 3 tests to do it right!
			      (fboundp new-trans-char)
			      (functionp new-trans-char))
			  ; then call the function
			  (funcall new-trans-char an-interactor
				   string-object event))
			 (t ; otherwise, must be a bad character or an
			    ; undefined edit operation
			  (beep)))))))))))

(in-package :garnet-gadgets)


;;; insert-text-into-sis is used to insert a string into a
;;; scrolling-input- string gadget.
(defun insert-text-into-sis (sis str)
  (let* ((text-obj (g-value sis :string))
	 (real-string (g-value text-obj :real-string))
	 (split-index (+ (g-value text-obj :first-vis-char)
			 (g-value text-obj :cursor-index)))
	 (new-string (concatenate 'string
				   (subseq real-string 0 split-index)
				   str
				   (subseq real-string split-index)))
	 (new-real-index (+ split-index (length str)))
	 (new-first-vis-char (max 0 (- (g-value text-obj :last-vis-char)
				       (length str))))
	 (len (g-value text-obj :vis-length)))
    ;; scroll-set is defined in the scrolling-input-string gadget.
    (inter::scroll-set text-obj new-string new-real-index
		       new-first-vis-char len)))

;;; insert-text-into-box is used to insert a string into the field string
;;; of a scrolling-labeled-box or motif-scrolling-labeled-box.
(defun insert-text-into-box (l-box str)
  (let* ((text-obj (g-value l-box :field-text :string))
	 (real-string (g-value text-obj :real-string))
	 (split-index (+ (g-value text-obj :first-vis-char)
			 (g-value text-obj :cursor-index)))
	 (new-string (concatenate 'string
				   (subseq real-string 0 split-index)
				   str
				   (subseq real-string split-index)))
	 (new-real-index (+ split-index (length str)))
	 (new-first-vis-char (max 0 (- (g-value text-obj :last-vis-char)
				       (length str))))
	 (len (g-value text-obj :vis-length)))
    ; scroll-set is defined in the scrolling-input-string gadget.
    (inter::scroll-set text-obj new-string new-real-index
		       new-first-vis-char len)))

(create-instance 'small-font opal:font (:size :small)(:family :serif))

(create-instance 'dot-dot-dot opal:text
  (:constant '(:font :string))
  (:font small-font)
  (:string "..."))

(defparameter *dot-dot-width* (g-value dot-dot-dot :width))

(create-instance 'scrolling-input-text-edit inter:text-interactor
   (:window (o-formula (gvl :operates-on :window)))
   (:start-where (o-formula (list :in (gvl :operates-on))))
   (:start-event :leftdown)
   (:stop-event (list  #\return :control-j :control-\j))
   (:obj-to-change (o-formula (gvl :operates-on :string)))
   (:edit-func #'inter::scrolling-edit-string)
   (:active (o-formula (and (gvl :operates-on :active-p)
			    (gvl :operates-on :visible)
			    (gvl :window))))
   (:start-action
    #'(lambda (an-interactor obj-over start-event)
	(let ((str (g-value an-interactor :operates-on :value)))
	  (s-value obj-over :real-string str)
	  (s-value an-interactor :original-real-string
		   ;; in case aborted
		   (copy-seq str))
	  (s-value an-interactor :original-first-vis-char
		   (g-value obj-over :first-vis-char))
	  (inter::shift-vis-string obj-over
				   (g-value obj-over :first-vis-char)
				   (g-value obj-over :cursor-index)))
	(call-prototype-method an-interactor obj-over start-event)))
   (:abort-action
    #'(lambda (an-interactor obj-over abort-event)
	(s-value obj-over :real-string
		 (g-value an-interactor :original-real-string))
	(s-value obj-over :first-vis-char
		 (g-value an-interactor :original-first-vis-char))
	(call-prototype-method an-interactor obj-over abort-event)))
   (:final-function
    #'(lambda (interactor obj event final-string x y)
	(declare (ignore obj event x y final-string))
	(let* ((op-on (g-value interactor :operates-on))
	       (real-string (g-value op-on :string :real-string)))
	  (s-value op-on :value real-string)
	  ;; execute global :selection-function
	  (kr-send op-on :selection-function op-on real-string)))))

(create-instance 'scrolling-input-string opal:aggregadget
  :declare ((:parameters :left :top :width :value :font :line-style :active-p
			 :visible)
	    (:type (string :value)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((or (is-a-p opal:line-style) null) :line-style)
		   (kr-boolean :active-p)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :font :line-style :active-p
			     :visible))
  ;; customizable slots
  (:left 0)
  (:top 0)
  (:width 100)
  (:value "type here")
  (:selection-function nil)
  ;; **must be fixed width**
  (:font opal:default-font)
  (:active-p t)
  (:line-style (o-formula (if (gvl :active-p)
			      opal:default-line-style
			      opal:gray-line)))
					; Generally non-customizable slots
  (:field-char-width (o-formula (opal:string-width (gvl :font) "w")))
  (:height (o-formula (opal:string-height (gvl :font) "x")))
  (:parts
   `((:string ,opal:cursor-text
	      (:left ,(o-formula (+ (gvl :parent :left) *dot-dot-width*)))
	      (:top ,(o-formula (gvl :parent :top)))
	      (:max-width ,(o-formula (let ((padding (* 2 *dot-dot-width*)))
					(max (- (gvl :parent :width) padding)
					     padding))))
	      (:font ,(o-formula (gvl :parent :font)))
	      (:line-style ,(o-formula (gvl :parent :line-style)))
	      ;; first-vis-char is the index into real-string of the
	      ;; first visible set by interactor
	      (:first-vis-char 0)
	      ;; set by interactor
	      (:cursor-index nil)
	      ;; the number of characters that can be shown
	      (:vis-length
	       ,(o-formula (floor (gvl :max-width)
				  (gvl :parent :field-char-width))))
	      ;; the next one is also set by the interactor directly
	      (:real-string ,(o-formula (gvl :parent :value)))
	      ;; last-vis-char is the index into real-string of the last visible
	      (:last-vis-char
	       ,(o-formula (let ((real-len (length (gvl :real-string)))
				 (first-vis (gvl :first-vis-char))
				 (vis-len (gvl :vis-length)))
			     ;; check in case someone sets the :value slot
			     (when (> first-vis real-len)
			       (setq first-vis 0)
			       (s-value (gv :self) :first-vis-char first-vis))
			     (if (< (- real-len first-vis) vis-len)
				 (1- real-len)
				 (+ first-vis vis-len -1)))))
	      (:string ,(o-formula (let* ((last (gvl :last-vis-char))
					  (first (gvl :first-vis-char)))
				     ;; guarantee that last-vis asked before first,
				     ;; because it may s-value first.
				     (subseq (gvl :real-string)
					     first (1+ last)))))
	      (:fast-redraw-p ,(o-formula (gvl :parent :fast-redraw-p)))
	      (:fast-redraw-filling-style ,(o-formula (gvl :parent :fast-redraw-filling-style))))
     (:dot1 ,dot-dot-dot
	    (:visible ,(o-formula
			(and (gvl :parent :visible)
			     (/= 0 (gvl :parent :string :first-vis-char)))))
	    (:left ,(o-formula (gvl :parent :left)))
	    (:top ,(o-formula  (gvl :parent :top))))
     (:dot2 ,dot-dot-dot
	    (:visible ,(o-formula
			(and (gvl :parent :visible)
			     (/= (gvl :parent :string :last-vis-char)
				 (1- (length
				      (gvl :parent :string :real-string)))))))
	    (:left ,(o-formula (+ (gvl :parent :left)
				  (- (gvl :parent :width) *dot-dot-width*))))
	    (:top ,(o-formula (gvl :parent :top))))))
  (:interactors
   `((:text-edit ,scrolling-input-text-edit))))


;;;  Demo function


#+garnet-test (defparameter scrolling-input-string-win nil)
#+garnet-test (defparameter scrolling-input-string-top-agg nil)
#+garnet-test (defparameter scrolling-input-string-obj nil)

#+garnet-test
(defun scrolling-input-string-go ()
  (create-instance 'scrolling-input-string-win inter:interactor-window
     (:height 100)(:width 350)(:top 5)(:left 650))
  (s-value scrolling-input-string-win
	   :aggregate
	   (create-instance 'scrolling-input-string-top-agg opal:aggregate))
  (create-instance 'scrolling-input-string-obj scrolling-input-string
     (:left 50) (:top 50)
     (:selection-function #'(lambda (obj value)
			     (format t "final string for ~s is ~s~%" obj value))))
  (opal:add-components scrolling-input-string-top-agg scrolling-input-string-obj)
  (opal:update scrolling-input-string-win))

#+garnet-test
(defun scrolling-input-string-stop ()
  (opal:destroy scrolling-input-string-win))
