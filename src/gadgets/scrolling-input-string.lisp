;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

#|
============================================================
Change log:
  01/26/94  Andrew Mickish - Added control-j to :stop-event
  03/19/93  Brad Myers - added ^k
  03/08/93  Andrew Mickish - Added Insert-Text-Into-SIS
  03/01/93  Andrew Mickish - Fixed paren mismatch in type declaration
  12/14/92  Andrew Mickish - Added type and parameter declarations
  11/30/92  Andrew Mickish - Moved Insert-Text-Into-Box here from motif-slb
  11/25/92  Andrew Mickish - Added :active-p
  10/30/92  Andrew Mickish - Set :first-vis-char to 0 in :last-vis-char formula
  03/30/92  Brad Myers - fixed so cutting to X gets whole string
  02/11/92  Andrew Mickish - Added :maybe-constant list
  01/28/92  Ed Pervin - Must have # before '(lambda for CMUCL.
  10/07/91  Andrew Mickish - Added fast-redraw-p slots
  08/02/91  Andrew Mickish - Moved S-I-S's :string part's :width formula
              to its :max-width slot, and accordingly changed reference
              in :vis-length formula.
  07/28/91  Andrew Mickish - Added :height slot to S-I-S so that :height
              now depends only on the font and not on the string.
  05/13/91  Ed Pervin - removed extra right parenthesis
  11/19/90  Brad Myers - Fixed bugs with ^E and s-valueing :value after
              started (reported by Karen.York.Kietzke)
  09/22/90  Brad Myers - Fixed so can be made invisible
  09/05/90  Brad Myers - added line-style so you can change the font color
  06/01/90  Brad Myers - created
============================================================
|#

(in-package :GARNET-GADGETS)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Scrolling-Input-String Insert-Text-Into-Box Insert-Text-Into-SIS))
  #+garnet-test
  (export '(Scrolling-Input-String-Go Scrolling-Input-String-Stop
	    Scrolling-Input-String-Top-Agg Scrolling-Input-String-Win
	    Scrolling-Input-String-Obj)))

;;;-------------------------------------------------------------------------
(in-package :INTERACTORS)

;;;new-cursor-index is based on the on-screen string, not the real-string
;;; The last-vis-char and string fields are set by formulas in the object
(defun Shift-Vis-String (str-obj new-first-vis new-cursor-index)
    (s-value str-obj :first-vis-char new-first-vis)
    (s-value str-obj :cursor-index new-cursor-index))

(defun Scroll-Set (string-object real-str real-index first-vis-char len)
  (let ((index (- real-index first-vis-char)))
    (s-value string-object :real-string real-str)
    (if (> index len)
	(shift-vis-string string-object (- real-index len) len)
	(shift-vis-string string-object first-vis-char index))))
  
(defun Scrolling-Edit-String (an-interactor string-object event)
  (if (or (null event) (not (schema-p string-object)))
      NIL ; ignore this event and keep editing
      ; else
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
	    ; then change the cursor position
	    (s-value string-object :cursor-index pos)
	    ; else use the translation
	    (let ((new-trans-char
		   (Translate-key (event-char event) an-interactor)))
	      (when new-trans-char
		(case new-trans-char
		  (:prev-char (if (and (= index 0)(/= first-vis-char 0))
				  ; then shift string one right, leave index=0
				  (shift-vis-string string-object
						    (1- first-vis-char)
						    index)
				  ; else move normally
				  (s-value string-object :cursor-index
					   (max 0 (1- index)))))
		  (:next-char (if (and (= index len)
				       (> (1- real-len) last-vis-char))
				  ; then shift string to left
				  (shift-vis-string string-object
						    (1+ first-vis-char)
						    index)
				  (s-value string-object :cursor-index
				       (min (length str) (1+ index)))))
		  (:up-line (beep)) ; only single lines
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
		     ;; since only one line, can't ever ADD to the kill buffer
		     (s-value string-object :real-string 
			      (DoKillLine real-str real-index NIL
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
		  (:copy-to-X-cut-buffer ; don't modify string, but copy it to
		   			 ; the X cut buffer
		   (opal:Set-X-Cut-Buffer (event-window event) real-str))
		  (:copy-from-X-cut-buffer
		   (let ((real-index (+ index first-vis-char)))
		     (multiple-value-setq (real-str real-index)
		       (Add-X-Cut-Buffer real-str real-index (event-window event)))
		     (Scroll-Set string-object real-str real-index
				   first-vis-char len)))
		  (T ;; here might be a keyword, character, string, or function
		   (cond ((event-mousep event) NIL) ; ignore these
			 ((and (characterp new-trans-char)
			       (or (graphic-char-p new-trans-char)
				   (eql new-trans-char #\NewLine)))
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
			    (Scroll-Set string-object real-str real-index
				   first-vis-char len)))
			 ; now check for functions
			 ((if (symbolp new-trans-char) ; check if a function,
			      		  ; need all 3 tests to do it right!
			      (fboundp new-trans-char)
			      (functionp new-trans-char))
			  ; then call the function
			  (funcall new-trans-char an-interactor
				   string-object event))
			 (T ; otherwise, must be a bad character or an
			    ; undefined edit operation 
			  (Beep)))))))))))
;;;-------------------------------------------------------------------------
(in-package :GARNET-GADGETS)


;;; INSERT-TEXT-INTO-SIS is used to insert a string into a Scrolling-Input-
;;; String gadget.
;;;
(defun Insert-Text-Into-SIS (sis str)
  (let* ((text-obj (g-value sis :string))
	 (real-string (g-value text-obj :real-string))
	 (split-index (+ (g-value text-obj :first-vis-char)
			 (g-value text-obj :cursor-index))) 
	 (new-string (concatenate 'string
				   (subseq real-string 0 split-index)
				   str
				   (subseq real-string split-index)))
	 (new-real-index (+ split-index (length str)))
	 (new-first-vis-char (MAX 0 (- (g-value text-obj :last-vis-char)
				       (length str))))
	 (len (g-value text-obj :vis-length)))
    ; Scroll-Set is defined in the Scrolling-Input-String gadget.
    (inter::scroll-set text-obj new-string new-real-index
		       new-first-vis-char len)))

;;; INSERT-TEXT-INTO-BOX is used to insert a string into the field string
;;; of a scrolling-labeled-box or motif-scrolling-labeled-box.
;;;
(defun Insert-Text-Into-Box (l-box str)
  (let* ((text-obj (g-value l-box :field-text :string))
	 (real-string (g-value text-obj :real-string))
	 (split-index (+ (g-value text-obj :first-vis-char)
			 (g-value text-obj :cursor-index))) 
	 (new-string (concatenate 'string
				   (subseq real-string 0 split-index)
				   str
				   (subseq real-string split-index)))
	 (new-real-index (+ split-index (length str)))
	 (new-first-vis-char (MAX 0 (- (g-value text-obj :last-vis-char)
				       (length str))))
	 (len (g-value text-obj :vis-length)))
    ; Scroll-Set is defined in the Scrolling-Input-String gadget.
    (inter::scroll-set text-obj new-string new-real-index
		       new-first-vis-char len)))
		      

(create-instance 'small-font opal:font (:size :small)(:family :serif))
(create-instance 'dot-dot-dot opal:text
  (:constant '(:font :string))
  (:font small-font)
  (:string "..."))

(defparameter dot-dot-width (g-value dot-dot-dot :width))

(create-instance 'SCROLLING-INPUT-TEXT-EDIT inter:text-interactor
   (:window (o-formula (gvl :operates-on :window)))
   (:start-where (o-formula (list :in (gvl :operates-on))))
   (:start-event :leftdown)
   (:stop-event (list  #\RETURN :CONTROL-J :CONTROL-\j))
   (:obj-to-change (o-formula (gvl :operates-on :string)))
   (:edit-func #'inter::Scrolling-Edit-String)
   (:active (o-formula (and (gvl :operates-on :active-p)
			    (gvl :operates-on :visible)
			    (gvl :window))))
   (:start-action
    #'(lambda (an-interactor obj-over start-event)
	(let ((str (g-value an-interactor :operates-on :value)))
	  (s-value obj-over :real-string str)
	  (s-value an-interactor :original-real-string
		   (copy-seq str)) ; in case aborted
	  (s-value an-interactor :original-first-vis-char
		   (g-value obj-over :first-vis-char))
	  (inter::Shift-Vis-String obj-over
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
	  ;; Execute global :selection-function
	  (kr-send op-on :selection-function op-on real-string)))))

(create-instance 'SCROLLING-INPUT-STRING opal:aggregadget
   :declare ((:parameters :left :top :width :value :font :line-style :active-p
			  :visible)
	     (:type (string :value)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		    ((or (is-a-p opal:line-style) null) :line-style)
		    (kr-boolean :active-p)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :width :font :line-style :active-p
			      :visible))
   ; Customizable slots
   (:left 0) (:top 0)
   (:width 100)
   (:value "Type here")
   (:selection-function NIL)
   (:font opal:default-font) ;;**Must be fixed width**
   (:active-p T)
   (:line-style (o-formula (if (gvl :active-p)
			       opal:default-line-style
			       opal:gray-line)))

   ; Generally non-customizable slots
   (:field-char-width (o-formula (opal:string-width (gvl :font) "w")))
   (:height (o-formula (opal:string-height (gvl :font) "X")))
   (:parts
    `((:string ,opal:cursor-text
	       (:left ,(o-formula (+ (gvl :parent :left) dot-dot-width)))
	       (:top ,(o-formula (gvl :parent :top)))
	       (:max-width ,(o-formula (- (gvl :parent :width) (* 2 dot-dot-width))))
	       (:font ,(o-formula (gvl :parent :font)))
	       (:line-style ,(o-formula (gvl :parent :line-style)))

	         ;first-vis-char is the index into real-string of the first visible
	       (:first-vis-char 0) ; set by interactor
	       (:cursor-index NIL) ; set by interactor
	       (:vis-length   ;; the number of characters that can be shown
		,(o-formula (floor (gvl :max-width)
				   (gvl :parent :field-char-width))))
	       ;;the next one is also set by the interactor directly
	       (:real-string ,(o-formula (gvl :parent :value)))
	         ;last-vis-char is the index into real-string of the last visible
	       (:last-vis-char
		,(o-formula (let ((real-len (length (gvl :real-string)))
				  (first-vis (gvl :first-vis-char))
				  (vis-len (gvl :vis-length)))
			      ;; check in case someone sets the :value slot
			      (when (> first-vis real-len)
				(setq first-vis 0)
				(s-value (gv :SELF) :first-vis-char first-vis))
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
				   (- (gvl :parent :width) dot-dot-width))))
	     (:top ,(o-formula (gvl :parent :top))))))
   (:interactors
    `((:text-edit ,SCROLLING-INPUT-TEXT-EDIT))))

;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test (defparameter Scrolling-Input-String-Win NIL)
#+garnet-test (defparameter Scrolling-Input-String-Top-Agg NIL)
#+garnet-test (defparameter Scrolling-Input-String-Obj NIL)

#+garnet-test
(defun Scrolling-Input-String-Go ()
  (create-instance 'Scrolling-Input-String-win inter:interactor-window
     (:height 100)(:width 350)(:top 5)(:left 650))
  (s-value Scrolling-Input-String-win
	   :aggregate
	   (create-instance 'Scrolling-Input-String-top-agg opal:aggregate))
  (create-instance 'Scrolling-Input-String-obj Scrolling-Input-String
     (:left 50) (:top 50)
     (:selection-function #'(lambda (obj value)
			     (format T "Final string for ~s is ~s~%" obj value))))
  (opal:add-components Scrolling-Input-String-top-agg Scrolling-Input-String-Obj)
  (opal:update Scrolling-Input-String-win))

#+garnet-test
(defun Scrolling-Input-String-Stop ()
  (opal:destroy Scrolling-Input-String-win))
