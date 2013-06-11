;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The "Value Filter" extension to Gilt is a tool for assigning dependencies
;;; among objects.  With the Value Control module, the values returned by
;;; gadgets can be defined by a user-supplied expression.  The Enable Control
;;; module allows the user to define expressions that regulate whether gadgets
;;; are enabled and may be operated.  The Error Checking module allows the
;;; definition of an error handling routine, complete with a customized
;;; error dialog box.
;;;
;;; Designed by Brad Myers
;;; Implemented by Andrew Mickish

;;
;;  Value Control module
;;

;;; CHANGE LOG:
;;; 08/03/93 Andrew Mickish - #\control-j  --->  :control-j
;;; 06/30/93 Andrew Mickish - Added :lisp-mode-p to EDITABLE-TEXT
;;; 03/08/93 Brad Myers - invoke from menubar
;;; 02/02/93 Andrew Mickish - opal:set-strings ---> opal:set-text


(in-package "GILT")

(declaim (special VALUE-CONTROL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *Obj-Name-Assoc-List* NIL)
(defparameter *User-Name-Cnt* 0)

(defun Get-User-Name (for-obj)
  (let ((old-name (assoc for-obj *Obj-Name-Assoc-List*)))
    (if old-name
	(cdr old-name)
	; else make a name
	(let ((name (or (when (null for-obj) "NIL")
			(if (schema-p for-obj)
			    (or (g-value for-obj :known-as)
				(g-value for-obj :label-string)
				(g-value for-obj :string)
				(g-value for-obj :items)
				(kr:name-for-schema for-obj))
			    for-obj))))
	  (unless (stringp name) (setq name (prin1-to-string name)))
	  (when (rassoc name *Obj-Name-Assoc-List* :test #'equal)
	      (incf *User-Name-Cnt*)
	      (setq name (format NIL "~a-~a" name *User-Name-Cnt*)))
	  (push (cons for-obj name) *Obj-Name-Assoc-List*)
	  name))))


(defun Panel-P (obj)
  (or (and (boundp 'gg::text-button-panel)
	   (is-a-p obj gg::text-button-panel))
      (and (boundp 'gg::radio-button-panel)
	   (is-a-p obj gg::radio-button-panel))
      (and (boundp 'gg::x-button-panel)
	   (is-a-p obj gg::x-button-panel))
      (and (boundp 'gg::motif-text-button-panel)
	   (is-a-p obj gg::motif-text-button-panel))
      (and (boundp 'gg::motif-radio-button)
	   (is-a-p obj gg::motif-radio-button-panel))
      (and (boundp 'gg::motif-check-button-panel)
	   (is-a-p obj gg::motif-check-button-panel))))

(defun Button-P (obj)
  (let ((button-list (g-value obj :parent)))
    (if button-list
	(let ((panel (g-value button-list :parent)))
	  (panel-p panel)))))

;; Insert a "(gvl ... :filtered-value)" call into the given text-obj
;;
(defun Insert-The-Ref (src-obj dest-obj text-obj slot)
  (let ((ref `(gvl ,@(make-path src-obj dest-obj) ,slot)))
    (opal:insert-text text-obj (write-to-string ref :case :downcase))))

;; Selection function for the "Use Value of Object" button
;;
(defun Insert-Ref-Obj-Into-Str (gadget val)
  (declare (ignore val))
  (let* ((vc (g-value gadget :parent))
	 (sel-obj (g-value *Selection-Obj* :value))
	 (text-obj (g-value vc :filter-text-obj))
	 (for-obj (g-value vc :for-object))
	 (slot (g-value vc :slot-to-reference)))
    (cond ((null sel-obj)(gilt-error "Nothing selected"))
	  ((listp sel-obj)
	   (dolist (o (reverse sel-obj))
	     (Insert-The-Ref for-obj o text-obj slot)))
	  (t (Insert-The-Ref for-obj sel-obj text-obj slot)))))

(defun Lookup-Fn-Types (fn)
  (cdr (assoc fn *Param-Type-List*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Funcitons to infer the value filter expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Variable used when trying to convert the default value to a symbol
;;
(defparameter *keyword-package* (find-package 'keyword))

;; Given new-val is a string, can real-val be converted into new-val by going
;; to all uppercase or all lowercase?  Return :upcase or :downcase if yes,
;; otherwise, return :nope.
;;
(defun Check-String-Conv (real-val new-val)
  (if (stringp real-val)
      (cond ((string= (string-upcase real-val) new-val) :upcase)
	    ((string= (string-downcase real-val) new-val) :downcase)
	    (T :nope))
      :nope))

;; Given new-val is a number, can real-val be converted into the same number?
;; Return :number if real-val is a string representation of the new-val number,
;; :zero-index or :one-index if appropriate, or :nope if no obvious trans.
;;
(defun Check-Number-Conv (real-val new-val)
  (cond
    ((numberp real-val) :nope) ; Both numbers, not equal because already
                               ; checked equality.  Can't guess.
    ((stringp real-val)
     (multiple-value-bind (num errorp)
	 (gg:careful-read-from-string real-val *error-gadget*)
       (if errorp
	   :nope
	   (if (numberp num)
	       (if (= num new-val)
		   :number   ; Real-val is string representation of new-val
		   :nope)    ; Some different number, can't guess.
	       (if num
		   ;; else real-val is not a number
		   ;; check-index returns NIL or :zero-index or :one-index 
	           :nope ;;; (check-index for-obj new-val)
		         ;;; CHECK-INDEX not implemented yet
		   :nope)))))
    (T :nope)))

;; Given new-val is a symbol, can real-val be converted into the same symbol?
;; (Keywords like :fixed and atoms like 'fixed are subtypes of symbols.)
;; Return :keyword, :atom, or :nope.
;;
(defun Check-Symbol-Conv (real-val new-val)
  (if (stringp real-val)
      (let ((upcase-real-val (substitute #\- #\space
					 (string-upcase real-val))))
	(format t "new-val = ~S~%" new-val)
	(if (string= (symbol-name new-val) upcase-real-val)
	    (if (eq *keyword-package* (symbol-package new-val))
		:keyword
		:atom)
	    ;; otherwise, symbol but don't know how to transform
	    :nope))
      ;; otherwise, is symbol to symbol
      :nope))

(defun Make-Cond-Branch (real-val new-val spaces-p)
  ;; If the old-val is a list, then we must quote it so it will not be
  ;; interpreted as a function
  (let ((val (if (and real-val (listp real-val)) `',real-val real-val)))
    (format NIL "~a((equal val ~s) ~s)~%"
	    (if spaces-p "        " "")
	    val new-val)))

(defun Make-Cond-Stmt (real-val new-val other-vals)
  (let ((old-pair (assoc real-val other-vals :test #'equal)))
    (when old-pair (setq other-vals
			 (remove old-pair other-vals :test #'equal))))
  (let ((str (concatenate 'simple-string
"(let ((val (gv :self :value)))
  (cond " (make-cond-branch real-val new-val NIL))))
    (dolist (o other-vals)
      (setq str (concatenate 'simple-string str
			     (make-cond-branch (car o)(cdr o) T))))
    (setq str (concatenate 'simple-string str
			   "        (T val)))"))))

;;   Only need to pass real-val and new-val if you are also sending :nope
;; as the value of trans-key (which is the case to make a cond statement).
;;
(defun Lookup-Trans (trans-key &optional real-val new-val)
  (case trans-key
    (:nothing "(gv :self :value)")
    (:upcase "(string-upcase (gv :self :value))")
    (:downcase "(string-downcase (gv :self :value))")
    (:number "(Gilt:Make-Number (gv :self :value))")
    (:zero-index "(Gilt:Index-Of (gv :self :value))")
    (:one-index "(1+ (Gilt:Index-Of (gv :self :value)))")
    (:keyword "(Gilt:Make-Keyword (gv :self :value))")
    (:atom "(Gilt:Make-Atom (gv :self :value))") 
    (:nope (Make-Cond-Stmt real-val new-val NIL))
    (T (error "bad return ~s" trans-key))))
	

;; Real-val is the unfiltered value of the widget, and new-val is the
;; user-edited value.  Try to find how they relate, and return a string
;; representation of the lisp code that will perform the conversion.
;;
;; Keywords are used to denote the type of transformation to make, stored in
;; the variable 'trans-key'.  All transformations assume that real-val is a
;; string.  See the case statement in Lookup-Trans for a list of
;; allowed values.
;;
(defun Create-Filter-Expression (real-val new-val other-vals)
  (if other-vals
      ;; Just make a cond statement with available value pairs
      (Make-Cond-Stmt real-val new-val other-vals)
      ;; Try to find a conversion from real-val to new-val, and store the
      ;; keyword name of the transformation in trans-key
      (let ((trans-key
	     (cond ((equal new-val real-val) :nothing) ; same
		   ((stringp new-val) (Check-String-Conv real-val new-val))
		   ((numberp new-val) (Check-Number-Conv real-val new-val))
		   ((symbolp new-val) (Check-Symbol-Conv real-val new-val))
		   ((consp new-val) (if (eq 'quote (car new-val))
					(Check-Symbol-Conv real-val
							   (second new-val))
					:nope))
		   (T :nope))))
	(Lookup-Trans trans-key real-val new-val))))


;;   Infer-Filter is the top-level function for inferring the transformation
;; function:  default value --> user's desired value
;; 
(defun Infer-Filter (result-box new-val)
  (let* ((real-val (g-value result-box :real-value))
	 (other-vals (g-value result-box :real-edited-pairs))
	 (str-obj (g-value result-box :parent :filter-text-obj))
	 (user-edited-p (g-value str-obj :user-edited-p))
	 (filter-already-p (not (zerop (length (g-value str-obj :value)))))
	 new-expr errorp)
    
    ;; Signal an error if the user has already started typing in a filter
    (when (and user-edited-p filter-already-p)
      (gilt-error "Sorry, cannot guess a value filter after user editing,
unless you erase the Filter expression first.")
      (s-value result-box :value (prin1-to-string real-val))
      (return-from Infer-Filter))
    
    ;; if user has erased the filter expression, then ignore the old val pairs
    (unless filter-already-p
      (setq other-vals NIL))

    ;; Destringify what the user typed in
    (multiple-value-setq (new-val errorp)
      (gg:careful-read-from-string new-val *error-gadget*))
    (when errorp (s-value result-box :value (prin1-to-string real-val))
      (return-from Infer-Filter))

    ;; Add this new actual-value/desired-value pair to the list of pairs.
    ;; This list will be used to figure out a filter expression.
    (let ((old-pair (assoc real-val other-vals)))
      (cond (old-pair (rplacd old-pair new-val)
		      (mark-as-changed result-box :real-edited-pairs))
	    (t (s-value result-box :real-edited-pairs
			(cons (cons real-val new-val) other-vals)))))
    (setq new-expr (Create-Filter-Expression real-val new-val other-vals))

    ;; Must to set both the string that is displayed and the string that is
    ;; used internally.
    (when new-expr
      (opal:set-text str-obj new-expr)
      (s-value str-obj :value new-expr)
      (s-value str-obj :user-edited-p NIL))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions which analyze the filter expression that the user typed.
;; If the expression contains references to other objects (calls to gv),
;; then pop up value control panels for those objects, too.  As the new
;; value control windows come up, try to determine what their filter
;; expressions should be based on the current values of the referenced
;; objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Used to guess a filter transformation for a new value control menu
;; that just popped up.  Called by the function that pops up the new menu.
;;
(defun Get-Trans (value type)
  (cond
    ((typep value type) :nothing)     ; No conversion required
    ((not (stringp value)) :nothing)  ; Only know how to convert from strings
    (t (case type
	 (number :number)
	 (keyword :keyword)
	 (string :nothing)
	 (atom :atom)
	 (t :nothing)))))             ; Can't guess a conversion

;; Check whether the expression is of the form (gv obj :filtered-value).
(defun Is-A-Call-To-GV (expr)
  (and (listp expr)
       (eq 'gv (first expr))
       (eq :filtered-value (first (last expr)))))

;; Check whether the expression is of the form (gvl slot)
(defun Is-A-Call-To-GVL (expr)
  (and (listp expr)
       (eq 'gvl (first expr))
       (eq :filtered-value (first (last expr)))))

;; Check whether the expression is of the form (gv ... :filtered-value)
;; or (gvl ... :filtered-value).
(defun Is-A-Ref (expr)
  (or (Is-A-Call-To-GV expr)
      (Is-A-Call-To-GVL expr)))

(defun Get-Obj-From-Ref (expr for-obj)
  (let (obj path)
    (cond ((Is-A-Call-To-GVL expr)
	   (setf obj for-obj)
	   (setf path (cdr expr)))
	  ((Is-A-Call-To-GV expr)
	   (setf obj (second expr))
	   (setf path (cddr expr)))
	  (t (error "Could not identify the object referenced by the expression ~S" expr)))
    (dolist (current-slot path)
      ; stop at obj whose :filtered-value slot is referenced
      (unless (eq :filtered-value current-slot)
	(setf obj (g-value obj current-slot))))
    obj))


;; Given that the expr is a reference to another object's :filtered-value slot,
;; bring up a new value-control window for the referenced object.  Guess a
;; filter function for the object using the type.
(defun Show-Value-Control-For-Ref (expr type for-obj)
  (let* ((obj (Get-Obj-From-Ref expr for-obj))
	 (value (g-value obj :value))
	 (old-filtered-value (gg:Careful-Eval (g-value obj :filtered-value))))
    (if old-filtered-value
	;; No need to guess a transformation because there already is a
	;; filter for the object.
	(Show-Value-Control obj type)
	;; Guess a filter expression to transform the current
	;; :value of the object into the type required by the fn.
	(Show-Value-Control obj (if (eq type :nothing) 
				    :nothing
				    (Get-Trans value type)) type))))

;;   Get-Filters-For-Params takes a filter expression and analyzes it.  If the
;; function call is to one of the standard Gilt functions, then the parameters
;; are type-checked.  For each type mismatch, a new value control window is
;; created and the user will have the opportunity to enter a filter expression
;; for the lower-level offending objects.
;;
;;   Returns T if the dialog boxes for the parameters are popped up, and
;; NIL if an incorrect number of parameters were passed to a known function.
;;
(defun Get-Filters-For-Params (expression vc)
  (let ((for-obj (g-value vc :for-object)))
  (if (listp expression)
    (if (Is-A-Call-To-GV expression)
	;; Then pop up a new value control window for the referenced object
	;; and return T.
	(progn
	  (Show-Value-Control-For-Ref expression :nothing for-obj)
	  T)
	;; Otherwise, check if the call is to a predefined Gilt function.
	(let* ((top-level-fn (first expression))
	       (parameters (rest expression))
	       (type-list (Lookup-Fn-Types top-level-fn)))
	  (cond
	    (type-list
	     ;; Then it is a known function.  If we got the right number of
	     ;; parameters, then pop up a menu for each parameter and return T.
	     (s-value vc :expected-type type-list)
	     (when (eq (length type-list) (length parameters))
	       (do* ((i 0 (+ i 1))
		     (type (nth i type-list) (nth i type-list))
		     (param (nth i parameters) (nth i parameters)))
		    ((null type) T)
		 (cond
		   ((listp param)
		    (when (Is-A-Ref param)
		      (Show-Value-Control-For-Ref param type for-obj)))))))
	    (t 
	     ;; Don't know anything about the function, so pop up menus
	     ;; for all parameters that are references and return T.
	     (dolist (param parameters)
	       (when (Is-A-Call-To-GV param)
		 (Show-Value-Control-For-Ref param :nothing for-obj)))
	     T))))
    (progn
      (s-value vc :expected-type NIL)
      T)
    )))

;; Given a list of types and an expression, check whether each parameter in
;; the expression matches its expected type.  Return T if there is a type
;; violation, and return NIL if all params are of the expected type.
;;
(defun Check-Each-Param (expected-types expr for-obj)
  (let ((params (cdr expr)))
    (if (eq (length params) (length expected-types))
	(do* ((i 0 (+ i 1))
	      (type (nth i expected-types) (nth i expected-types))
	      (param (nth i params) (nth i params)))
	     ((null type) NIL)
	  (unless (typep (gg:Careful-Eval-Formula-Lambda
			  param *error-gadget* NIL for-obj NIL :ignore NIL)
			 type)
	    (return T)))
	T)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that set the filtered value of the object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun Set-Filter-Slot (expression vc)
  (let ((obj (g-value vc :for-object)))
    (s-value obj
	     :filtered-value
	     (formula `(gilt::Wrap-In-Error-Check ,expression)))
    (when (button-p obj)
      (let* ((agglist (g-value obj :parent))
	     (panel (g-value agglist :parent)))
	(s-value panel :filtered-value
		 (o-formula (let ((value-obj (gvl :value-obj)))
			      (when value-obj
				(gv value-obj :filtered-value)))))
	(s-value agglist :dump-children-as-parts T)))))

;; Strip-Error-Check removes the call to Wrap-In-Error-Check which is put
;; around a filter expression.  If the user already used Gilt to set the
;; :filtered-value of an object, then Gilt will have put a call to WIEC
;; around the expression that the user typed in.  We don't want to show
;; this function call in the filtered-value formula that we show to the
;; user.  So only show the 'cdr' of the expression if the call to WIEC is
;; there.  The call will be restored when the user hits "OK" on the value
;; control panel and Set-Filter-Slot is executed.
;;
(defun Strip-Error-Check (expr)
  (let ((fn (car expr)))
    (if (or (eq 'Wrap-In-Error-Check fn)
	    (eq 'gilt::Wrap-In-Error-Check fn))
	(second expr)
	expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used by the Value-Control OK-Apply-Cancel buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used as the :function-for-ok in the Value-Control menu
;;
(defun Value-Control-OK-Fn (vc values)
  (let ((expr (g-value vc :filter-expr)))
    (if expr
	(Set-Filter-Slot expr vc)))
  (if (string= "OK" (gilt:value-of :ok-apply-cancel values))
      (Close-VC-Win vc)))

;; Used as the :function-for-cancel in the Value-Control menu
;;
(defun Value-Control-Cancel-Fn (vc values)
  (declare (ignore values))
  (Close-VC-Win vc))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *Foreground-Color* (create-instance NIL opal:color
			     (:red .9) (:green .75) (:blue .75)))
(defvar *Foreground-Fill* (create-instance NIL opal:default-filling-style
			    (:foreground-color *Foreground-Color*)))

(defvar *Highlight-Color* (create-instance NIL opal:color
			    (:red .9)(:green .83)(:blue .83)))
(defvar *Highlight-Fill* (create-instance NIL opal:default-filling-style
			   (:foreground-color *Highlight-Color*)))


;; Filter-Text-Fn is the final function for the EDITABLE-TEXT object
;; in the value control panel.
;;
(defun Filter-Text-Fn (i o e string x y)
  (declare (ignore e x y))
  (s-value o :user-edited-p T)
  (s-value o :value (opal:text-to-string string))
  (let* ((vc (g-value i :operates-on :value-control))
	 (expr (g-value vc :filter-expr)))  ; Maintained by a formula
    (if expr
	(if (not (Get-Filters-For-Params expr vc))
	    (s-value vc :filter-expr NIL)))))


;; The text object that the user edits to create the filter expression
;; In this editable-text object, there are two strings.  One is in
;; the :strings slot, which is displayed and edited by the user.
;; The other is in the :value slot, which is set when the user has
;; finished editing the string in the :strings slot.  This allows
;; dependencies to be set up on the :value, which is always a
;; complete expression, versus the dynamic one in :strings.
(create-instance 'EDITABLE-TEXT opal:aggregadget
   (:parts
    `((:string ,opal:multifont-text
       (:left 5)(:top 5)
       (:scrolling-window ,(o-formula (gvl :parent :scrolling-window)))
       (:auto-scroll-p T)
;       (:fast-redraw-p :rectangle)
;       (:fast-redraw-filling-style
;	,(o-formula (gvl :parent :scrolling-window :foreground-fill)))
       )))
   (:interactors
    `((:edit-it ,inter:multifont-text-interactor
       (:lisp-mode-p T)
       (:match-parens-p T)
       (:start-where ,(o-formula (list :in (gvl :operates-on :string))))
       (:stop-event (:CONTROL-\j :CONTROL-J #\linefeed))
       (:window ,(o-formula (gvl :operates-on :window)))
       (:active ,(o-formula (gvl :operates-on :window :visible)))
       (:final-function ,(o-formula (gvl :operates-on :final-function)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level function to display a value-control panel for an object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *vc-position-list* NIL)
(defparameter *Used-VC-Wins* NIL)

;; These VC position functions are used to position the next value control
;; menu appropriately with respect to the menus that are already on the
;; screen.
;;
(defun Next-VC-Position-Index ()
  (if *vc-position-list* (1+ (reduce 'max *vc-position-list*)) 0))
(defun Next-VC-Left (i)
  (+ (g-value VALUE-CONTROL :window-left) (* 30 i)))
(defun Next-VC-Top (i)
  (+ (g-value VALUE-CONTROL :window-top) (* 30 i)))

(defun Close-VC-Win (vc)
  (inter:abort-interactor (g-value vc :filter-text-obj :parent :edit-it))
  (setf *vc-position-list* (remove (g-value vc :position-index)
				   *vc-position-list*))
  (push (g-value vc :window) *Used-VC-Wins*))

(defun Destroy-Filter-Wins ()
  (dolist (vc (g-value VALUE-CONTROL :is-a-inv))
    (opal:destroy (g-value vc :window)))
  (setf *Used-VC-Wins* NIL)
  (setf *vc-position-list* NIL))

(defun Set-VC-Win-Position (vc-win)
  (let ((vc (car (g-value vc-win :aggregate :components)))
	(i (next-vc-position-index)))
    (push i *vc-position-list*)
    (s-value vc-win :left (next-vc-left i))
    (s-value vc-win :top (next-vc-top i))
    (s-value vc :position-index i)))

(defun Get-Visible-VC-Win (for-obj)
  (do* ((vc-list (g-value VALUE-CONTROL :is-a-inv) (cdr vc-list))
	(vc (car vc-list) (car vc-list)))
       ((or (null vc-list) (eq for-obj (g-value vc :for-object)))
	(if vc
	    (let ((vc-win (g-value vc :window)))
	      ;; If we found a VC Panel for the object but it is not visible,
	      ;; then it must be on the *Used-VC-Wins* list, and should be
	      ;; retrieved using Pop-Used-VC-Win, not this function.
	      (when (g-value vc-win :visible)
		(setf *vc-position-list* (remove (g-value vc :position-index)
						 *vc-position-list*))
		;; Make vc-win invisible while changing position because
		;; of border-width problems when it is moved while visible
		(s-value vc-win :visible NIL)
		(opal:update vc-win)
		(Set-VC-Win-Position vc-win)
		(s-value vc-win :visible T)
		(opal:raise-window vc-win)
		(opal:update vc-win)
		vc))))))

(defun Pop-Used-VC-Win (for-obj title)
  (let ((vc-win (pop *Used-VC-Wins*)))
    (when vc-win
      (Set-VC-Win-Position vc-win)
      (s-value vc-win :title title)
      (s-value vc-win :visible T)
      (let ((vc (car (g-value vc-win :aggregate :components))))
	(s-value vc :for-object for-obj)
	(s-value (g-value vc :result) :real-edited-pairs NIL)
	(opal:update vc-win)
	vc))))


(defun Create-VC-Win (for-obj title)
  (let* ((i (next-vc-position-index))
	 (vc (create-instance NIL VALUE-CONTROL
		(:position-index i)
		(:for-object for-obj)
		(:window-title title)))
	 (vc-win (show-in-window vc (next-vc-left i) (next-vc-top i)))
	 (filter-win (Generate-Filter-Expr-Win vc vc-win))
	 (filter-str-agg (create-instance NIL EDITABLE-TEXT
			   (:value-control vc)
			   (:scrolling-window filter-win)
			   (:final-function #'Filter-Text-Fn)))
	 (filter-text-obj (g-value filter-str-agg :string)))
    (push i *vc-position-list*)
    (opal:update filter-win)
    (opal:add-component (g-value filter-win :inner-aggregate) filter-str-agg)
    (s-value vc :filter-text-obj filter-text-obj)
    (s-value vc :scrolling-window filter-win)
    vc))

;;   The for-obj parameter is the object that we will be creating a filter
;; function for.  The :trans-key is a keyword that corresponds to an initial
;; filter function to appear in the menu.
;;
;;   A filter function is a function that transforms the default value of
;; the object into the type we want (e.g., from a string to a keyword).
;;
(defun Show-Value-Control (for-obj &optional (trans-key :nothing)
				             (expected-type NIL))

  (or (Get-Visible-VC-Win for-obj)
  
  ;; Reuse or create a value control menu
  (let* ((title "Value Control")
	 ;(concatenate 'string "Value Control:  " (name-for-schema for-obj))
	 (vc (or (Pop-Used-VC-Win for-obj title)
		 (Create-VC-Win for-obj title)))
	 (filter-text-obj (g-value vc :filter-text-obj)))
    (s-value vc :expected-type (unless (eq :nothing expected-type)
				 expected-type))
    (gg:scroll-win-to (g-value vc :scrolling-window) 0 0)
    ;; Set the initial filter function string
    (let ((fv (get-value for-obj :filtered-value))
	  trans-string)
      (if fv
	  ;; We will use the formula already in the slot
	  (let ((expr (if (formula-p fv)
			  (kr::a-formula-lambda fv)
			  fv)))
	    (setq trans-string
		  (if (stringp expr) expr
		      (write-to-string (Strip-Error-Check expr)
				       :pretty T :case :downcase))))
	  ;; We have to guess a transformation based on the trans-key
	  (setq trans-string (Lookup-Trans trans-key)))
      (opal:set-text filter-text-obj trans-string)
      (s-value filter-text-obj :value trans-string))
    vc)))


(defun Value-Control-Func (&rest args)
  (declare (ignore args))
  (let* ((objs (g-value *Selection-Obj* :value))
	 (obj (car objs))
	 (found-vc NIL))
    (cond ((null objs) (Gilt-Error "Nothing selected"))
	  ((cdr objs) (Gilt-Error "Only one object can be selected"))
	  ((progn
	     (dolist (vc (g-value VALUE-CONTROL :is-a-inv))
	       (if (and (eq obj (g-value vc :for-object))
			(g-value vc :window :visible))
		   (setf found-vc vc)))
	     found-vc)
	   (Gilt-Error (format NIL "A value control panel is already
being used for ~S" obj)))
          (T (Show-Value-Control obj)))))




;;
;;  Motif Value Control objects
;;

(in-package "GILT")

(create-instance 'VALUE-CONTROL opal:aggregadget
  (:window-left 300) (:window-top 400)
  (:window-width 495) (:window-height 225)
  (:window-title "Value Control")
  (:window-background-color *Foreground-Color*)
  (:width (o-formula (gvl :window :width) 495))
  (:height (o-formula (gvl :window :height) 225))
  (:for-object NIL)
  (:slot-to-reference :filtered-value)
  (:type-error-p (o-formula
		  (let ((et (gvl :expected-type))
			(for-obj (gvl :for-object)))
		    (if et
			(if (listp et)
			    (Check-Each-Param et (gvl :filter-expr) for-obj)
			    (not (typep (gvl :filter-result) et)))))))
  (:filter-expr (o-formula (gg:careful-read-from-string
			    (gvl :filter-text-obj :value)
			    *error-gadget*)))
  (:filter-result (o-formula
		   (let ((for-obj (gvl :for-object))
			 (expr (gvl :filter-expr)))
		     (gg:Careful-Eval-Formula-Lambda
		      expr *error-gadget* NIL for-obj
		      :filter-result kr::*current-formula* NIL))))
  (:error-check-alist NIL)
  (:function-for-ok #'Value-Control-OK-Fn)
  (:function-for-cancel #'Value-Control-Cancel-Fn)
  (:parts `(
    (:title ,opal:text
      (:left 4) (:top 4)
      (:font ,(opal:get-standard-font NIL :bold-italic :large))
      (:string ,(o-formula (concatenate 'simple-string
			    "Exported Value Control for \""
			    (Get-User-Name (gvl :parent :for-object))
			    "\""))))
    (:current-value ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
      (:left 10) (:top 36) (:width 285)
      (:label-string "Unfiltered Value:")
      (:value ,(o-formula (prin1-to-string (gvl :parent :for-object :value))))
      (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
      (:parts (:label-text
	       (:field-text ,garnet-gadgets:scrolling-input-string
			    :inherit (:left :top :width :value :font)
			    (:interactors
			     ((:text-edit :omit)))))))
    (:ok-apply-cancel ,GARNET-GADGETS:MOTIF-TEXT-BUTTON-PANEL
      (:direction :horizontal)
      (:items ("OK" "Apply" "Cancel" ))
      (:final-feedback-p NIL)
      (:left 300) (:top 35)
      (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
      (:selection-function ,#'gilt:okcancel-function)
      (:interactors
       (:press (:key :omit))))
    (:filter-label ,OPAL:TEXT
      (:left 12) (:top 78)
      (:string "Filter Expression:")
      (:font ,(opal:get-standard-font NIL :bold NIL)))
    (:insert-ref ,GARNET-GADGETS:MOTIF-TEXT-BUTTON
      (:selection-function ,#'Insert-Ref-Obj-Into-Str)
      (:string "Use Value of Object")
      (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
      (:left ,(o-formula (- (opal:gv-right (gvl :parent :ok-apply-cancel))
			    (gvl :width))))
      (:top ,(o-formula (+ 5 (opal:gv-bottom
			      (gvl :parent :ok-apply-cancel)))))
      (:interactors
       (:press (:key :omit))))
    (:error-check-button ,GARNET-GADGETS:MOTIF-TEXT-BUTTON
      (:left ,(o-formula (- (opal:gv-right (gvl :parent :ok-apply-cancel))
			    (gvl :width))))
      (:top 193)
      (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
      (:string "Error Check")
      (:selection-function Show-Error-Check)
      (:interactors
       (:press (:key :omit))))
   (:warning ,OPAL:TEXT
     (:left 12) (:top 58)
     (:string "** Types do not match **")
     (:line-style ,opal:red-line)
     (:visible ,(o-formula (gvl :parent :type-error-p)))
     (:fast-redraw-p :rectangle)
     (:fast-redraw-filling-style ,*Foreground-Fill*))
   (:result ,GARNET-GADGETS:MOTIF-SCROLLING-LABELED-BOX
    (:left 12) (:top 195)
    (:width ,(o-formula (- 450 (gvl :parent :error-check-button :width))))
    (:label-string "Resulting Filtered Value:")
    (:foreground-color ,(o-formula (gvl :parent :window-background-color)))
    ;; This formula sets the :real-value of the result box to be the :value
    ;; of the object named by the control panel.  Then, if there is a
    ;; filter expression, the object's value will be filtered through it
    ;; and the resulting filtered value will be shown.
    (:value ,(o-formula
	      (let ((real-val (gvl :parent :for-object :value)))
		(s-value (gv :self) :real-value real-val)
		(prin1-to-string (gvl :parent :filter-result)))))
    (:selection-function Infer-Filter))
	    )))

(defun Generate-Filter-Expr-Win (vc vc-win)
  (create-instance NIL gg:motif-scrolling-window-with-bars
    (:parent-window vc-win)
    (:value-control vc)
    (:left 12) (:top 94) (:width 460) (:height 90)
    (:total-width 600)
    (:total-height (o-formula (+ 15 (gvl :value-control :filter-text-obj
					 :height)) 29))
    (:foreground-color *Highlight-Color*)
    (:foreground-fill *Highlight-Fill*)
    (:parts
     `((:v-scroll :modify
	(:interactors
	 (:slide :jump (:key :omit))))
       (:h-scroll :modify
	(:interactors
	 (:slide :jump (:key :omit))))))))
