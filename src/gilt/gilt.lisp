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
;;; $Id$
;;;


;;; Gilt is a simple interface builder for Garnet.  It lets a user
;;; construct a user interface by selecting gadgets in the gadget window
;;; and then drawing them in the work window.  The work window can be
;;; exercised and written out.
;;;
;;; Designed and implemented by Brad Myers

;;  **** BUGS:
;;  Opal: windows should appear in new place
;;  Gilt: Unset old name slot in aggregate when :known-as of a gadget changes
;;  Gilt: Align menu can't center both row and column
;;  Gilt: Should be easier to add the user's own gadgets
;;  Gilt: The dialog boxes that were created with Gilt (like line-props) should
;;    be split up into multiple code segments instead of one big c-i call.
;;    This will make them easier to compile.
;;  Gilt: Need to write out "in-package" info line as first executable instr.

;;**** In Read, save other parts of main aggregate, such as interactors


(in-package "GILT")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(Do-Go Do-Stop)))

(defparameter Gilt-Version "V3.0")

(defparameter *Run-Build-obj* NIL) ; the gadget that determines whether in
                                   ; build or run mode
(defparameter *Selection-obj* NIL) ; The gadget that is the user's selection in
				   ; the work window
(defparameter *Ib-Win* NIL) ; the window that the gadgets are in
(defparameter *garnet-Ib-Win* NIL)
(defparameter *motif-Ib-Win* NIL)
(defparameter *main-win* NIL) ; window containing the main menu and controls
(defparameter *objs-agg* NIL) ; aggregate containing created objects
(defparameter *top-agg* NIL)  ; top aggregate in the work window


(defparameter *Last-Filename* "") ; last file name used to read or save a file
(defparameter *Top-Gadget-Name* "TEMP-GADGET") ; name used for the top gadget
(defparameter *main-menu* NIL) ;; the main menubar

;; These are slots that should not be put into the file from any objects
(defparameter create-time-do-not-dump-slots
  (list :selected :value-obj :value :do-not-dump-slots
	:gg-interim-selected :gg-selected :internally-parented
	:parameters :*bar-item-popped-up))

(defparameter save-time-extra-do-not-dump-slots
  (list :point-to-leaf :select-function))

(defparameter common-lisp-user::*gilt-obj* NIL) ; global variable set with current
				    ; selection

(declaim (special save-file text-edit))

(defun Set-Up-Special-Slot-With-Value (obj temp-slot orig-slot)
  (let ((old-val (get-value obj orig-slot)))
    (if (formula-p old-val)
	(s-value obj temp-slot (formula old-val)) ; create an instance
        (s-value obj temp-slot old-val)))) ; otherwise, just use old value

(defun Save-Temp-Value (orig-obj orig-slot temp-obj temp-slot 
				  new-value-for-orig)
  (if (has-slot-p orig-obj orig-slot)
      (kr::move-formula orig-obj orig-slot temp-obj temp-slot)
      (s-value temp-obj temp-slot :*no-old-value*))
  (s-value orig-obj orig-slot new-value-for-orig))

(defun Restore-Temp-Value (orig-obj orig-slot temp-obj temp-slot
				    destroy-temp-p)
  (let ((old-val (get-local-value temp-obj temp-slot)))
    (if (eq old-val :*no-old-value*)
	(destroy-slot orig-obj orig-slot)
	(kr::move-formula temp-obj temp-slot orig-obj orig-slot))
    (mark-as-changed orig-obj orig-slot)
    (when destroy-temp-p
      (destroy-slot temp-obj temp-slot))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Menu Functions


(defun Make-Main-Menu ()
  (let* ((win (create-instance NIL inter:interactor-window
		 (:top 45)(:left 500)(:width 346)(:height 131)
		 (:title "Gilt Commands")
		 (:background-color wheat)))
	 (agg (create-instance NIL opal:aggregate
		 (:left 0) (:top 0) (:width 405) (:height 145)))
	 (obj (create-instance NIL garnet-gadgets:motif-radio-button-panel
		 (:constant '(T))
		 (:left 6)(:top 46)(:items '(:Build :Run))
		 (:text-on-left-p T)
		 (:foreground-color wheat)
		 (:selection-function #'(lambda (gad val)
					  (declare (ignore gad))
					  (if (eq val :Run)
					      (Garnet-Gadgets:Set-Selection
					       *Selection-Obj* NIL))))
		 (:interactors
		  `(:press
		    (:key :omit)))))
	 

	 (left-number (create-instance NIL number-enter-proto
	     (:constant '(T :EXCEPT :active-p :LABEL-STRING :FIELD-FONT))
	     (:foreground-color wheat)
	     (:left 93)(:top 44)
	     (:line-p (formula LinepForm))
	     (:Label-string (o-formula
			     (if (gvl :line-p) "    X1" "  LEFT") "  LEFT"))
	     (:slot (o-formula  (if (gvl :line-p) :x1 :left)))
	     (:selection-function 'LeftX1-Set-Func)))
	 (top-number (create-instance NIL number-enter-proto
		(:left 93)(:top 71)
		(:foreground-color wheat)
		(:constant '(T :EXCEPT :active-p :LABEL-STRING :FIELD-FONT))
		(:line-p (o-formula (gv left-number :line-p)))
		(:Label-string (o-formula
			   (if (gvl :line-p) "    Y1" "   TOP") "   TOP"))
		(:slot (o-formula (if (gvl :line-p) :y1 :top)))
		(:selection-function 'TopY1-Set-Func)))
	 (width-number (create-instance NIL number-enter-proto
		(:left 205)(:top 44)
		(:foreground-color wheat)
		(:constant '(T :EXCEPT :active-p :LABEL-STRING :FIELD-FONT))
		(:line-p (o-formula (gv left-number :line-p)))
		(:Label-string (o-formula
			    (if (gvl :line-p) "    X2" " WIDTH") " WIDTH"))
		(:slot (o-formula (if (gvl :line-p) :X2 :width)))
		(:selection-function 'WidthX2-Set-Func)))
	 (height-number (create-instance NIL number-enter-proto
		(:left 205)(:top 71)
		(:foreground-color wheat)
		(:line-p (o-formula (gv left-number :line-p)))
		(:constant '(T :EXCEPT :active-p :LABEL-STRING :FIELD-FONT))
		(:Label-string (o-formula
			    (if (gvl :line-p) "    Y2" "HEIGHT") "HEIGHT"))
		(:slot (o-formula (if (gvl :line-p) :y2 :height)))
		(:selection-function 'HeightY2-Set-Func)))
	 (selected (create-instance NIL opal:aggregadget
		      (:left 4) (:top 114)
		      (:parts
		       `((:label ,opal:text (:string "Selected Object: ")
			  (:constant (T))
			  (:left ,(o-formula (gvl :parent :left)))
			  (:top ,(o-formula (gvl :parent :top)))
			  (:font ,(g-value gg:motif-scrolling-labeled-box :label-font)))
			 (:value ,opal:text
			  (:constant (T :except :string))
			  (:fast-redraw-p :rectangle)
			  (:fast-redraw-filling-style ,wheat-fill)
			  (:left ,(o-formula (+ 3 (opal:gv-right
						   (gvl :parent :label)))))
			  (:top ,(o-formula (gvl :parent :top)))
			  (:string
			   ,(o-formula
			     (let ((objs (gv *selection-obj* :value)) obj)
			       (cond
				 ((cdr objs) "<multiple>")
				 ((setq obj (car objs))
				  (let ((kr::*print-as-structure* NIL))
				    (format NIL "~s" obj)))
				 (T "<none>")))))
			  ))))))

    (setq *Run-Build-Obj* obj)

    (let ((menu (create-instance NIL gg:motif-menubar
		 (:constant '(T :except :active-p))
		 (:left 1)(:top 1)
		 (:active-p (o-formula
			     (if (schema-p *Run-Build-Obj*)
				 (eq :build (gv *Run-Build-Obj* :value)))))
		 (:foreground-color wheat)
		 (:min-menubar-width (o-formula (- (gv win :width) 2)))
		 (:accelerator-windows (list *work-win*)) ; NOT in cmd win
		 (:items `(("File" NIL
			    (("Open..." Show-Read-Dialog)
			     ("New" gg:standard-delete-all)
			     ("Save As..." Show-Save-Dialog)
			     ("Load Other Gadgets" Load-Other-Gadgets)
			     ("Refresh" gg:standard-refresh)
			     ("Quit" Quit-Func)))
			   ("Edit" NIL
			    (("Cut" gg:standard-cut)
			     ("Copy" gg:standard-copy)
			     ("Paste" gg:standard-paste-inc-place)
			     ("Duplicate" gg:standard-duplicate)
			     ("Delete" gg:standard-delete)
			     ("Delete All" gg:standard-delete-all)
			     ("Undo Last Delete" gg:Standard-Undo-Last-Delete)
			     ("Select All" gg:Standard-Select-All)
			     ("To Top" gg:standard-to-top)
			     ("To Bottom" gg:standard-to-bottom)
			     ("Properties..." Properties-Func)
			     ("Align" Align-Func)
			     ))
			   ("Control" NIL
			    (("Value Control" Value-Control-Func)
			     ("Enable Control" Enable-Control-Func)
			     ("Other Control..." gg:Standard-NIY)
			     ))))
		 (:accelerators
		  `((("^o" :|CONTROL-o|) ("^n" :|CONTROL-n|)
		     ("^s" :|CONTROL-s|) NIL ("^l" :|CONTROL-l|)
		     NIL)
		   (("^x" :|CONTROL-x|) ("^c" :|CONTROL-c|)
		    ("^v" :|CONTROL-v|) ("^d" :|CONTROL-d|)
		    ("DEL" #\rubout) NIL
		    ("^z" :|CONTROL-z|) ("^*" :|CONTROL-*|)
		    ("^t" :|CONTROL-t|) ("^b" :|CONTROL-b|)
		    ("^p" :|CONTROL-p|) ("^a" :|CONTROL-a|))
		   (NIL NIL NIL)))
		 (:bar-above-these-items
		  `(("Quit")
		   ("Delete" "Select All" "To Top" "Properties...")
		   NIL))
		)))
  
    (opal:update win)  ;;** bug in menubar need update first before add **
    (Init-value obj :build) ; start in Build mode
    (setq *main-win* win)
    (s-value win :aggregate agg)
    (opal:update win)  ;;** bug in menubar need update first before add **
    (opal:add-components agg obj menu left-number top-number
			 width-number height-number selected)
    (opal:update win)
    (gg:Standard-Initialize-Gadget menu *selection-obj* *objs-agg*
				   :undo-delete? T)
    (setq *main-menu* menu)
    )))


;;;
;;;
(defun Load-File-Name (item)
  (let ((file-name (cadr (assoc item *load-file* :test #'string=))))
    (if file-name
	file-name
	item)))

(defun Generate-Uses-List ()
  (let (gadgets gadgets-to-load)
    (dolist (obj (g-value *objs-agg* :components))
      (pushnew (car (g-value obj :is-a)) gadgets))
    (Format T ";;; This file uses the following objects:~%")
    (dolist (gad gadgets)
      (let ((item (name-for-schema gad))
	    (pack (package-name (symbol-package (kr::schema-name gad)))))
	(format T ";;;     ~a from package ~a~%" item pack)
	(if (equalp pack "GARNET-GADGETS")
	    (unless (or (gg:is-a-motif-background gad)
			(gg:is-a-motif-rect gad))
	      (pushnew (string-downcase (Load-File-Name item)) gadgets-to-load)))))
    (when gadgets-to-load
      (format T "(eval-when (:compile-toplevel :load-toplevel :execute)~%")
      (format T "  (dolist (gadget '(")
      (dolist (gad gadgets-to-load)
	(format T "\"~a-loader\"~%		    " gad))
      (format T "))~%    (garnet-load (concatenate 'string \"gadgets:\" gadget))))~%"))

    (format T ";;;~%")
    (when (member opal:multifont-text gadgets)
      (format T "(garnet-load \"opal:multifont-loader\")~%"))

    (format T ";;;~%")
    (format T ";;;     Functions needed from Gilt~%")
    (format T "(dolist (file '(\"gilt-functions-loader\"
		\"filter-functions-loader\"))
  (garnet-load (concatenate 'string \"gilt:\" file)))~%")
    (format T ";;;~%")))

(defun Write-Standard-Header (package)
  (format T ";;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ~a; Base: 10 -*-~%"
	  package)
  (format T ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
  (format T ";;; This file created by GILT ~a: The Garnet Interface Builder~%" 
	  Gilt-Version)
  (format T ";;; on ~a~%" (inter::time-to-string))
  (format T ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
  (format T "(in-package ~s)~%~%" package)
  (format T "(defparameter common-lisp-user::*Used-Gilt-Version* ~s)~%~%" Gilt-Version)
  (Generate-Uses-List)
  (format T ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%~%")
  )

;; determine if the OK function should be asked for.  Return T if gray
;; rectangle should be visible (so no OK button)

(defun Check-Ask-OK ()
  (let (have-ok)
    (setq have-ok (dolist (obj (g-value *objs-agg* :components))
		    (when (g-value obj :Ok-Cancel-p)
		      (return T))))
    (not have-ok)))


;;; See if the string mentions a package, and if not, put it in the
;;; specified package.  If so, put it in that package (creating that
;;; package first if it does not exist--so no error).  Returns the new symbol.
(defun Check-Atom-In-Package (str package)
  (let ((pos (position #\: str)))
    (setq str (string-upcase str))
    (if pos
	(let ((pack (subseq str 0 pos)))
	  (unless (find-package pack)
	    (make-package pack))
	  (intern (subseq str (1+ pos)) pack))
	;; else no colon; put it into package
	(if (= 0 (length str))  ; use NIL for the empty string
	    NIL
	    (intern str package)))))

;;; Always intern the str in the specified package, even if a package name
;;; is specified.
(defun Move-To-Package (str package)
  (let ((pos (position #\: str)))
    (setq str (string-upcase str))
    (if pos
	(intern (subseq str (1+ pos)) package)
	(intern str package))))

;;;Searches through all objects and if there is a :select-function
;;; slot, moves it into the :selection-function slot.   Do this before the
;;; gadgets are written out
(defun Set-Selection-Functions (package)
  (let (sel-func)
    (dovalues (each-gadget *objs-agg* :components)
	      (when (setq sel-func (g-value each-gadget :select-function))
		
		(cond ((stringp sel-func) ; then package probably not defined
		       (setq sel-func (Check-Atom-In-Package sel-func package))
		       (s-value each-gadget :select-function sel-func))
		      ;; this next clause turns out to be a bad idea, since
		      ;; can't tell if the user explicitly specified a
		      ;; package, or if one just appeared.  Also, the
		      ;; okcancel function is in gilt.
		      ;;((and package
		      ;;    (symbolp sel-func)
		      ;;    (eq (symbol-package sel-func)
		      ;;	(find-package 'user)))
		      ;; ;; then move to the specified package
		      ;;(setq sel-func (intern (symbol-name sel-func) package))
		      ;; (s-value each-gadget :select-function sel-func))
		      (t NIL))
		(s-value each-gadget :selection-function sel-func)))))

(defparameter keywordpackage (find-package 'keyword))

;;;Searches through all objects and makes sure the known-as slots are
;;; filled with keywords, not atoms or strings.
(defun Fix-Known-As ()
  (let (knownas)
    (dovalues (each-gadget *objs-agg* :components)
	      (when (setq knownas (g-local-value each-gadget :known-as))
		(cond ((stringp knownas) ; then package probably not defined
		       (setq knownas (Move-To-Package knownas keywordpackage))
		       (with-constants-disabled
			   (s-value each-gadget :known-as knownas)))
		      ((symbolp knownas) ;; then move to the keyword package
		       (unless (eq keywordpackage (symbol-package knownas)) 
			 (setq knownas (intern (symbol-name knownas) keywordpackage))
			 (with-constants-disabled
			     (s-value each-gadget :known-as knownas))))
		      (t NIL))))))

;;;Sets the do-not-dump slot of all objects correctly for before saving
(defun Set-Do-Not-Dump-Slot-For-Save ()
  (dolist (each-gadget (g-value *objs-agg* :components))
    (s-value each-gadget :do-not-dump-slots 
	     (append save-time-extra-do-not-dump-slots
		     (g-value each-gadget :do-not-dump-slots)))))

;;;Removes the extra values from do-not-dump-slots
(defun Set-Do-Not-Dump-Slot-After-Save ()
  (dolist (each-gadget (g-value *objs-agg* :components))
    (s-value each-gadget :do-not-dump-slots 
	     (set-difference (g-value each-gadget :do-not-dump-slots)
			     save-time-extra-do-not-dump-slots))))

;;;Searches through all objects and if there is a :selection-function
;;; slot, then removes the value from the :selection function slot and puts
;;; it into the :select-function slot.   Do this after the
;;; gadgets are read back in. 
(defun Remove-Selection-Functions ()
  (let (sel-func)
    (dovalues (each-gadget *objs-agg* :components)
	      (when (setq sel-func (g-value each-gadget :selection-function))
		(s-value each-gadget :selection-function NIL)
		(s-value each-gadget :select-function sel-func)))))


;;; This is called by the Save-file dialog box when OK is hit.  Values is a
;;; list of the gadgets and their values
(defun Do-Save-File (gadget values)
  (declare (ignore gadget))
  (let ((filename (value-of :filename values))
	(gadget-name (value-of :gadget-name values))
	(window-title (value-of :win-title values))
	(package (string-upcase (value-of :package-name values)))
	(function-for-ok-name (value-of :FUNCTION-FOR-OK-NAME values))
	(export-p (if (value-of :export-p values)
		      T NIL))) ;use T instead of string name
  (cond ((string= "" filename) (Gilt-Error "Filename must be supplied"))
	((string= "" gadget-name) (Gilt-Error "Gadget name must be supplied"))
	((string= "" package) (Gilt-Error "Package name must be supplied"))
	((gu:probe-directory (directory-namestring filename))
	   (format T "Saving current work window to file ~s...~%" filename)
	   (setq *Last-Filename* filename)
	   (with-open-file (*standard-output* filename :direction :output
					      :if-exists :supersede)
	     (unless (find-package package)
	       (make-package package))
	     ;; first set up objects so they can be dumped successfully
	     (Set-Selection-Functions package)
	     (Fix-Known-As)
	     (Set-Do-Not-Dump-Slot-For-Save)
	     ;; now start dumping
	     (write-standard-header package)
	     (setf (kr::schema-name *objs-agg*)
		   (read-from-string (string-upcase gadget-name)))
	     (when export-p
	       (Format T "~%(export '(~a))~%~%" (kr::schema-name *objs-agg*)))
	     (s-value *objs-agg* :function-for-ok
		      (Check-Atom-In-Package function-for-ok-name package))
	     (s-value *objs-agg* :package-name package)
	     (s-value *objs-agg* :window-height (g-value *work-win* :height))
	     (s-value *objs-agg* :window-width (g-value *work-win* :width))
	     (s-value *objs-agg* :window-top (g-value *work-win* :top))
	     (s-value *objs-agg* :window-left (g-value *work-win* :left))
	     (s-value *objs-agg* :window-title window-title)
	     (s-value *objs-agg* :export-p export-p)
	     (Format T "(defparameter common-lisp-user::*Garnet-Object-Just-Created* ~%")
	     (opal:write-gadget *objs-agg* T T)
	     (Format T ")~%~%"))
	   ;; finished with stuff that goes to the file
	   ;; now clean up from save
	   (Remove-Selection-Functions)
	   (Set-Do-Not-Dump-Slot-After-Save)
	   (format T "...Done saving file~%"))
	(T (Gilt-Error "Cannot save to that file")))))

(declaim (special common-lisp-user::*Garnet-Object-Just-Created*))

;;; This is called by the Read-file dialog box when OK is hit.  Values is a
;;; list of the gadgets and their values
(defun Do-Read-File (gadget values)
  (declare (ignore gadget))
  (let ((filename (value-of :filename values))
	(addp (and (g-value *objs-agg* :components)
		   (string= (value-of :add-replace values)
			    "Add to existing objects")))
	new-obj-list)
  (cond ((string= "" filename) (Gilt-Error "Filename must be supplied"))
	((probe-file filename)
	 (format T "Loading work window from file ~s...~%" filename)
	 (opal:With-HourGlass-Cursor
	   (with-constants-disabled
	     (Load filename))
	   (format T "Restoring objects...~%")
	   (let ((win-width (g-value common-lisp-user::*Garnet-Object-Just-Created*
				     :window-width))
		 (win-height (g-value common-lisp-user::*Garnet-Object-Just-Created*
				      :window-width)))
	     (when win-width
	       (s-value *work-win* :width
			(if addp		; if adding objects, then wider
			    (max win-width (g-value *work-win* :width))
			    ;; otherwise, size specified in the file
			    win-width)))
	     (when win-height
	       (s-value *work-win* :height
			(if addp	; if adding objects, then taller
			    (max win-height (g-value *work-win* :height))
			    ;; otherwise, size specified in the file
			    win-height))))
	   (setq *Last-Filename* filename)
	   (setq *Top-Gadget-Name*
		 (or (name-for-schema common-lisp-user::*Garnet-Object-Just-Created*)
		     *Top-Gadget-Name*))
	   ;; saved as one big aggregadget
	   (unless addp 
	     ;; delete all old objects
	     (gg:standard-Delete-All *main-menu* NIL)
	   
	     ;; if not add, then replace, so use new file's values
	     (s-value *objs-agg* :function-for-ok
		      (g-value common-lisp-user::*Garnet-Object-Just-Created*
			       :function-for-ok))
	     (s-value *objs-agg* :package-name
		      (or (g-value common-lisp-user::*Garnet-Object-Just-Created*
				   :package-name)
			  "COMMON-LISP-USER"))
	     (s-value *objs-agg* :window-title
		      (or (g-value common-lisp-user::*Garnet-Object-Just-Created*
				   :window-title)
			  "TEMP WINDOW"))
	     (s-value *objs-agg* :export-p
		      (g-value common-lisp-user::*Garnet-Object-Just-Created* :export-p))
	     (let ((links (g-value common-lisp-user::*Garnet-Object-Just-Created* :links)))
	       (s-value *objs-agg* :links links)
	       (dolist (link links)
		 (let ((val (get-value common-lisp-user::*Garnet-Object-Just-Created*
				       link)))
		   (if (formula-p val)
		       (s-value *objs-agg* link (kr::copy-formula val))
		       (s-value *objs-agg* link val)))))
	     )
	   ;; now add all objects
	   (dolist (obj (setq new-obj-list
			 (copy-list (g-value common-lisp-user::*Garnet-Object-Just-Created*
					     :components))))
	     ;; The user's aggregadget was created with a :parts list, so its
	     ;; :components slot is constant.
	     (with-constants-disabled
	       (opal:remove-component common-lisp-user::*Garnet-Object-Just-Created* obj))
	     (s-value obj :do-not-dump-slots 
		      (append create-time-do-not-dump-slots
			      (g-value obj :do-not-dump-slots)))
	     (with-constants-disabled ; since adding to an aggregadget
	       (opal:add-component *objs-agg* obj
				   (when (gg:is-a-motif-background obj)
				     :back))))

	   ;; now clean up
	   (Garnet-Gadgets:Set-Selection *Selection-Obj* new-obj-list)
	   (Remove-Selection-Functions)
	   ) ; Close opal:With-HourGlass-Cursor
	 (format T "...Done~%"))
	(T (Gilt-Error "That file does not exist")))))


(defun Prop-Sheet-Finish (prop-sheet)
  (let* ((obj (g-value prop-sheet :obj))
	 (changed-values (g-value prop-sheet :changed-values))
	 ;; (gilt-type (g-value obj :gilt-type))
	 ;; (aggrel-slot (g-value gilt-type :aggrelist-slots)) *BAM*not needed
	 known-as)

    
    
    ;; If the :items slot of an aggrelist changed, update the work window
    ;; immediately so that the components and :items are rendered consistent
 ;;*BAM* Not Needed? (when (and aggrel-slot 
 ;;	       (assoc (setq slot (car aggrel-slot)) changed-values))
 ;;   (opal:update *work-win*))
    
    ;; If the :known-as slot of an object changed, set the corresponding
    ;; name slot in the top-level gadget
    ;; ** BUG **  Should unset old name slot!
    (when (setq known-as (second (assoc :known-as changed-values)))
      (s-value *objs-agg* known-as obj))))

(defparameter opal-package (find-package 'opal))
(defparameter gg-package (find-package 'gg))

(defun Get-Nice-Name (obj-or-objs)
  (if (and (listp obj-or-objs)
	   (cdr obj-or-objs))
      "<Multiple>"
      (let ((obj (if (listp obj-or-objs)(car obj-or-objs) obj-or-objs)))
	;; KR won't generate the object name unless it is printed first.
	(format NIL "~s" obj) 
	;; loop to get to a nice package
	(let (pack)
	  (loop
	   (setq pack (symbol-package (kr::schema-name obj)))
	   (when (or (eq pack opal-package)
		     (eq pack gg-package))
	     (return))
	   (setq obj (car (g-value obj :is-a)))
	   (when (null obj)
	     (error "no is-a")))
	  ;; here, obj is in a nice package
	  (substitute #\space #\- (symbol-name (kr::schema-name obj)))))))

(defun PopUpPropsWin (obj-or-objs)
  (let ((obj (if (listp obj-or-objs)
		 (car obj-or-objs)
		 obj-or-objs))
	left top)
    (multiple-value-setq (left top)
      (opal:convert-coordinates (g-value obj :window)
			 (g-value obj :left)
			 (opal:bottom obj) NIL))
    (setq top (+ 40 top))
    (Garnet-gadgets:pop-up-win-change-obj *prop-sheet* obj-or-objs NIL 
					  left top
					  (get-nice-name obj-or-objs))))

(defun Properties-Func (&rest args)
  (declare (ignore args))
  (let ((objs (g-value *Selection-Obj* :value)))
    (cond ((null objs) (Gilt-Error "Nothing selected"))
	  ((and (g-value *prop-sheet* :window)
		(g-value *prop-sheet* :window :visible))
	   (Gilt-Error (format NIL "Property sheet already being used for ~s"
			       (g-value *prop-sheet* :obj))))
	  (t (PopUpPropsWin objs)))))

(defun Quit-Func (&rest args)
  (declare (ignore args))
  (Do-Stop)
  #-cmu (inter:exit-main-event-loop))


(defun Load-Other-Gadgets (&rest args)
  (declare (ignore args))
  (s-value *ib-win* :visible NIL) ; get rid of the other window
  (cond ((eq *ib-win* *motif-Ib-Win*) (Make-Garnet-Palette-Window))
	((eq *ib-win* *garnet-Ib-Win*) (Make-Motif-Palette-Window))
	(T (error "don't have either ib-win displayed"))))


; convert s to an integer or return NIL
(defun Make-Integer (s)
  (let* ((sym (read-from-string s))
	 (number (when (integerp sym) sym)))
    number))


(defun Val-Set-Func (gadget new-val-string line-p slot1 slot2 indx)
  (let ((objs (g-value *Selection-Obj* :value))
	(slot (if line-p slot1 slot2))
	(boxpoints (if line-p :points :box))
	new-val found-one)
    (when (and (car objs)	; at least one selection
	       (setq new-val (make-integer new-val-string)))
      (dolist (obj objs)
	(when (and (member slot (g-value obj :parameters))
		   (g-value obj slot))	; there is a value there
	  (setq found-one T)
	  (setf (nth indx (g-value obj boxpoints)) new-val)
	  (mark-as-changed obj boxpoints))))
    (unless found-one
      (let ((inter (g-value gadget :TEXT-INTER)))
	(inter:beep)
					; go back to original value
	(s-value gadget :value (g-value inter :original-string))
	(inter:abort-interactor inter)))))

;; Gadget is the number entry object, not the object to be operated on
(defun LeftX1-Set-Func (gadget new-val-string)
  (val-set-func gadget new-val-string (g-value gadget :line-p)
					  :x1 :left 0))
(defun TopY1-Set-Func (gadget new-val-string)
  (val-set-func gadget new-val-string (g-value gadget :line-p)
					  :y1 :top 1))
(defun WidthX2-Set-Func (gadget new-val-string)
  (val-set-func gadget new-val-string (g-value gadget :line-p)
					  :x2 :width 2))
(defun HeightY2-Set-Func (gadget new-val-string)
  (val-set-func gadget new-val-string (g-value gadget :line-p)
					  :y2 :height 3))


;; if point-list is a single number, then the existing pointlist is
;; incremented by that amount (this is used when an object is duplicated),
;; otherwise the point-list is copied and then used.
;; Returns the new object
(defun Create-New-Gadget (gadget point-list)
  (let ((init (g-value gadget :maker))
	 newobj slot points)
    (if init
	(let ((loaded (g-value gadget :loaded)))
	  (unless (or (eq T loaded) (get :garnet-modules loaded))
	    ;; then it is a bitmap pretending to be an object.
	    (opal:With-HourGlass-Cursor
              (common-lisp-user::garnet-load (concatenate 'string "gadgets:"
                                              (g-value gadget :load-file)))))
	  (setq newobj (with-constants-disabled (eval (first init))))
	  (if (second init) (Init-Value newobj (second init))))
	(progn ; else not a bitmap of an object
	  (setq newobj
		(with-constants-disabled (opal:copy-gadget gadget NIL)))
	  (s-value newobj :known-as NIL)))
    (setq slot (if (g-value gadget :line-p) :points :box))
    (if (numberp point-list)
	; then copy old and increment appropriately
	(progn (setq points (copy-list (g-value newobj slot)))
	  (incf (first points) point-list)
	  (incf (second points) point-list)
	  (when (g-value gadget :line-p)
	    (incf (third points) point-list)
	    (incf (fourth points) point-list)))
	; else just use a copy of the parameter
	(setq points (copy-list point-list)))
    (s-value newobj slot points)
    (s-value newobj :do-not-dump-slots 
	     (append create-time-do-not-dump-slots
		     (g-value newobj :do-not-dump-slots)))
    (opal:add-component *objs-agg* newobj
       (when (gg:is-a-motif-background newobj)
	 :back))
    newobj))

(defun Move-Feedback-To-Correct-Window (feedback obj)
  (let ((new-win (g-value obj :window)))
    (unless (eq new-win (g-value feedback :window))
      (with-constants-disabled
	  (let ((old-agg (g-value feedback :parent)))
	    (when old-agg
	      (opal:remove-component old-agg feedback))
	    (if (eq new-win *work-win*)
		(opal:add-component (g-value *work-win* :aggregate)
				    feedback :behind *Selection-Obj*)
		;; else a random pop-up window
		(opal:add-component (g-value new-win :aggregate)
				    feedback)))))))

(defun work-win-interactors (work-win)
  (opal:add-components (g-value work-win :aggregate) *Selection-Obj*)
  ;; interactor to create new objects
  (create-instance 'creator inter:two-point-interactor
	(:window work-win)
	(:start-event :rightdown)
	(:start-where T)
	(:abort-if-too-small NIL)
	(:min-width (o-formula (let ((min-width (gvl :window :current-gadget
						     :min-width)))
				 (if min-width min-width 3))))
	(:min-height (o-formula (let ((min-height (gvl :window :current-gadget
						       :min-height)))
				  (if min-height min-height 3))))
	(:feedback-obj
	 ;;use the feedback objects in the graphics-selection object
	 ;;pick which feedback depending on whether drawing line or box
	 (o-formula
	  (if (gvl :line-p)
	      (gv *Selection-Obj* :line-movegrow-feedback)
	      (gv *Selection-Obj* :rect-movegrow-feedback))))
	(:line-p (o-formula (gvl :window :current-gadget :line-p)))
	;; active if in :build mode
	(:active (formula BuildGadgetActiveForm))
	(:final-function
	 #'(lambda (an-interactor point-list)
	     (when point-list
	       (let ((gadget (g-value an-interactor :window :current-gadget)))
		 (if gadget 
		     (Create-New-Gadget gadget point-list)
		     ; else no gadget
		     (inter:beep)))))))
  ;; interactor to edit the strings in objects
  (create-instance 'text-edit inter:text-interactor
	 (:extra-window NIL) ; popped-up window added to this list
	 (:window (o-formula (let ((other-win (gvl :extra-window)))
			       (if other-win
				   (list *work-win* other-win)
				   *work-win*))))
	   ;; higher priority so this one will go instead of the selection
	 (:waiting-priority *Higher-than-Selection-Priority-Level*)
	 (:running-priority *Higher-than-Selection-Priority-Level*)
	 (:start-event :leftdown)
	 (:stop-event '(#\return :control-\n :control-\j))
	 (:selection-obj *Selection-Obj*)
	 ;; active if in :build mode
	 (:active (o-formula (and (gvl :selection-obj :value)
				  (eq :build (gv *Run-Build-Obj* :value)))))
	 (:obj-list (o-formula (let ((extra-win (gvl :extra-window))
				     (sel-list (gv *Selection-Obj* :value)))
				 (if extra-win 
				     (append sel-list
					     (g-value extra-win :aggregate
						      :components))
				     sel-list))))
	 (:start-where (o-formula (list :list-leaf-element-of
					(gv :SELF) :obj-list
					:type opal:text)))
	 (:final-function
	  #'(lambda (inter obj final-event final-string x y)
	      (declare (ignore x y))
	      (Set-Item-Slot-Appropriately obj inter final-event 
					   final-string))))
  (create-instance 'mf-text-edit inter:multifont-text-interactor
	 (:window (o-formula (gv text-edit :window)))
	   ;; higher priority so this one will go instead of the selection
	 (:waiting-priority *Higher-than-Selection-Priority-Level*)
	 (:running-priority *Higher-than-Selection-Priority-Level*)
	 (:start-event :leftdown)
	 (:stop-event '(:control-\n :control-\j))
	 (:selection-obj *Selection-Obj*)
	 ;; active if in :build mode
	 (:active (o-formula (and (gvl :selection-obj :value)
				  (eq :build (gv *Run-Build-Obj* :value)))))
	 (:start-where (o-formula (list :list-leaf-element-of
					(gvl :selection-obj) :value
					:type opal:multifont-text)))
	 (:feedback-obj NIL))
  (create-instance 'popup-sub-obj-if-should inter:button-interactor
	 (:window work-win)
	 ;; Higher priority so this one will go instead of the selection.
	 ;; Both the text editing and this one might go at the same time.
	 (:waiting-priority *Higher-Than-Text-Edit-Level*)
	 (:running-priority *Higher-Than-Text-Edit-Level*)
	 (:start-event :leftdown)
	 (:selection-obj *Selection-Obj*)
	 (:continuous NIL)
	 ;; active if in :build mode
	 (:active (o-formula (and (gvl :selection-obj :value)
				  (eq :build (gv *Run-Build-Obj* :value)))))
	 (:start-where (o-formula (list :list-element-of
					(gvl :selection-obj) :value)))
	 (:do-start #'(lambda (inter obj ev)
			(call-prototype-method inter obj ev)
			(if (Pop-Up-Sub-Objs-If-Should obj)
			    T
			    :stop)))) ; return the special value :stop so
					; this interactor doesn't stop others
					; from running.
    )

;;; Find a member of the selection set which is or contains text-obj
(defun Find-Top-Obj (text-obj)
  (let ((selected (g-value *selection-obj* :value))
	(obj text-obj)
	obj1)
    (if (setq obj1 (g-value text-obj :window :*pop-up-window-from*))
	;; first check if special popup object
	obj1
	;; else loop up looking for a selected parent
	(loop
	 (if (member obj selected)
	     (return obj)
	     ;; else go to parent
	     (unless (setq obj (g-value obj :parent))
	       ;; reached top, didn't find anything
	       (return NIL)))))))

;; Called from the final function of the text interactor that edits strings
;; to cause the appropriate string slot to be set.
(defun Set-Item-Slot-Appropriately (obj inter final-event final-string)
  (declare (ignore inter))
  (let* ((top-obj (Find-Top-Obj obj))
	 (string-set-func (when top-obj
			    (g-value top-obj :String-Set-Func))))
    (unless 
	(and string-set-func
	     (funcall
	      string-set-func top-obj obj final-event final-string))
      ;; error- cannot edit that string
      (Gilt-Error
"You cannot edit that string directly.
Please use the dialog box that pops up when
you give the 'Properties' command or go into Run mode."))))
  
(defun Create-Selection-Obj ()
  (setq *Selection-Obj*
	(create-instance NIL garnet-gadgets:multi-graphics-selection
	  (:start-where `(:element-of-or-none ,*objs-agg*))
	  (:running-where `(:in ,*top-agg*))
	  (:check-grow-p T)   ; only some objects can be grown
	  (:check-move-p NIL) ; all objects can be moved
	  (:check-line T) ; check the :line-p slot of objects
	  (:allow-component-select T)
	  (:selection-function
	   #'(lambda (gadget newselection)
	       (declare (ignore gadget))
	       (Clean-up-popups)
	       (when newselection
		 (if (cdr newselection)
		     (setq common-lisp-user::*gilt-obj* newselection)
		     (setq common-lisp-user::*gilt-obj* (car newselection))))))
	  (:interactors
	   `((:select-it :modify
	      (:waiting-priority ,*Selection-Priority-Level*)
	      (:running-priority ,*Selection-Priority-Level*))
	     (:select-in-region :modify
	      (:waiting-priority ,*Selection-Priority-Level*)
	      (:running-priority ,*Selection-Priority-Level*))
	     (:down-level-select-it :modify
	      (:waiting-priority ,*Selection-Priority-Level*)
	      (:running-priority ,*Selection-Priority-Level*))
	     (:move-grow-it :modify
	      (:waiting-priority ,*Selection-Move-Grow-Priority-Level*)
	      (:running-priority ,*Selection-Move-Grow-Priority-Level*))
	     (:grow-multiple :modify
	      (:waiting-priority ,*Selection-Grow-Multiple-Priority-Level*)
	      (:running-priority ,*Selection-Grow-Multiple-Priority-Level*))
	     ))))
  )


(defun do-go (&optional gadget-set)
  "Start Gilt.  Gadget-set must be one of :garnet or :motif"

  (unless (or (eq gadget-set :motif)
	      (eq gadget-set :garnet))
    (error "Gilt:Do-go must be passed the gadget set to use: :motif or :garnet"))
  
  (setq *work-win* (create-instance NIL inter:interactor-window
		      (:title "Gilt Work Window")
		      (:left 0)(:top 45)(:width 450)(:height 300)
		      (:current-gadget (o-formula (gvl :ib-win :aggregate
					       :feedback :obj-over)))
		      (:aggregate
		       (setq *top-agg*
			  (create-instance NIL opal:aggregate
			     (:left 0)(:top 0)
			     (:width (o-formula (gvl :window :width)))
			     (:height (o-formula (gvl :window :height))))))))
  (s-value *work-win* :objs-agg
	   (setq *objs-agg*
		 (create-instance NIL opal:aggregadget
				    (:left 0)(:top 0)
				    (:width (o-formula (gvl :window :width)))
				    (:height (o-formula (gvl :window :height)))
				    ;; initial values for the Save Dialog box
				    (:package-name "COMMON-LISP-USER")
				    (:window-title "TEMP WINDOW")
				    (:export-p T)
				    (:FUNCTION-FOR-OK NIL)
				    )))

  (s-value *objs-agg* :do-not-dump-slots
	   (append (list :selected :gg-selected
			 :do-not-dump-objects :do-not-dump-slots)
		   (g-value *objs-agg* :do-not-dump-slots)))

  (Create-Selection-Obj)
  (Make-Main-Menu) ; this uses *Selection-Obj* in formulas
  (s-value *Selection-Obj* :active-p
	   (o-formula (eq :build (gv *Run-Build-Obj* :value))))

  (ecase gadget-set
    (:motif (Make-Motif-Palette-Window))
    (:garnet (Make-Garnet-Palette-Window)))

  (opal:add-component *top-agg* *objs-agg*)
  (opal:update *work-win*)
  (work-win-interactors *work-win*)
  (opal:update *ib-win*)
  (opal:update *work-win*)
  (setq *Error-Gadget* (Make-Error-Gadget *work-win*))
  (make-prop-sheet *error-gadget*)
  *work-win* ; return work-win
  ;;if not CMU CommonLisp, then start the main event loop to look for events
  #-cmu (inter:main-event-loop)
  )
  
(defmacro careful-delete (obj)
  `(when (and (boundp ',obj) (schema-p ,obj))
    (opal:destroy ,obj)))

(defun do-stop ()
  (when *garnet-ib-win*
    (opal:destroy *garnet-ib-win*))
  (when *motif-ib-win*
    (opal:destroy *motif-ib-win*))
  (opal:destroy *work-win*)
  (opal:destroy *main-win*)
  (careful-delete text-edit)
  (careful-delete mf-text-edit)
  (careful-delete popup-sub-obj-if-should)
  (Destroy-Filter-Wins)
  (Destroy-Encon-Wins)
  (Destroy-Error-Check-Wins))

