;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-DEBUG; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$


;;; Pop up a window displaying the slots of the object under the mouse
;;  when the HELP key is hit.  Allow editing of slots of the object.
;; 
;;  Designed and implemented by Brad Myers


(in-package "GARNET-DEBUG")
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(inspector inspect-next-inter Find-Slot-Starting-With
	    *INSPECTOR-KEY* *SHOW-OBJECT-KEY* *INSPECTOR-NEXT-INTER-KEY*)))

(defparameter big-font (opal:get-standard-font NIL :bold :large))
(defparameter regular-font (opal:get-standard-font NIL NIL NIL))
(defparameter bold-font (opal:get-standard-font NIL :bold NIL))
(defparameter bold-italic-font (opal:get-standard-font NIL :bold-italic NIL))
(defparameter italic-font (opal:get-standard-font NIL :italic NIL))
(defparameter formula-font (opal:get-standard-font NIL NIL :small))
(defparameter inherited-formula-font (opal:get-standard-font NIL
							     :italic :small))
(defparameter inherited-marker (cons " (I)" bold-italic-font))
(defparameter invalid-marker (cons " (IV)" bold-italic-font))
(defparameter valid-marker (cons " (V)" bold-italic-font))
(defparameter constant-marker (cons " (C)" bold-italic-font))

;; for debugging, when reload this module
(declaim (special used-window-list))
(when (boundp 'used-window-list)
  (dolist (w used-window-list)
    (when (schema-p w)
      (opal:destroy w))))

(defparameter used-window-list NIL)  ; list of all inspector windows


(defun beep-print (str)
  (fresh-line)
  (inter:beep)
  (princ str)
  (terpri))


;;;*******************************************************************

(defparameter debug-started-main-event-loop NIL)

;; This function returns T if we are in the debugger from inside the
;; main-event-loop-process or if we are not using the m-e-l-p then
;; returns T if inside debugger
(defun broken-inside-main-event-loop ()
  (if opal::*main-event-loop-process*
      ;; if the current process is the same as the m-e-l process
      (when (eq opal::*main-event-loop-process*
		#+allegro mp:*current-process*
                #+cmu mp:*current-process*
		#+ccl ccl:*current-process*
		#+sb-thread sb-thread:*current-thread*
		#-(or allegro cmu ccl sb-thread) T)
	;; and if it is in the debugger...
	;; the allegro code supplied by georgej@Franz.COM (George Jacob)
	#+allegro
	(not (zerop (multiprocessing:symeval-in-process
		     'tpl::*break-level*
		     opal::*main-event-loop-process*)))
	#+ccl
	;; Modeled after allegro code above (fmg)
	(not (zerop (ccl::symbol-value-in-process
		     'ccl::*break-level*
		     opal::*main-event-loop-process*)))
	#+sbcl
	;; A thread that is broken will bind the variable
	;; sb-debug:*debug-condition* whereas in a running
	;; thread it will be unbound.
	(ignore-errors
	  (sb-thread:symbol-value-in-thread
	   'sb-debug:*debug-condition*
	   *process-with-main-event-loop*))
	#-(or allegro ccl sbcl) NIL
	)
      ;; if not running the m-e-l process, then check if in debugger.
      ;; If so, then assume main-event-loop was in the process that crashed.
      ;; (This might be wrong if there are multiple processes in the
      ;; application, but the one that crashed is NOT the one running m-e-l).
      (if opal::*inside-main-event-loop*
	  #+sb-thread
	  (ignore-errors
	    (sb-thread:symbol-value-in-thread
	     'sb-debug:*debug-condition*
	     sb-thread:*current-thread*))
	  #+ccl
	  (not (zerop (ccl::symbol-value-in-process
		       'ccl::*break-level*
		       ccl:*current-process*)))
	  #+allegro
	  (not (zerop (mp:symeval-in-process 
		       'tpl::*break-level* 
		       mp:*current-process*)))
	      ;; if not in m-e-l, then need to run it
	  T)))

(defun INSPECTOR (obj)
  (let ((new-win (Get-Window-For-pop-up-debug 30 30)))
    (Create-Multi-Font-String-For-PS obj NIL new-win))
  (when (broken-inside-main-event-loop)
    (beep-print (format NIL
       "** Entering main-event-loop. Type the ~a key in an inspector window or
** hit the `Done All' button to exit" inter:*garnet-break-key*))
    (setq debug-started-main-event-loop T)
    ;; pretend that m-e-l-p is not running, in case breaking inside of it
    (let ((opal::*main-event-loop-process* NIL)
	  (opal::*inside-main-event-loop* NIL))
      (inter:main-event-loop))))

(defun inspect-inter (inter)
  (beep-print (format NIL "Debug: INSPECTOR of interactor ~s" inter))
  (inspector inter))

(defun inspect-next-inter ()
  (setq inter:*debug-next-inter* #'inspect-inter))
		   
(defun int-inspect-next-inter (ev)
  (declare (ignore ev))
  (if (eq inter:*debug-next-inter* #'inspect-inter)
      (progn
	(beep-print "Removing inspection of next interactor")
	(setq inter:*debug-next-inter* NIL))
      (progn
	(beep-print "Will pop up inspect window on next interactor")
	(inspect-next-inter))))
	   
;; This is set as a global accelerator to view objects
(defun Pop-Up-ps-for-event (ev)
  (let ((win (inter:event-window ev))
	 agg obj)
    (if win
	(if (setq agg (g-value win :aggregate))
	    (if (setq obj (opal:point-to-leaf agg (inter:event-x ev)
					      (inter:event-y ev)))
		(progn 
		  (beep-print (format NIL
			 "Debug: INSPECTOR on ~s from window ~s" obj win))
		  (INSPECTOR obj)
		  (opal:update-all))
		(beep-print
		 (format NIL
	       "INSPECTOR: No object under point (~s,~s) in window ~s"
			 (inter:event-x ev) (inter:event-y ev) win)))
	    (beep-print
		 (format NIL "INSPECTOR: No aggregate in window ~s" win)))
	(beep-print
		 (format NIL "INSPECTOR: No window in event ~s" ev)))))
	    
;; This is set as a global accelerator to just print out the object's name
(defun Show-Object-On-event (ev)
  (let ((win (inter:event-window ev))
	 agg obj)
    (fresh-line)
    (if win
	(if (and (setq agg (g-value win :aggregate))
		 (setq obj (opal:point-to-leaf agg (inter:event-x ev)
					       (inter:event-y ev))))
	    (progn
	      (format T "--> (~s,~s) = ~s in window ~s~%"
		    (inter:event-x ev) (inter:event-y ev) obj win)
	      (opal:set-x-cut-buffer win (format NIL "~s" obj)))
	    ;; else no object
	    (format T "--> No object at (~s,~s) in window ~s~%"
		    (inter:event-x ev) (inter:event-y ev) win))
	;; else no window
	(format T "--> No window in event ~s~%" ev))))
	    

;;;	(defvar *INSPECTOR-KEY* :help)
;;;	(defvar *INSPECTOR-NEXT-INTER-KEY* :control-help)
;;;	(defvar *SHOW-OBJECT-KEY* :shift-help)
(defvar *INSPECTOR-KEY* :f1)
(defvar *INSPECTOR-NEXT-INTER-KEY* :control-f1)
(defvar *SHOW-OBJECT-KEY* :shift-f1)

(inter:add-global-accelerator *INSPECTOR-KEY* 'Pop-Up-ps-for-event :first? T)
(inter:add-global-accelerator *INSPECTOR-NEXT-INTER-KEY*
			      'int-inspect-next-inter :first? T)
(inter:add-global-accelerator *SHOW-OBJECT-KEY*
			      'Show-Object-On-event :first? T)

(format T "~%==> Garnet-Debug: hit ~A key for INSPECTOR on object under mouse~%"
        *inspector-key*)
(format T "==> Garnet-Debug: hit ~A for INSPECTOR on next Interactor~%"
        *inspector-next-inter-key*)
(format T "==> Garnet-Debug: hit ~A to list obj under mouse~%~%"
        *show-object-key*)


;;*******************************************************************

(defun Db-Show-Error (window string &optional (error? T))
  (let ((err-obj (g-value window :aggregate :error-string))
	(err-str (if error?
		 (format NIL "** ERROR: ~a" string)
		 string)))
    ;; ** remove any CRs in the string
    ;; ** (setq err-str (substitute #\space #\newline err-str))
    (s-value err-obj :string err-str)
    (s-value err-obj :font bold-font)
    (opal:update window)
    (when error?
      (inter:beep))))

(defun Db-Done-Error (window)
  (let ((pop-ps (g-value window :aggregate)))
    (when pop-ps
      (let ((err-str (g-value pop-ps :error-string)))
	(s-value err-str :string (g-value pop-ps :object-name))
	(s-value err-str :font big-font)))))

(defun Handle-Double-Click (inter string-obj event)
  (declare (ignore event))
  (Select-Word-Around-Cursor string-obj)
  ;; put word into X cut buffer
  (opal:set-x-cut-buffer (g-value string-obj :window)
			 (caaar (opal:copy-selected-text string-obj)))
  (inter:stop-interactor inter))

;; Called when hit return
(defun Edit-Field (inter string-obj event)
  (declare (ignore event))
  ;; find = to the left, if not there then error
  (let* ((pop-ps (g-value string-obj :parent))
	 (win (g-value string-obj :window))
	 (obj (car (g-value pop-ps :current-object-l)))
	 str slot)
    (cond ((formula-p obj)
	   (Db-Show-Error win "Can't set slots of formulas"))
	  ((not (schema-p obj))
	   (Db-Show-Error win (format NIL "~s is no longer a valid schema"
				      obj)))
	  ((null (setq slot (Get-Slot-Name-From-line string-obj)))
	   (Db-Show-Error win
		     "Line with cursor does not start with a slot name"))
	  ((null (setq str (Search-For-Value string-obj)))
	   (Db-Show-Error win "Can't find = to the left of the cursor"))
	  (T (db-careful-read-and-s-value obj slot str win)))
  (inter:stop-interactor inter)
  ;; refetch the display
  (Create-Multi-Font-String-For-PS obj (g-value pop-ps :showing-inherited?)
				   win :reset-error-msg NIL
				   :reset-extra-slots NIL)))

;; searches left until finds = on the current line.  Then
;; searches right until finds newline or end-of line.  Returns
;; string in between or NIL if no = on this line.  Leaves cursor at
;; current position 
(defun Search-For-Value (gob) 
  (multiple-value-bind (orig-line orig-char)
      (opal:get-cursor-line-char-position gob)
    (let (len str)
      (do ((char (opal:FETCH-PREV-CHAR gob) (opal:FETCH-PREV-CHAR gob)))
	  ((or (null char) (eq char #\newline) (eq char #\=)))
	(opal:GO-TO-PREV-CHAR gob))
      (when (eq (opal:FETCH-PREV-CHAR gob) #\=)
	(multiple-value-bind (eq-line eq-char)
	    (opal:get-cursor-line-char-position gob)
	  (do ((char (opal:FETCH-NEXT-CHAR gob) (opal:FETCH-NEXT-CHAR gob))
	       (cnt 0 (1+ cnt)))
	      ((or (null char) (eq char #\newline)))
	    (opal:GO-TO-NEXT-CHAR gob)
	    (setq len cnt))
	  (setq str (make-string len :initial-element #\+))
	  ;; can't do eq-char + 1 because might be off eq-line, so
	  ;; goto next char and it will be after the "= "
	  (opal:set-cursor-to-line-char-position gob eq-line eq-char)
	  (opal:go-to-next-char gob)
	  (dotimes (i len)
	    (setf (char str i) (opal:GO-TO-NEXT-CHAR gob)))))
      (opal:set-cursor-to-line-char-position gob orig-line orig-char)
      str)))				; may be NIL

(defparameter spacers " 
 ") ;; space and newline

;; must be used in a "Dependencies" line.  Returns (values obj slot) or NIL
(defun search-line-for-slot-and-obj (gob &key (previous? NIL))
  (let (slot-name slot obj-name obj start end)
    (multiple-value-bind (orig-line orig-char)
	(opal:get-cursor-line-char-position gob)
      (when previous? (opal:go-to-prev-line gob))
      (opal:go-to-beginning-of-line gob)
      (opal:go-to-next-word gob)	; skip white-space
      (setq slot-name (opal:go-to-next-word gob))
      (setq slot-name (string-trim spacers slot-name))
      (when (and (> (length slot-name) 0)
		 (eq #\: (char slot-name 0)))
	(multiple-value-bind (val error?)
	    (gg:careful-read-from-string slot-name)
	  (unless error?
	    (setq slot val)
	    (opal:go-to-next-word gob)	; " of "
	    (setq obj-name (string-trim spacers (opal:go-to-next-word gob)))
	    (setq obj-name (string-upcase obj-name))
	    (when (and (setq start (search "#K<" obj-name))
		     (setq end (position #\> obj-name :from-end T)))
	      (setq obj-name (subseq obj-name (+ 3 start) end))
	      (multiple-value-bind (val error?)
		  (gg:careful-string-eval obj-name)
		(unless error?
		  (setq obj val)))))))
      (opal:set-cursor-to-line-char-position gob orig-line orig-char)
      (when (and slot obj)
	(values obj slot)))))


;; see if there is a slot name as the first thing on the line with the
;; cursor.  Leave the cursor where it starts off at
(defun Get-Slot-Name-From-line (gob)
  (multiple-value-bind (orig-line orig-char)
      (opal:get-cursor-line-char-position gob)
    (do ((char (opal:FETCH-PREV-CHAR gob) (opal:FETCH-PREV-CHAR gob)))
	((or (null char) (eq char #\newline)))
      (opal:GO-TO-PREV-CHAR gob))
    ;; now at beginning of line
    (let ((str (opal:go-to-next-word gob))
	  slot)
      (when (eq #\: (char str 0))
	(multiple-value-bind (val error?)
	    (gg:careful-read-from-string str)
	  (unless error? (setq slot val))))
      (opal:set-cursor-to-line-char-position gob orig-line orig-char)
      slot)))
	
;;*** TEMP, until #K<...> is a reader macro
;; if the str is an object definition, returns the object itself.  If
;; error, returns NIL and sets error string
(defun Find-Object-In-Word (str win &key (ok-no-selection NIL)
				         (ok-keyword NIL)
					 (ok-dot-dot NIL))
  (if (and str (> (length str) 0))
    (let ((obj-str (string-trim "'(){}" str))
	  start end)
      (setq obj-str (string-upcase obj-str))
      (if (and (setq start (search "#K<" obj-str))
		 (setq end (position #\> obj-str :from-end T)))
	  (progn
	    (setq obj-str (subseq obj-str (+ 3 start) end))
	    (multiple-value-bind (val errorp)
		(gg:careful-string-eval obj-str)
	      (if errorp
		  (progn (Db-Show-Error win
	     "Selected object seems to be invalid.  Might be destroyed.")
			 NIL)
		  ;; else, legal value!!
		  val)))
	  ;; else no #k<>
	  (cond ((and ok-dot-dot
		      (string= obj-str "..."))
		 :dot-dot)
		(ok-keyword  ; check if selection is a keyword
		 (multiple-value-bind (val errorp)
		      (gg:careful-string-eval obj-str)
		    (if errorp
		       (progn (Db-Show-Error win
		        "Selection must be an object or a keyword (slot name)")
			      NIL)
		       ;; else, legal keyword
		       val)))
		(T ;; else not legal
		 (Db-Show-Error win
		    "Only works for Garnet objects; which must be in #k<>")
		 NIL))))
    ;; else need a string
    (if ok-no-selection :none
	;; otherwise, need a string, so error.
	(progn
	  (Db-Show-Error win "First Double click on the name of the object to show")
	  NIL))))

(defun db-careful-read-and-s-value (obj slot str error-win)
  (multiple-value-bind (val error?)
      (gg:careful-read-from-string str)

    ;; special hack that evals the value if there is no error.  Allows
    ;; computations and also the use of atom names for objects
    (unless error?
      (multiple-value-bind (new-val new-error?)
	  (gg:careful-eval val)
	(unless new-error?
	  (setq val new-val))))
		 
    (cond (error?			; then return string
	   (Db-Show-Error error-win (princ-to-string error?)))
	  ;; else read was ok, check types
	  ((stringp (setq error? (kr:check-slot-type obj slot val NIL)))
	   (Db-Show-Error error-win error?))
	  (T				; value is OK
	   (let ((win (g-value obj :window)))
	     (when (slot-constant-p obj slot) ; exported by KR
	       (Db-Show-Error error-win (format NIL
		     "~s is constant, set anyway but may not work" slot) NIL))
	     (with-constants-disabled
		 (s-value obj slot val))
	     (when (and win (schema-p win))
	       ;; the global update-all won't update the obj's window if there
	       ;; was a previous error, so explicitly update it
	       (opal:update win))
	     NIL)))))			; return NIL when all OK

(defun Select-Word-Around-Cursor (gob)
  (let ((our-delim-chars (append opal::*delim-chars* (list #\( #\)))))
    (multiple-value-bind (orig-line orig-char)
	(opal:get-cursor-line-char-position gob)
      (do ((char (opal:FETCH-PREV-CHAR gob) (opal:FETCH-PREV-CHAR gob)))
	  ((or (null char) (member char our-delim-chars)))
	(opal:GO-TO-PREV-CHAR gob))
      (multiple-value-bind (start-line start-char)
	  (opal:get-cursor-line-char-position gob)
	(opal:set-selection-to-line-char-position gob start-line start-char)
 
	(opal:set-cursor-to-line-char-position gob orig-line orig-char)
	(do ((char (opal:FETCH-NEXT-CHAR gob) (opal:FETCH-NEXT-CHAR gob)))
	    ((or (null char) (member char our-delim-chars)))
	  (opal:GO-TO-NEXT-CHAR gob))))))




;;;******************************************************************

(defun Find-Common-Prefix (slot-list)
  (let* ((min-mism 9999)
	 (first-slot (symbol-name (car slot-list)))
	 (len (length first-slot))
	 mism)
    (dolist (sl (cdr slot-list))
      (setq mism (mismatch first-slot (symbol-name sl)))
      (if mism
	  (setq min-mism (min min-mism mism))
	  (setq min-mism (min min-mism len))))
    (subseq first-slot 0 min-mism)))

;; If a single slot starts with str, then returns the slot,
;; If a single slot is the same as str, then returns slot
;; If multiple slots start with str, then returns multiple values:
;; (NIL list-of-slots unique-prefix), where list-of-slots are all
;; the slots that start with str, and unique-prefix is the string
;; starting with str that is unique among all slots.
;; The colon on the front of the str is optional.
(defun Find-Slot-Starting-With (object str)
  "Find a slot of object that starts with str"
  (let* ((match-str (string-upcase (string-left-trim ": " str)))
	 (len (length match-str))
	 slot-list)
    (call-on-ps-slots 
     object
     #'(lambda (schema slot form inherited valid real-value
		types bits indent limit)
	 (declare (ignore schema form inherited valid real-value
			  types bits indent limit))
	 (let* ((slotname (symbol-name slot))
		(mism (mismatch match-str slotname)))
	   (cond ((null mism)		; names are identical
		  (return-from Find-Slot-Starting-With slot))
		 ((eq len mism)		; then is a prefix
		  (push slot slot-list)))))
     :inherit T)
    (cond ((null slot-list) NIL)
	  ((null (cdr slot-list)) (car slot-list))
	  (T				; multiple slots match
	   (values NIL slot-list (Find-Common-Prefix slot-list))))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(create-instance 'debug-edit inter:multifont-text-interactor
  (:start-where (o-formula (list :in (gvl :operates-on :string-obj))))
  (:running-priority inter:normal-priority-level) ; so buttons can be hit
  (:stop-event :foo)				  ; stopped explicitly by actions
  )



;;; remove all the font change commands

(inter:unbind-key :control-\u debug-edit) 
(inter:unbind-key :control-B debug-edit) 
(inter:unbind-key :control-I debug-edit) 
(inter:unbind-key :control-> debug-edit) 
(inter:unbind-key :control-< debug-edit) 
(inter:unbind-key :control-1 debug-edit)
(inter:unbind-key :control-2 debug-edit)
(inter:unbind-key :control-3 debug-edit)
(inter:unbind-key :control-4 debug-edit)
(inter:unbind-key :control-! debug-edit)
(inter:unbind-key :control-@ debug-edit)
(inter:unbind-key :control-# debug-edit)
(inter:unbind-key :control-$ debug-edit)
(inter:unbind-key :control-F debug-edit)
(inter:unbind-key :control-T debug-edit)
(inter:unbind-key :control-H debug-edit)

;; double-click 
(inter:bind-key :double-leftdown 'Handle-Double-Click debug-edit)
;; middledown like in xterm
(inter:bind-key :middledown :copy-from-X-cut-buffer debug-edit)

;; return (set new value)
(inter:bind-key #\return #'Edit-Field debug-edit)
(inter:bind-key :NUM-PAD-ENTER #'Edit-Field debug-edit)



;;; Commands
;;

(defun Re-Fetch (widget value)
  (declare (ignore value))
  (let* ((ps-pop (g-value widget :parent))
	 (win (g-value widget :window))
	 (current-object (car (g-value ps-pop :current-object-l))))
    (Create-Multi-Font-String-For-PS current-object
				     (g-value ps-pop :showing-inherited?)
				     win
				     :push-object NIL
				     :reset-extra-slots NIL)))

;; Show the selected object in the same window
(defun Show-Object (widget value)
  (declare (ignore value))
  (let* ((ps-pop (g-value widget :parent))
	 (win (g-value widget :window))
	 (string-obj (g-value ps-pop :string-obj))
	 (str (opal::text-to-string (opal:copy-selected-text string-obj)))
	 (obj (Find-Object-In-Word str win)))
    (when obj
	(Create-Multi-Font-String-For-PS obj NIL win))))

;; Show the selected object in a new window
(defun Show-Object-in-new (widget value)
  (declare (ignore value))
  (let* ((window (g-value widget :window))
	 (ps-pop (g-value widget :parent))
	 (string-obj (g-value ps-pop :string-obj))
	 (str (opal::text-to-string (opal:copy-selected-text string-obj)))
	 (obj (Find-Object-In-Word str window)))
    (when obj				; find object-in-word will print error message
      (let ((new-win (Get-Window-For-pop-up-debug
		      (+ 30 (g-value window :left))
		      (+ 30 (g-value window :top)))))
	(Create-Multi-Font-String-For-PS obj NIL new-win)))))



;;;******************************************************************

(defparameter search-prompt "Find slot named: ")

;; Button function for search: start text editing
(defun Search-Slot (widget value)
  (declare (ignore value))
  (let* ((ps-pop (g-value widget :parent))
	 (search-str (g-value ps-pop :search-string)))
    (inter:stop-interactor (g-value ps-pop :debug-edit))
    (db-show-error (g-value widget :window)
		   (format NIL "~%~a" search-prompt) NIL)  
    (s-value search-str :visible T)
    (s-value search-str :string "")
    (setf (inter:event-x inter:*current-event*) (g-value search-str :left))
    (setf (inter:event-y inter:*current-event*) (g-value search-str :top))
    (inter:start-interactor (g-value ps-pop :search-edit))))

;; Final-Function for when hit CR in search
(defun Do-Search (inter search-str-obj ev final-str x y)
  (declare (ignore ev x y))
  (let* ((ps-pop (g-value inter :operates-on))
	 (obj (car (g-value ps-pop :current-object-l))))
    (multiple-value-bind (slot list-of-slots unique-prefix)
	(Find-Slot-Starting-With obj final-str)
      (if slot
	  (progn
	    (s-value ps-pop :extra-slots-to-show 
		     (nconc (g-value ps-pop :extra-slots-to-show)
			     (list slot)))
	    (s-value search-str-obj :visible NIL)
	    (s-value search-str-obj :string "")
	    (Create-Multi-Font-String-For-PS obj
			       (g-value ps-pop :showing-inherited?)
			       (g-value ps-pop :window)
			       :push-object NIL
			       :reset-extra-slots NIL))
	  ;; else ambiguous or not found
	  (let ((win (g-value search-str-obj :window)))
	    (if list-of-slots
		;; fill out prefix
		(progn
		  (s-value search-str-obj :string unique-prefix)
		  (db-show-error win
		     (format NIL "Ambiguous: ~a~%~a" list-of-slots
			     search-prompt)))
		;; else nothing starting with that prefix
		(db-show-error win
		     (format NIL "No slot starting with that prefix~%~a"
			     search-prompt)))
	    (setf (inter:event-x inter:*current-event*)
		  (g-value search-str-obj :left))
	    (setf (inter:event-y inter:*current-event*)
		  (g-value search-str-obj :top))
	    (setf (inter:event-char inter:*current-event*) NIL)
	    (inter:start-interactor inter))))))
		  

	   

;;;******************************************************************

(create-instance 'ps-pop-agg opal:aggregadget
  (:current-object-l NIL) ; a list of the current object.  is a list
			  ; so that a formula object can be put in it
  (:object-history NIL)
  (:object-name (o-formula (format NIL "~s" (car (gvl :current-object-l)))))
  (:showing-inherited? NIL)
  (:extra-slots-to-show NIL) ; list of slots to show
  (:parts
   `((:button-list ,gg:text-button-panel
      (:Constant (T :except :pixel-margin))
      (:items (("Show Object" Show-Object)
	       ("Show in New" Show-Object-in-new)
	       ("Re-Fetch" Re-Fetch)
	       ("Dependencies" Show-dependencies)
	       ("Done" Done)
	       ("Done All" Done-All)
	       ("Flash" Flash-Object)
	       ("Search" Search-Slot)
	       ("Notify" Notify-Slot)
	       ("Break" Break-Slot)
	       ("Clear Breaks" Clear-Breaks)
	       ("Inherited Slots" Show-Hide-Inherited-Slots)
	       ("Objects" List-Other-Objects)))
      (:selection-function ,#'(lambda (gad val)
			       (declare (ignore val))
			       (Db-Done-Error (g-value gad :window))))
      (:top 3)(:left 3)
      (:GRAY-WIDTH 1)
      (:FINAL-FEEDBACK-P NIL)
      (:pixel-margin ,(o-formula (gvl :window :width)))
      (:TEXT-OFFSET 1)
      (:SHADOW-OFFSET 1)
      (:DIRECTION :HORIZONTAL)
      (:fixed-width-p NIL))
     (:error-string ,opal:multi-text	; used for title and errors
      (:font ,big-font)
      (:string "<no object>")
      (:left 2)(:top ,(o-formula (+ 2 (opal:gv-bottom
				       (gvl :parent :button-list))))))
     (:search-string ,opal:cursor-text  
      (:visible NIL)
      (:string "")
      (:left ,(+ 2 (opal:string-width opal:default-font search-prompt)))
      (:top ,(o-formula (+ (gvl :parent :error-string :top)
			   (opal:string-height opal:default-font "]")))))
     (:string-obj ,opal:multifont-text
      (:left 5)
      (:word-wrap-p t)
      (:text-width ,(o-formula (gvl :window :width) 460))
      (:top ,(o-formula (+ 2 (gvl :parent :error-string :top)
		   (max (* 2 (opal:string-height opal:default-font "]"))
			(opal:string-height big-font "]"))))))))
    (:interactors
     `((:debug-edit ,debug-edit
	(:window ,(o-formula (gvl :operates-on :window))))
       (:search-edit ,inter:text-interactor
	(:window ,(o-formula (gvl :operates-on :window)))
	(:abort-event (:control-\g :any-mousedown))
	(:cursor-where-press NIL)
	(:start-event NIL)		; explicitly started
	(:start-where ,(o-formula (list :in
					(gvl :operates-on :search-string))))
	(:final-function Do-Search)
	(:abort-action ,#'(lambda (inter obj ev)
			    (Db-Done-Error (g-value inter :window))
			    (s-value obj :visible NIL)
			    (call-prototype-method inter obj ev)))
	))))

(defun Done-All (widget value)
  (declare (ignore widget value))
  (let ((all-ps-pops (g-value ps-pop-agg :is-a-inv))
	win)
    (clear-slot-set)
    (dolist (ps-pop all-ps-pops)
      (when (and (setq win (g-value ps-pop :window))
		 (g-value win :visible))
	(Done-With-Window win)))
    (when debug-started-main-event-loop
      (opal:update-all)
      (setq debug-started-main-event-loop NIL)
      (inter:exit-main-event-loop))))

(defun Done (widget value)
  (declare (ignore value))
  (clear-slot-set)
  (let ((window (g-value widget :window)))
    (Done-With-Window window))
  (when debug-started-main-event-loop
    ;; check and see if any inspector windows left, and if so, exit m-e-l
    (let ((all-ps-pops (g-value ps-pop-agg :is-a-inv))
	  (found-one NIL)
	  win)
      (dolist (ps-pop all-ps-pops)
	(when (and (setq win (g-value ps-pop :window))
		   (g-value win :visible))
	  (setq found-one T)))
      (unless found-one
	(opal:update-all)
	(setq debug-started-main-event-loop NIL)
	(inter:exit-main-event-loop)))))

(defun flash-it (obj ps-pop)
  (let ((pop-win (g-value ps-pop :window)))
    (cond ((is-a-p obj inter:interactor)
	   (db-show-error pop-win "Can't flash interactors"))
	  ((formula-p obj)
	   (db-show-error pop-win "Can't flash formulas"))
	  (T (let ((win (g-value obj :window))
		   str)
	       (when win 
		 (s-value win :visible T)
		 (opal:raise-window win))
	       (Db-Show-Error pop-win (format NIL "--Flashing ~s" obj) NIL)
	       (setq str (with-output-to-string (*standard-output*)
			   (garnet-debug:flash obj)))
	       (if (string= str "")
		   ;; all ok
		   (Db-Done-Error pop-win)
		   ;; else error
		   (progn
		     (setq str (string-trim '(#\newline) str))
		     (Db-Show-Error pop-win str))))))))

(defun Flash-Object (widget value)
  (declare (ignore value))
  (let* ((ps-pop (g-value widget :parent))
	 (pop-win (g-value widget :window))
	 (main-obj (car (g-value ps-pop :current-object-l)))
	 (string-obj (g-value ps-pop :string-obj))
	 (str (opal::text-to-string (opal:copy-selected-text string-obj)))
	 (obj (Find-Object-In-Word str pop-win :ok-no-selection T)))
    ;; find-object-in-word will return a valid object, or else
    ;; will print a message, unless NOTHING is selected
    (cond ((eq obj :none)		; then print the main object
	   (cond ((schema-p main-obj)
		  (flash-it main-obj ps-pop))
		 (T			; whoops, obj was destroyed
		  (Db-Show-Error 
		   pop-win
		   (format NIL
			   "Object ~s seems to be invalid.  Might be destroyed."
			   main-obj)))))
	   (obj (flash-it obj ps-pop))
	   (T				; error message already printed
	    ))))

(defun Show-Hide-Inherited-Slots (widget value)
  (declare (ignore value))
  (let* ((ps-pop (g-value widget :parent))
	 (current-object (car (g-value ps-pop :current-object-l))))
    (Create-Multi-Font-String-For-PS current-object
			       (not (g-value ps-pop :showing-inherited?))
			       (g-value widget :window)
			       :push-object NIL
			       :reset-extra-slots NIL)))



;;; Formula dependencies view
;;

(defparameter indent-amt 3)
(defparameter max-depth 3)

(defparameter max-indent (* indent-amt (1- max-depth)))

;; returns a list of strings of the dependencies
(defun Generate-Dependencies (slot obj)
  (int-generate-dependencies (kr::i-depend-on obj slot) indent-amt))

(defun int-Generate-Dependencies (deps indent)
  (let (string-list)
    (when deps
      (dolist (objslot deps)
	(let ((subobj (car objslot))
	      (subslot (cdr objslot))
	      subdeps this subs)
	  (setq this (list (cons (format NIL "~a~s of ~s = ~s"
				(make-string indent :initial-element #\space)
				subslot subobj 
				(when (schema-p subobj)
				  (g-value subobj subslot)))
			    regular-font)))
	  (when (setq subdeps (kr::i-depend-on subobj subslot))
	    (if (> indent max-indent)
		(push (list (cons (format NIL "~a..."
				(make-string (+ indent indent-amt)
					     :initial-element #\space))
			    bold-font))
		      string-list)
		(progn
		  (setq subs (int-generate-dependencies subdeps
							(+ indent indent-amt)))
		  (setq string-list (append subs string-list)))))
	  (push this string-list)))
      string-list
      )))

(defun Show-Dependencies (widget value)
  (declare (ignore value))
  (let* ((ps-pop (g-value widget :parent))
	 (window (g-value widget :window))
	 (string-obj (g-value ps-pop :string-obj))
	 (str (opal::text-to-string (opal:copy-selected-text string-obj)))
	 (other-obj (Find-Object-In-Word str window :ok-no-selection T
					 :ok-keyword T
					 :ok-dot-dot T))
	 form obj slot title string-list)

    ;; find-object-in-word will return a valid object, or else
    ;; will print a message, unless NOTHING is selected
    (cond ((formula-p other-obj)
	   (setq form other-obj)
	   (setq slot (g-formula-value form :slot))
	   (setq obj (g-formula-value form :schema)))
	  ((schema-p other-obj) 
	   ;; see if can find a slot on this line to the left
	   (multiple-value-setq (obj slot)
	     (search-line-for-slot-and-obj string-obj))
	   (unless obj
	     (Db-Show-Error window "Need to select a slot, formula or ...")
	     (return-from Show-Dependencies)))
	  ((eq other-obj :none)
	   (multiple-value-setq (obj slot)
	     (search-line-for-slot-and-obj string-obj))
	   (unless obj
	     (if (formula-p (setq form
				  (car (g-value ps-pop :current-object-l))))
	       (progn
		 (setq slot (g-formula-value form :slot))
		 (setq obj (g-formula-value form :schema)))
	       (progn
		 (Db-Show-Error window "Need to select a slot, formula or ...")
		 (return-from Show-Dependencies)))))
	  ((eq other-obj :dot-dot)
	   (multiple-value-setq (obj slot)
	     (search-line-for-slot-and-obj string-obj :previous? T))
	   (unless obj
	     (Db-Show-Error window "Can't find obj and slot on line above ...")
	     (return-from Show-Dependencies)))
	  ((keywordp other-obj)
	   (setq slot other-obj)	; that slot of obj
	   (multiple-value-bind (new-obj new-slot)
	       (search-line-for-slot-and-obj string-obj)
	     (if (and new-obj (eq new-slot slot))
		 (setq obj new-obj)
		 ;; otherwise, use the object for this display
		 (setq obj (car (g-value ps-pop :current-object-l))))))
	  (T (return-from Show-Dependencies))) ; error already printed

	  (unless form
	    (setq form (get-value obj slot))
	    (unless (formula-p form)
	      (Db-Show-Error window
		       (format NIL "Slot ~s of ~s does
          not contain a formula"
				 slot obj))
		   (return-from Show-Dependencies)))
    (setq title
	  (list 
	   (list (cons (format NIL
			 "Slot ~s of ~s (formula = ~s) = ~s:"
			 slot obj form (g-cached-value obj slot))
		      bold-font))
	   (list (cons (format NIL "Expression = ~a"
			       (write-to-string (g-formula-value form :lambda)
						:pretty t))
		       regular-font))
	   (list (cons "Dependencies:" bold-font))))

    (setq string-list (append title (Generate-Dependencies slot obj)))

    ;; now set string-list
    
    (opal:set-text string-obj string-list)
    (s-value string-obj :visible T)
    (Db-Done-Error window)
    (s-value window :height (+ (g-value string-obj :top)
			       (g-value string-obj :height)
			       5))
    (opal:update window)))



;;; Break and Notify
;;

(defun show-inspector-break (schema slot new-value reason window break?)
  (let ((s (format NIL "~s slot ~s set with ~s due to ~a"
		   schema slot new-value reason)))
    (format T "#### ~a~%" s)
    (when (schema-p window)
      (db-show-error window
		     (if break?
			 (format NIL "#BREAK# ~a.
#### See Lisp Listener Window ####" s)
			 s)
		     NIL))))

(defun Inspector-Break-Slot (schema slot new-value reason window)
  (show-inspector-break schema slot new-value reason window T)
  (inter:beep)
  (break))

(defun Inspector-Notify-Slot (schema slot new-value reason window)
  (show-inspector-break schema slot new-value reason window NIL))

(defun Clear-Breaks (widget value)
  (declare (ignore value))
  (clear-slot-set)
  (Db-Show-Error (g-value widget :window)
		 "--Cleared all breaks and notifies" NIL))
(defun Break-Slot (widget value)
  (declare (ignore value))
  (Internal-Break-or-Notify-Slot widget #'Inspector-Break-Slot))
(defun Notify-Slot (widget value)
  (declare (ignore value))
  (Internal-Break-or-Notify-Slot widget #'Inspector-Notify-Slot))

(defun Internal-Break-or-Notify-Slot (widget fnc)
  (let* ((ps-pop (g-value widget :parent))
	 (obj (car (g-value ps-pop :current-object-l)))
	 (string-obj (g-value ps-pop :string-obj))
	 (window (g-value widget :window))
	 (str (opal::text-to-string (opal:copy-selected-text string-obj)))
	 (other-obj (Find-Object-In-Word str window :ok-no-selection T
					 :ok-keyword T))
	 slot)
    (cond ((schema-p other-obj) (setq obj other-obj)
	   (setq slot :*any*))				   ; use other obj; any slot
	  ((eq other-obj :none) (setq slot :*any*))	   ; any slot of this obj
	  ((keywordp other-obj) (setq slot other-obj))	   ; that slot
	  (T (return-from Internal-Break-or-Notify-Slot))) ; error printed
    (Db-Show-Error window (format NIL "--Will ~a when ~s slot ~s set."
				  (if (eq fnc #'Inspector-Break-Slot)
				      "Break" "Print Message")
				  obj slot) NIL)
    (call-func-on-slot-set obj slot :*any* fnc window))) ; in debug-fns
      


;;; Object Window
;;

(defun List-Other-Objects (widget value)
  (declare (ignore value))
  (let* ((ps-pop (g-value widget :parent))
	 (window (g-value widget :window))
	 (string-obj (g-value ps-pop :string-obj))
	 (str (opal::text-to-string (opal:copy-selected-text string-obj)))
	 (other-obj (Find-Object-In-Word str window :ok-no-selection T))
	 obj history title o is-a-list agg-list string-list)

    ;; find-object-in-word will return a valid object, or else
    ;; will print a message, unless NOTHING is selected
    (cond ((schema-p other-obj)
	   (Set-Up-Ps-Pop-For-Obj window ps-pop other-obj NIL T T))
	  ((eq other-obj :none))		; fine
	  (T (return-from List-Other-Objects))) ; error already printed

    (setq obj (car (g-value ps-pop :current-object-l)))
    (setq history (g-value ps-pop :object-history))

    ;; do is-a
    (cond ((formula-p obj)
	   (setq title
		 (list (cons (format NIL "For ~s in slot ~s of ~s :" obj
				     (g-formula-value obj :slot)
				     (g-formula-value obj :schema))
			     bold-font)))

	   (setq o (g-formula-value obj :is-a))
	   (loop
	    (when (null o) (return))
	    (push o is-a-list)
	    (if (formula-p o)
		(setq o (g-formula-value o :is-a))
		(return))))		; a broken one
	  ((schema-p obj)
	   (setq title
		 (list (cons (format NIL "For ~s :" obj) bold-font)))
	   (setq o (car (g-value obj :is-a)))
	   (loop
	    (when (null o) (return))
	    (push o is-a-list)
	    (if (schema-p o)
		(setq o (car (g-value o :is-a)))
		(return))))		; a bad object
	  (T				; must be a destroyed object
	   (Db-Show-Error window
		     (format NIL "Object ~s is invalid; might be destroyed"
			     obj))
	   (return-from List-Other-Objects)))
    ;; do agg
    (unless (formula-p obj)
      (setq o (g-value obj :parent))
      (loop
       (when (null o)
	 (when (setq o (g-value obj :window))
	   (push o agg-list))
	 (return))
       (push o agg-list)
       (if (schema-p o)
	   (setq o (g-value o :parent))
	   (return))))			; must be a destroyed object

    ;; now set string-list
    
    (setq string-list (Gen-Object-Columns history is-a-list agg-list))
    (push title string-list)
    (push (list "") string-list)	; blank line
    (opal:set-text string-obj string-list)
    (s-value string-obj :visible T)
    (Db-Done-Error window)
    (s-value window :height (+ (g-value string-obj :top)
			       (g-value string-obj :height)
			       5))
    (opal:update window)))

     
(defun Gen-Object-Columns (history is-a-list agg-list)
  (let (strings)
    (if history
	(dolist (o history)
	  (push (list (format NIL "   ~s" o)) strings))
	(push (list "   <<none>>") strings))
    (push (list (cons "Objects Shown in This Window:" bold-font))
	  strings)
    (push (list "") strings)		; blank line
    (if agg-list
	(dolist (o agg-list)
	  (push (list (format NIL "   ~s" o)) strings))
	(push (list "   <<none>>") strings))
    (push (list (cons "Aggregate hierarchy (:parent slot):" bold-font))
	  strings)
    (push (list "") strings)		; blank line

    (if is-a-list
	(dolist (o is-a-list)
	  (push (list (format NIL "   ~s" o)) strings))
	(push (list "   <<none>>") strings))
    (push (list (cons "IS-A hierarchy:" bold-font)) strings)
    (push (list "") strings)		; blank line

    strings))

    
	
;;;******************************************************************

(defun Get-Window-For-pop-up-debug (left top)
  (let ((win (pop used-window-list)))
    (unless (and win
		 (schema-p win))	; make sure not destroyed somehow
      (setq win (create-instance NIL inter:interactor-window
		  (:title "Inspector")
		  (:aggregate (create-instance NIL ps-pop-agg))
		  (:width 460))))
    (s-value win :left left)
    (s-value win :top top)
    (s-value win :visible T)
    ;; (format T "Window ~s pop-ps ~s~%" win (g-value win :aggregate))
    win))

(defun Done-With-Window (win)
  (s-value win :visible NIL)
  (s-value (g-value win :aggregate) :object-history NIL)
  (push win used-window-list))

;; Sets up the data structure and window but doesn't load the string list.
(defun Set-Up-Ps-Pop-For-Obj (window ps-pop schema inherit? reset-error-msg
			      push-object)
  (let (temp-w)
    (s-value ps-pop :current-object-l (list schema))
    (s-value ps-pop :showing-inherited? inherit?)
    (s-value (g-value ps-pop :search-string) :visible NIL)
    (when push-object
      (pushnew schema (g-value ps-pop :object-history)))
    (unless window (error "no window"))
    (when reset-error-msg
      (Db-Done-Error window))
    (s-value window :title (Format NIL "Showing ~s" schema))
    ;; find a valid window
    (cond ((g-value window :drawable) (setq temp-w window))
	  ;; else try object
	  ((and (schema-p schema)
		(setq temp-w (g-value schema :window))
		(schema-p temp-w)))
	  (T (setq temp-w NIL)))
    (when temp-w
      (opal:set-x-cut-buffer temp-w
			     (g-value ps-pop :object-name)))))
	

(defparameter *extra-slot-list* NIL)
;; temp variable used to hold strings list while it is being generated
(defparameter *strings-list* nil)

;; Called on each slot to generate the appropriate strings list
(defun format-function (schema slot form inherited valid real-value
			       types bits indent limit)
  (declare (ignore types indent limit))
  (let* ((value (if (and (formula-p schema)
			 (eq slot :lambda))
		  ;; special hack for formula values
		  (format NIL " = ~a"
			  (write-to-string real-value
					   :pretty T))
		  ;; else regular
		  ;;(format NIL " = ~s" real-value)
		  (concatenate 'string " = " (write-to-string real-value))))
	 (parameter? (unless (formula-p schema)
		       (member slot (g-value schema :parameters))))
	 (text-color (if parameter? opal:red opal:black))
	 (constant? (kr::is-constant bits))
	 (l (list (list value bold-font text-color))))
    (when inherited
      (push inherited-marker l))
    (when form
      (push (if valid valid-marker invalid-marker) l)
      (push (list (format NIL " - ~s" form)
		  (if inherited inherited-formula-font
		      formula-font)
		  text-color)
	    l))
    (when constant? (push constant-marker l))
    (push (list (format NIL "~s" slot) opal:default-font text-color) l)
    (push l *strings-list*)))

;; calls the format function except for slots that have already been
;; printed because they are in the *extra-slot-list*
(defun Elim-extra-FUNCTION (schema slot form inherited valid real-value
			    types bits indent limit)
  (unless (member slot *extra-slot-list*)
    (format-function schema slot form inherited valid real-value
		     types bits indent limit)))

(defun Create-Multi-Font-String-For-PS (schema inherit? window
					       &key (reset-error-msg T)
					       (push-object T)
					       (reset-extra-slots T))
  (if (or (formula-p schema)
	  (schema-p schema))
    (let* ((ps-pop (g-value window :aggregate))
	   (string-obj (g-value ps-pop :string-obj))
	   string-list)
      (when reset-extra-slots
	(s-value ps-pop :extra-slots-to-show NIL))
      (setf *strings-list* nil)
      (setf *extra-slot-list* (g-value ps-pop :extra-slots-to-show))
      (call-on-ps-slots schema #'Elim-extra-FUNCTION :inherit inherit?)
      (setq *strings-list* (nreverse *strings-list*))
      (dolist (sl *extra-slot-list*)
	(kr::call-on-one-slot schema sl #'FORMAT-FUNCTION))
      (setq string-list *strings-list*)
      (opal:set-cursor-visible string-obj NIL)
      (s-value string-obj :visible T)
      (opal:set-text string-obj string-list)
      (s-value string-obj :visible T)
      (s-value window :height (+ (g-value string-obj :top)
				 (g-value string-obj :height)
				 5))
      (Set-Up-Ps-Pop-For-Obj window ps-pop schema inherit? reset-error-msg
			     push-object)
      (opal:update window)

      ;; XXX Work around --- somehow the height doesn't get set right
      ;; in the above. Re-setting it after update corrects this.
      (s-value window :height (+ (g-value string-obj :top)
				 (g-value string-obj :height)
				 5))
      (opal:update window)

      ;; Next is a special hack to make sure can interact with the
      ;; inspector windows even if there is a modal window visible.  It
      ;; is probably OK not to re-enable the modal-ness after finished
      ;; inspecting.
      (setq inter::*Visible-Modal-Windows* NIL) 
      )
    ;; else not a schema
    (Db-Show-Error window
		   (format NIL "Object ~s is invalid; might be destroyed"
			   schema))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
** fix to use hourglass cursor
** use icons in text stream
** INSPECTOR and HELP go into existing window if visible, controlled
         by a global variable
** search for string in window
|#
