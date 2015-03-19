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
;;;  This is the utilities file for the save and load gadget.
;;;  The gadget files (save-gadget.lisp & load-gadget.lisp) contain
;;;  just the gadgets.  This file contains all the functions that
;;;  are called by the gadgets.
;;;
;;; CHANGE LOG:
;;; 09/17/03 Robert Goldman - Modified PUT-FILENAMES-IN-MENU to make a 
;;;            happier fit for CMUCL (by turning off some of the extensions
;;;            to DIRECTORY).
;;; 08/20/98 Fred Gilham - Wrote CMUCL version of real-path that made better
;;;            substitutions for "." and "" pathnames.
;;; 10/26/95 RGA --- Added ignore errors on (truename Prev-Dir).  This
;;;          should eliminate errors when previous directory is not a
;;;          valid pathname. 
;;; 12/06/94 Bruno Haible - Commented out :directories keyword for CLISP 
;;; 07/29/94 Marty Geier - Wrote real-path function to compute REAL-PATH
;;;            needed for allegro directory bug, implemented in
;;;            update-file-menu.
;;; 07/27/94 Marty Geier - switched the order of checking for directories 
;;;            placed opal:directory-p check before gu:probe-directory in 
;;;            function check-load-filename since the later function crashes
;;;            in allegro on .. path and derivations there of.
;;; 06/16/94 Marty Geier - Re-did the pathname system so that it would support
;;;            Macintosh file paths, by adding #+ functions for : cases.  
;;;            Also made small change in select file function to support mac
;;;            paths.  Lastly, took out positive case in update-file-menu that
;;;            deletes all subdirectory references on the end of a file
;;;            (i.e., asdf::::: becomes asdf, likewise in unix) this is needed
;;;            to support macintosh directory changing to previous directory
;;;            (::) and seemed to be redundent anyhow.
;;; 06/16/94 Marty Geier - corrected a "believed" mistake in the next line,
;;;            changed :prev-dir value to "::" instead of "" for mac
;;; 05/05/94 Andrew Mickish - Gave :prev-dir value of "" on Mac
;;; 01/12/94 Andrew Mickish - xlib:drawable-plist ---> opal:drawable-to-window
;;; 10/06/93 Andrew Mickish - :background-color ---> :foreground-color
;;; 09/28/93 Rajan Parthasarathy - In File-Menu-Selection, added
;;;            directory-namestring call to check for "."
;;; 08/23/93 Andrew Mickish - Probe-file ---> gu:probe-directory
;;; 08/15/93 Rajan Parthasarathy - Created new directory-p function and
;;;            put it in opal.
;;; 08/11/93 Rajan Parthasarathy - Fixed small bug
;;; 07/26/93 James Landay - changed Display-Save-Gadget to change 
;;;            directories if the init-filename passed in contained one.
;;; 07/08/93 Rajan Parthasarathy - In Update-File-Menu, checked :prev-dir
;;;            for "" so that you can force it to update by setting
;;;            :prev-dir to "".
;;; 07/02/93 Rajan Parthasarathy - Added hourglass cursors, added ".."
;;;            to list of file names, added second optional parameter to
;;;            save-file-if-wanted to allow user to specify the string
;;;            in the query box.  Made save-file-if-wanted, display-save-
;;;            gadget-and-wait, and display-load-gadget-and-wait return
;;;            the value of the gadgets :selection-function when a save
;;;            or load is initiated.
;;; 06/28/93 Rajan Parthasarathy - In Check-Load-Filename, made probe-file
;;;            check for filename in correct directory.
;;; 04/17/93 Andrew Mickish - In Check-Filename, Lucid requires that we check
;;;            the possible return value of "" from directory-namestring.
;;; 04/05/93 Rajan Parthasarathy - In Check-Filename, must consider both
;;;            possible return values from directory-namestring: "." and "./"
;;; 04/05/93 Andrew Mickish - Removed with-demon-enabled call from :initialize
;;;            method
;;; 01/13/93 Rajan Parthasarathy - Fixed a typo
;;; 12/14/92 Rajan Parthasarathy - In Check-Filename, "." --> "./"
;;; 12/10/92 Andrew Mickish - *drawable-to-window-mapping* --> *garnet-windows*
;;; 10/14/92 Andrew Mickish - Moved code for Cut-Long-Strings into initialize
;;;            method, removed *CURRENT-SAVE-LOAD-GADGET*.
;;; 09/02/92 Andrew Mickish - Changed formula to o-formula in initialize method
;;; 08/20/92 Andrew Mickish - Moved Save-Load-Gadget-Destroy here from
;;;            save-gadget.lisp and motif-save-gadget.lisp
;;; 08/12/92 Rajan Parthasarathy - Created
;;;
;;; Known bugs:
;;;
;;; If using main-event-loop-process, Save-File-If-Wanted goes into
;;; event lock.  Without process, works great.
;;;

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(DISPLAY-SAVE-GADGET DESTROY-SAVE-GADGET HIDE-SAVE-GADGET
	    DISPLAY-LOAD-GADGET DESTROY-LOAD-GADGET HIDE-LOAD-GADGET
	    display-save-gadget-and-wait display-load-gadget-and-wait
	    save-file-if-wanted)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function gets called whenever the user hits
;;; either the save or the cancel function in the save-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Default-Save-Function (gadget value)
  (let* ((save-gad (g-value gadget :parent))
	 (dummy NIL))
    
    (if (equalp value (cadr (g-value save-gad :button-panel-items)))
	(progn
	  (hide-save-gadget save-gad)                ;; Cancel button was hit
	  (setf dummy :CANCEL)
	  (when (g-value save-gad :waiting)
	    (inter:interaction-complete dummy))
	  )
	
;;; The idea here is to check to see if the filename is blank
;;; and simply calling the :save-function
	(if (string/= (g-value save-gad :file-input :value) "")
	    (Check-Save-Filename save-gad
				 (g-value save-gad :file-input :value))
	    (inter:beep)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the default save function.  It first checks to see
;;; if the filename already exists.  If it does, it pops the
;;; query gadget and asks the user if he wants to save or abort
;;; If the user selects save, it uses write-gadget to write out
;;; the :top-aggregate slot.  Then, it hides the window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Check-Save-Filename (save-gad filename)
  (let* ((Prev-Dir (g-value save-gad :prev-dir))
	 (full-fn (merge-pathnames Prev-Dir filename))
	 (qg (g-value save-gad :query-window))
	 (dummy NIL))
    (if (AND ;;Already exists
	     (probe-file (merge-pathnames filename PREV-DIR))
	     ;;User wants it checked
	     (g-value save-gad :check-filenames-p))
	(progn
	  (s-value qg :selection-function #'(lambda (gadget val)
					      (declare (ignore gadget))
					      (when (string=
						     (first (g-value save-gad
								     :query-buttons))
						     val)
						(s-value save-gad :returned-val
							 (kr-send
							  save-gad :selection-function
							  save-gad
							  full-fn))
						(hide-save-gadget save-gad))
					      ))
	  (unless
	      (string= (second (g-value save-gad :query-buttons))
		       (gg:display-query-and-wait qg
						  (g-value save-gad :query-message)
						  (g-value save-gad :query-buttons)))
	    (when (g-value save-gad :waiting)
	      (inter:interaction-complete (g-value save-gad :returned-val))))
	  )
	(progn
	  (setf dummy
		(kr-send save-gad :selection-function save-gad full-fn))
	  (hide-save-gadget save-gad)
	  (when (g-value save-gad :waiting)
	    (inter:interaction-complete dummy))
	  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function checks to see if the filename is valid
;;; Invalid filenames are existing directory names, or
;;; typing in a directory name (eg. ~/rajan/foo/baz/save.lisp)
;;; If the name is invalid, we give a beep and tell the user
;;; to re-edit the string.  So you gotta keep typing and
;;; typing till you type a valid filename
;;;
;;; NOTE: the way to make the cursor stay there when a bad
;;; filename is typed is to modify the interactors running
;;; action so that if it gets a return, it checks the
;;; filename, and if there's an invalid filename, it beeps/
;;; Otherwise it'll go on to (call-prototype-method)  However
;;; it was too much of a hassle to add that.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Check-Filename (gadget value)
  (let* ((save-gad (g-value gadget :parent))
	 (save-win (g-value gadget :window))
	 (prev-dir (g-value save-gad :prev-dir)))
  (UNLESS (string= "" value)
    (when (OR (gu:directory-p (concatenate 'string PREV-DIR value))
	      (not (or (string= (directory-namestring value) ".")
		       (string= (directory-namestring value) "./")
		       (string= (directory-namestring value) ""))))
      (inter:beep)
      (s-value gadget :value "")
      (opal:update save-win)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function is a helper function for update-file-menu.
;;; it takes any pathname and returns its TRUE value
;;; ie... /afs/cs/project/ -> /afs/cs/project
;;;  /afs/cs/project/../ -> /afs/cs/
;;;  /.. -> /
;;; This function is nessesary for allegro which crashes on the
;;; .. and /../ cases and derivations thereof
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#-(and)
(defun real-path (path)
  (let* ((trimmed (string-right-trim "/" path))
         (len (length trimmed))
         (prev-dir-test (or (string= trimmed "..")
                            (not (>= len 3))
                            (string= (subseq trimmed (- len 3) len) "/..")))
         (truncat (remove #\/ (string-right-trim "." trimmed) :count 1
                                                              :from-end t))
         (last/ (position #\/ truncat :from-end t)))


    (cond ((not prev-dir-test) path)
          ((= (length truncat) 0) "/")
          (t (remove-if #'(lambda (x) (declare (ignore x)) t)
			truncat
			:start (+ last/ 1)
			:end (length truncat))))))


#+(and)
(defun real-path (path)
  (namestring (or (ignore-errors (truename path))
		  *default-pathname-defaults*)))

#-(and)
(defun real-path (path)
  (cond ((or (string= path "") (string= path "."))
	 (namestring *default-pathname-defaults*))
	(t path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function updates the file menu.  It is called
;;; whenever the directory is changed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Update-File-Menu (gadget value)
  (let* ((save-gad (g-value gadget :parent))
	 (save-win (g-value save-gad :window))
	 (prev-dir (g-value save-gad :prev-dir))
	 (val (real-path value)))
    (if (gu:probe-directory (directory-namestring val))
     	(let ((dir-name NIL))
	  (if (gu:directory-p val)
	      (progn
		(setf dir-name (string-right-trim "/" val))
		(setf dir-name (concatenate 'string
					    dir-name "/"))
		)
	      (progn
		(setf dir-name (directory-namestring val))
		(s-value (g-value save-gad :file-input) :value
			 (file-namestring val))
		))
	  (s-value (g-value save-gad :dir-input) :value
		   (directory-namestring (truename dir-name)))
	  
	  ;; The above crap gets the directory name of the current dir
	  ;; The below (unless...) sets the :items slot of the menu
	  ;; to be the contents of the directory

	  (when
	      (OR
	       (equal Prev-Dir "") ;; This is so that you can reload the dir
	                           ;; contents after you change it
	       (NOT (equal (truename dir-name)
			   ;; RGA added ignore-errors to get out of
			   ;; loosing situation where previous
			   ;; directory is invalid.
			   (ignore-errors (truename Prev-Dir)))))  ;; Same dir
	    (s-value save-gad :prev-dir dir-name)
	    (s-value (g-value save-gad :message) :string
		     (g-value save-gad :message-string))
	    (opal:update save-win)
	    
	    (if (NOT (g-value save-gad :window :visible))
		(put-filenames-in-menu save-gad dir-name)
		(opal:with-hourglass-cursor
		    (put-filenames-in-menu save-gad dir-name)))))
	
	;; When the directory is invalid, it will beep and put the previous
	;; directory there
	(progn
	  (s-value (g-value save-gad :dir-input) :value
		   (directory-namestring
		    (let ((tn (ignore-errors (truename prev-dir))))
		      ;; RGA added ignore-errors to get out of
		      ;; loosing situation where previous
		      ;; directory is invalid.
		      (if tn tn *default-pathname-defaults*))))
	  (inter:beep)))
    
    (opal:update save-win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function gets the directory and puts it in the file
;;; menu.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *wild-pathname* (make-pathname :name :wild :type :wild))

(defun put-filenames-in-menu (save-gad dir-name)
  (let ((dir-list nil)
	(file-list NIL)
	(dir (directory (merge-pathnames dir-name *wild-pathname*)
			;; cmucl has a few keyword options that
			;; default to values we might not like...
			#+cmu :check-for-subdirs #+cmu NIL
			#+cmu :follow-links #+cmu NIL
			))
	(save-win (g-value save-gad :window)))
    (dolist (pathname dir)
      (let ((filepart (make-pathname :directory nil :defaults pathname)))
	(if (equal filepart #P"")
	    ;; It's a directory. Extract the last component and
	    ;; turn it into a relative directory pathname. This
	    ;; will put a slash at the end of the namestring.
	    ;; I'm hoping this will be portable.
	    (push 
	     (namestring
	      (make-pathname
	       :directory (cons :relative (last (pathname-directory pathname)))))
	     dir-list)
	    ;; It's a file.
	    (push (namestring filepart) file-list))))
    
    (setf file-list
	  (append(sort dir-list #'(lambda (x y) (string< x y)))
		 (sort file-list #'(lambda (x y) (string< x y)))))
    (push ".." file-list)
    (s-value (g-value save-gad :file-menu) :items file-list)
    (s-value (g-value save-gad :file-menu) :selected-ranks NIL)
    (s-value (g-value save-gad :message) :string "")
    (s-value (g-value save-gad :file-menu :scroll-bar) :value 0)
    (opal:update save-win)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function is called when an object in the file-menu is
;;; selected.  It first converts the object so that it's full path-
;;; name is there, and then calls Update-File-Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun File-Menu-Selection (gadget value)
  (let ((prev-dir (g-value gadget :parent :prev-dir)))
    (Update-File-Menu gadget (merge-pathnames (g-value value :item)
					      (truename Prev-Dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function gets called whenever the user hits
;;; either the load or the cancel function in the load-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Default-Load-Function (gadget val)
  (let* ((load-gad (g-value gadget :parent))
	 (value (g-value load-gad :file-input :value))
	 (dummy NIL))

    (if (equalp val
		(second (g-value load-gad :button-panel-items)))
	(progn
	  (hide-load-gadget load-gad)  ;; Cancel button was hit
	  (setf dummy :CANCEL)
	  (when (g-value load-gad :waiting)
	    (inter:interaction-complete dummy))
	  )

	(if (or (equalp value "")
		(NOT (check-load-filename (g-value load-gad :file-input) value)))
	    (inter:beep)
	    (progn
	      (setf dummy (Do-Load-File load-gad value))
	      (when (g-value load-gad :waiting)
		(inter:interaction-complete dummy)))
	      ))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This simply loads the file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Do-Load-File (load-gad filename)
  (hide-load-gadget load-gad)
  (kr-send load-gad :selection-function load-gad
	   (merge-pathnames (g-value load-gad :prev-dir) filename))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function checks to see if the filename is valid
;;; Invalid filenames are existing directory names, or
;;; typing in a directory name (eg. ~/rajan/foo/baz/load.lisp)
;;; If the name is invalid, we give a beep and tell the user
;;; to re-edit the string.  So you gotta keep typing and
;;; typing till you type a valid filename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Check-Load-Filename (gad value)
  (let* ((gadget (g-value gad :parent))
	 (dir (g-value gadget :prev-dir))
	 (valid-p T))
    (when (g-value gadget :check-filenames-p)
      (let* ((load-win (g-value gadget :window)))
	(UNLESS (string= "" value)
	  (when (OR (gu:directory-p value)  ; this half of Or, MUST come 1st
		    (not (probe-file (merge-pathnames value dir))))
	    (inter:beep)
	    (setf valid-p NIL)
	    (s-value gad :value "")
	    (opal:update load-win)))))
    valid-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This puts the save-gadget & load-gadget in its own window slot. 
;;; It also does some other basic initializing, like creating a query-gadget
;;; and it creates the return interactor, which comes on when you
;;; hit the return key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SAVE-LOAD-GADGET-INITIALIZE (gad)
  (kr-send opal:aggregadget :initialize gad)
  (let ((window (create-instance NIL inter:interactor-window
		  (:parent
		   (o-formula (gvl :gadget :parent-window)))
		  (:gadget gad)
		  (:background-color (o-formula
				      (when (or
					     (gvl :gadget :motif-save-gadget-p)
					     (gvl :gadget :motif-load-gadget-p))
					(gvl :gadget :foreground-color))))
		  (:title (o-formula (gvl :gadget :window-title)))
		  (:width (o-formula (+ (gvl :gadget :width) 20)))
		  (:height (o-formula (+ (gvl :gadget :height) 45)))
		  (:visible NIL)))
	(aggregate (create-instance NIL opal:aggregate)))
    (s-value window :aggregate aggregate)
    (opal:update window)
;    (with-demon-enabled #'inter::inter-update-slot-invalidated
      (opal:add-component aggregate gad)
;      )
    
;; The following s-values are set here in case the user has a
;; :window-left slot that depends on something in it's own window
    (if (or
	 (g-value gad :save-gadget-p)
	 (g-value gad :motif-save-gadget-p))
	(s-value gad :query-window
		 (create-instance NIL (g-value gad :type-of-query)
		   (:save-load-gadget gad)
		   (:foreground-color (o-formula (gvl :save-load-gadget
						      :foreground-color)))
		   (:parent-window (if (g-value gad :parent-window)
				       (g-value gad :parent-window)

				       window)))))

    (s-value gad :prev-dir "../")
    ;; Each save-gadget must have its own customized item-to-string-function.
    ;; We are only allowed to pass one parameter to the i-to-s fn, so how do
    ;; we accomodate the different max-item-width values in different gadgets?
    ;; Answer: generate a new function for each instance, with a different
    ;; max-width value inside each function.
    (let* ((file-menu (g-value gad :file-menu))
	   (max-width (g-value file-menu :max-item-width))
	   (font (g-value file-menu :item-font)))
      (labels ((Cut-Long-Strings (str)
		 (if (>= (opal:string-width font str) max-width)
		     ;; If a string is too long, remove one char and try again
		     (Cut-Long-Strings
		      (string-right-trim
		       (string (elt str (1- (length str)))) str))
		     str)))
	(s-value file-menu :item-to-string-function #'Cut-Long-Strings)))
    (s-value gad :return-inter
	     (create-instance NIL inter:button-interactor
	       (:window window)
	       (:start-where T)
	       (:the-button (g-value gad :ok-cancel-buttons))
	       (:continuous NIL)
	       (:start-event #\RETURN)
	       (:final-function #'(lambda (inter obj)
				    (declare (ignore obj))
				    (let ((g (g-value inter :the-button)))
				      (if (or
					   (g-value gad :save-gadget-p)
					   (g-value gad :motif-save-gadget-p))
					  (default-save-function
					      g
					      (first (g-value g :button-panel-items)))
					  (default-load-function
					      g
					      (first (g-value g :button-panel-items)))
					  ))))))

    (Update-File-Menu (g-value gad :file-menu)
		      (g-value gad :initial-directory))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This destroys the save gadget by destroying its window.  Since
;;; the save gadget is inside the window, it'll be destroyed too
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Save-Load-Gadget-Destroy (gad &optional erase)
  (let ((agg (g-value gad :parent))
	(window (g-value gad :window)))
    (if agg
	(opal:remove-component agg gad))
    ;; make sure window isn't already being destroyed
    (if (and window
	     (schema-p window)
	     (opal:drawable-to-window (get-local-value window :drawable)))
	(opal:destroy window))
    (call-prototype-method gad erase)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function displays the gadget by setting its window to be
;;; visible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-save-gadget (gadget &optional init-filename wait-p)
  (let ((win (g-value gadget :window))
	(dummy NIL))

    (opal:with-hourglass-cursor
	(progn
	  (when (or (g-value gadget :motif-save-gadget-p)
		    (g-value gadget :save-gadget-p))
	    (with-constants-disabled
		(s-value (g-value gadget :query-window :button) :h-align :center)))
	  
	  (s-value win :left
		   (o-formula (gvl :gadget :window-left)))
	  (s-value win :top
		   (o-formula (gvl :gadget :window-top)))
	  
	  ;; This updates the file menu in case any new files have
	  ;; been added since the last time the gadget was displayed
	  (let ((temp (g-value gadget :prev-dir)))

	    ;; change directories if one was passed in
	    (when (and init-filename (directory-namestring init-filename))
	      (setf temp (directory-namestring init-filename)))

	    (s-value gadget :prev-dir "")
	    (Update-File-Menu (g-value gadget :file-menu)
			      temp)
	    (s-value gadget :prev-dir temp))
	  
	  (when init-filename
	    (s-value (g-value gadget :file-input) :value
		     (file-namestring init-filename)))
	  
	  (s-value win :visible T)
	  (s-value win :modal-p
		   (o-formula (gvl :gadget :modal-p)))
	  (opal:raise-window win)
	  (opal:update win)))
    (when wait-p
      (s-value gadget :waiting T)
      (setf dummy (inter:wait-interaction-complete)))
    dummy
    ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This displays the save gadget and waits for it to complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-save-gadget-and-wait (gadget &optional init-filename)
  (display-save-gadget gadget init-filename T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function hides the gadget by setting the :visible slot of
;;; its window to be NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hide-save-gadget (gadget)
  (s-value (g-value gadget :window) :visible NIL)
  (s-value (g-value gadget :file-menu) :selected-ranks NIL)
  (opal:update (g-value gadget :window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function displays the gadget by setting its window to be
;;; visible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-load-gadget (gadget &optional init-filename)
  (display-save-gadget gadget init-filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This displays the load gadget and waits for it to complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-load-gadget-and-wait (gadget &optional init-filename)
  (display-save-gadget-and-wait gadget init-filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This function hides the gadget by setting the :visible slot of
;;; its window to be NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hide-load-gadget (gadget)
  (hide-save-gadget gadget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save-File-If-Wanted pops up a query gadget asking "Save file first?"
;;; If "No" is selected, it simply returns.  If "Cancel" is selected,
;;; If "Yes" is selected, it displays the save gadget.  The saving is
;;; done by the selection function of the save gadget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Save-File-If-Wanted (save-gad &optional init-filename (query-string "Save file first?"))
  (let ((q-box (g-value save-gad :query-window))
	(dummy NIL))
    (with-constants-disabled
	(s-value (g-value q-box :button) :h-align :center))
    (s-value q-box
	     :selection-function
	     #'(lambda (g v)
		 (declare (ignore g))
		 (cond
		   ((equal v "Yes")
		    (setf dummy
			  (gg:display-save-gadget-and-wait save-gad init-filename)))
		   ((equal v "Cancel") (setf dummy :CANCEL))
		   (t (setf dummy :NO)))
		 ))
    (opal:raise-window (g-value q-box :window))
    (gg:display-query-and-wait q-box query-string '("Yes" "No" "Cancel"))
    dummy
    ))
