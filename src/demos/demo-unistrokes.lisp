;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-UNISTROKES; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; demo-unistrokes.lisp 
;;;
;;; This is DEMO-UNISTROKES. It is a sample application that uses
;;; gestures trained by AGATE (the Garnet Gesture training application)
;;; to implement a simple text editor that uses Dave Goldberg's (Xerox 
;;; PARC) Unistrokes (see INTERCHI '93 paper) for alphabetic input.
;;; It also uses some simple gestures to edit the text.
;;;
;;; Designed and implemented by James A. Landay 
;;;
;;; Future work: 
;;;
;;; Known bugs:

#|
==================================================================
Change log:
     05/30/94 Marty Geier - changed main window position in do-go
     09/16/93 James Landay   - started 
==================================================================
|#


(in-package :DEMO-UNISTROKES)

;; objects created in do-go
(declaim (special TOP-WIN MAIN-MENU GESTURE-INTER ERROR-DIALOG QUIT-DIALOG
		  SAVE-DIALOG LOAD-DIALOG TEXT TEXT-WIN UNISTROKE-ICON-WIN
		  UNISTROKE-ICON-PROTO UNISTROKE-ICON-AGGL
		  PUNCTUATION-ICON-AGGL CANVAS-WIN INSTRUCTIONS-TEXT RED-LINE-4
		  ORANGE-LINE-1 ORANGE-LINE-2 TEXT-LABEL ALPHABET-LABEL
		  CANVAS-LABEL PUNCTUATION-ICON-PROTO TOP-AGG))

;; Load motif stuff, unless already loaded
(defvar TRAIN-APP-INIT
  (progn
    (dolist (gadget '("motif-text-buttons-loader"
		      "motif-scrolling-labeled-box-loader"
		      "motif-radio-buttons-loader"
		      "motif-scrolling-window-loader"
		      "motif-save-gadget-loader"
		      "motif-error-gadget-loader"))
      (common-lisp-user::garnet-load (concatenate 'string "gadgets:" gadget)))

    (common-lisp-user::garnet-load "opal:multifont-loader")
    
    ;; load gesture-loader
    (common-lisp-user::garnet-load "gesture:gesture-loader")))

;; global variables definitions

(defparameter *unistroke-classifier* NIL) ;; unistroke classifier
(defparameter *unistroke-examples*  NIL)  ;; unistroke examples
(defparameter *saved* T)        ;; has current text been saved
                                ;; since last change? (not dirty yet)
(defparameter *last-filename* NIL)    ;; last filename saved or loaded 
(defvar *color-p* (g-value opal:color :color-p)) ;; is this a color screen?

(defparameter *example-icon-size* 60) ;; size of square gesture example icons
(defparameter *punctuation-icon-size* 40) ;; size of square punctuation icons
(defvar *cut-buffer*)                 ;; last text string deleted
(defvar *max-name-chars*)             ;; max length of labels for class icons
(defvar *max-name-height*)            ;; max height for labels in class icons


;; do-error displays the ERROR-DIALOG with the given string displayed.
;; Does not continue until the user presses the OK button.
;;
;; Parameters:
;;     string - message to display
;;
(defun do-error (string)
  (garnet-gadgets:display-error-and-wait ERROR-DIALOG string))


;; add-punctuation adds the proper punctuation character to the TEXT
;; object when the user clicks on the punctuation icon.
;; 
;; Parameters:
;;     obj-over - object clicked on
;;
(defun add-punctuation (obj-over)
  (let ((punc (char (string (nth (g-value obj-over :rank) 
				 (g-value PUNCTUATION-ICON-AGGL :items))) 0)))

    (if (equal punc #\c)
	(setf punc #\newline))
    (opal:add-char TEXT punc)))


;; class-name-equal is used to test whether a class in the class-agg items 
;; list is equal to the class we are now considering.
;;
;; Parameters:
;;    name - name to test
;;
(defun class-name-equal (name)
  #'(lambda (&rest arguments)
      (equal (string-upcase (string-trim '(#\Space) name))
         (string-upcase 
          (string-trim '(#\Space) 
               (apply #'inter::gest-class-name arguments))))))


;; handle-unistroke is called by the gesture interactor after it
;; classifies a gesture.  This is used to interpret the unistroke.
;;
;; Parameters:
;;    inter, first-obj-over, attribs - ignored
;;    class-name - name of recognized gesture (or NIL if unrecognized)
;;    points     - points in gesture 
;;    nap        - non-ambiguity of gesture
;;    dist       - distance from class-name
;;
(defun handle-unistroke (inter first-obj-over class-name attribs
                         points nap dist)
  (declare (ignore first-obj-over attribs points nap dist))
  (let ((selected (g-value UNISTROKE-ICON-AGGL :selected))) 
    (if (null class-name)
        (progn
          ;; de-sellect the selected icon in class agglist
          (when (schema-p selected)
	    (s-value selected :selected NIL)
	    (s-value UNISTROKE-ICON-AGGL :selected NIL))
	  (format t "Unrecognized"))
        (progn
          ;; select the icon in agglist corresponding to gesture
          (let* ((name (write-to-string class-name :escape nil))
             (index (position-if (class-name-equal name)
                     (g-value UNISTROKE-ICON-AGGL :items)))
             (selected-icon 
              (if index
		  (nth index (g-value UNISTROKE-ICON-AGGL :components))
		nil)))

	    ;; check to see if the class was found (can't happen error)
	    (if (null index)
		(do-error 
		 (format NIL "ERROR: Class named ~s is ~a~%~a~%~a" 
			 name 
			 " not present in current classifier."
			 "This is a bug, please report to the developers."
			 "Press OK to continue."))

	      (progn
		(if (schema-p selected)
		    (s-value selected :selected NIL))
		;; check for blank space (returned as "-space-")
		(if (equal (char (string class-name) 0) #\-)
		    (opal:add-char TEXT #\space)
		  (if (equal (g-value inter :start-char) :LEFTDOWN)
		      (opal:add-char TEXT (char-downcase 
					   (char (string class-name) 0)))
		      (opal:add-char TEXT (char (string class-name) 0))))
		(s-value selected-icon :selected T)
		(setf *saved* NIL)
		(s-value UNISTROKE-ICON-AGGL :selected selected-icon))))))))


;; handle-edit-gesture is called by the edit gesture interactor after it
;; classifies a gesture. This is used to change the cursor position in
;; the text window or to delete text.
;;
;; Parameters:
;;    inter, first-obj-over, attribs - ignored
;;    class-name - name of recognized gesture (or NIL if unrecognized)
;;    points     - points in gesture 
;;    nap        - non-ambiguity of gesture
;;    dist       - distance from class-name
;;
(defun handle-edit-gesture (inter first-obj-over class-name attribs
			    points nap dist)
  (declare (ignore inter first-obj-over points nap dist))
  (case class-name
    (:CUT
     ;; delete the text under the gesture by marking it with a selection
     (opal:set-cursor-to-x-y-position TEXT
        (inter:gest-attributes-startx attribs)
	(inter:gest-attributes-starty attribs))
     (opal:set-selection-to-x-y-position TEXT 
        (inter:gest-attributes-endx attribs)
	(inter:gest-attributes-endy attribs))
     (opal:toggle-selection TEXT T)
     (setf *cut-buffer* (opal:delete-selection TEXT))
     (setf *saved* NIL))
     
    (:PASTE
     ;; paste the cut buffer to the peak of the paste gesture
     (opal:set-cursor-to-x-y-position TEXT 
        (+ (inter:gest-attributes-minx attribs)
	   (floor (- (inter:gest-attributes-maxx attribs)
		     (inter:gest-attributes-minx attribs)) 2))
	(inter:gest-attributes-miny attribs))
     (opal:insert-text TEXT *cut-buffer*)
     (setf *saved* NIL))

    (:SET-CURSOR
     ;; set cursor to location of click
     (opal:set-cursor-to-x-y-position TEXT 
	(inter:gest-attributes-startx attribs)
	(inter:gest-attributes-starty attribs)))

    (otherwise 
     (format T "Unrecognized gesture ...~%~%"))))


;; scale copies the given points and then scales and translates them to 
;; the givin origin. The new points are then returned as a LIST!!!
;;
;; Parameters:
;;     x-factor - amount to scale x coordinate by (i.e. divide by)
;;     y-factor - amount to scale y coordinate by (i.e. divide by)
;;     x-origin - new origin to translate to 
;;     y-origin   
;;     points   - array of points of form [x1 y1 x2 y2 ...]
;;
(defun scale (x-factor y-factor x-origin y-origin points)
  (do* ((pts (copy-seq points))
    (index 0 (+ index 2)))
      ((>= index (length pts)) (coerce pts 'list)) ;; exit clause
        
    ; scale and translate x coord
    (setf (aref pts index) 
      (+ x-origin (ceiling (aref pts index) x-factor)))
    ; scale and translate y coord
    (setf (aref pts (1+ index)) 
      (+ y-origin (ceiling (aref pts (1+ index)) y-factor)))))


;; do-load loads some text from a file. 
;; classifier has not been saved, it will prompt the user to save
;; it.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-load (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (let ((cancel nil))
    (unless *saved*
      (setf cancel (gg:save-file-if-wanted SAVE-DIALOG
					   *last-filename*
					   "Save current file first?")))
    (unless (or (eq cancel :cancel) (eq cancel 'ERROR))
      (gg:display-load-gadget-and-wait LOAD-DIALOG *last-filename*))))


;; load-text-file loads a text file.
;;
;; Parameters:
;;    gadget - ignored
;;    file   - file to load text from
;;
(defun load-text (gadget file)
  (declare (ignore gadget))
  (opal:with-hourglass-cursor
   (opal:set-text 
    TEXT 
    ;; read the text in from the file
    (with-open-file (stream file :direction :input)
		    (do ((str (make-array '(0)
			  :element-type #+lucid 'string-char
			                #+allegro-v4.0 'cltl1::string-char
					#-(or lucid allegro-v4.0) 'character
			  :fill-pointer 0 :adjustable T))
			 (next-char 
			  (read-char stream NIL :eof)
			  (read-char stream NIL :eof)))
			((eq next-char :eof) str)
		      (vector-push-extend next-char str)))))
  (setf *last-filename* file)
  (setf *saved* T)
  (opal:update TEXT-WIN))


;; do-save saves the current text to a file.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-save (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (gg:display-save-gadget-and-wait SAVE-DIALOG *last-filename*))


;; save-text saves the current text to the given file.
;;
;; Parameters:
;;    gadget   - ignored
;;    filename - file to save text to
;;
(defun save-text (gadget filename)
  (declare (ignore gadget))

  (opal:with-hourglass-cursor
   (with-open-file (str filename :direction :output 
			:if-exists :supersede)
		   (format str "~a" (opal:get-string TEXT))))
  (setf *last-filename* filename)
  (setf *saved* T))


;; do-quit calls do-stop when the user hits the quit button. 
;; If the classifier is dirty and hasn't been saved asks the
;; user if they would like to save it first.
;;
;; Parmeters:
;;     gadgets-object (ignored)
;;     item-string (ignored)   
;;
(defun do-quit (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (if *saved*
      (do-stop)                   ;; quit, classifier is not dirty
      (let ((cancel nil))
    
    ;; see if they want to save
    (setf cancel (gg:save-file-if-wanted SAVE-DIALOG
					 *last-filename*))
    (unless (or (eq cancel :cancel) (eq cancel 'ERROR))
      (do-stop)))))


;; do-stop destroys the application window and everything beneath it.
;;
;; Parmeters:
;;     none
;;
(defun do-stop ()
    (opal:destroy TOP-WIN)
    (opal:destroy UNISTROKE-ICON-PROTO)
    (opal:destroy RED-LINE-4)
    (opal:destroy ORANGE-LINE-1)
    (opal:destroy ORANGE-LINE-2)

    (if (fboundp 'Common-Lisp-User::Garnet-Note-Quitted)
	(Common-Lisp-User::Garnet-Note-Quitted "DEMO-UNISTROKES"))

    #-cmu (inter:exit-main-event-loop))


;; do-go creates the necessary windows and Garnet objects, and 
;; then starts the application.
;;
;; Parameters (all keyword):
;;    dont-enter-main-event-loop - if T, don't enter the main event loop
;;    double-buffered-p          - if T, use double buffered windows
;;  
(defun do-go (&key dont-enter-main-event-loop
                   (double-buffered-p #-apple T #+apple NIL))
    (if (and (boundp 'TOP-WIN) (schema-p TOP-WIN))
	(do-stop))

    ;; create top-level window
    (create-instance 'TOP-WIN inter:interactor-window
        (:left 40) (:top 40)
        (:double-buffered-p double-buffered-p)
        (:width 542) (:height 800)
        (:title "GARNET Unistrokes-based Editor")
        (:icon-title "Unistrokes")
        (:background-color opal:motif-gray))

    ;; create the top level aggregate in the window
    (s-value TOP-WIN :aggregate
        (create-instance 'TOP-AGG opal:aggregate
            (:left 0) (:top -2)
            (:width (o-formula (gvl :window :width)))
            (:height (o-formula (gvl :window :height)))))

    ;; need to update top-win before instantiating dialos
    (opal:update TOP-WIN)

    ;; create an error dialog
    (create-instance 'ERROR-DIALOG garnet-gadgets:motif-error-gadget
        (:parent-window TOP-WIN))

    ;; create a query dialog for quitting before save
    (create-instance 'QUIT-DIALOG garnet-gadgets:motif-query-gadget
        (:parent-window TOP-WIN)
        (:button-names '("YES" "NO" "CANCEL")))

    ;; create a save dialog
    (create-instance 'SAVE-DIALOG garnet-gadgets:motif-save-gadget
        (:parent-window TOP-WIN)
    (:query-message "replace existing file")
    (:modal-p t)
    (:selection-function #'save-text))

    ;; create a load dialog
    (create-instance 'LOAD-DIALOG garnet-gadgets:motif-load-gadget
        (:parent-window TOP-WIN)
    (:modal-p t) 
    (:selection-function #'load-text))

    ;; create the main menu 
    (create-instance 'MAIN-MENU garnet-gadgets:motif-text-button-panel
        ;; can't be constant since we want to center it!
        (:items '(
            ("Load Text" do-load)
            ("Save Text" do-save)
            ("Quit" do-quit)))
        (:left (o-formula (- (round (gvl :parent :window :width) 2)
                 (round (gvl :width) 2))))
        (:top 5)
        (:direction :horizontal)
        (:h-align :center))

    (opal:update TOP-WIN) 

    ;; create the text object for displaying entered text
    (create-instance 'TEXT opal:multifont-text
        (:strings nil)
	(:current-font (opal:get-standard-font :fixed :roman :large))
	(:background-color opal:white)
	(:text-width 500)
        (:word-wrap-p T))

    (create-instance 'TEXT-LABEL opal:text
        (:string "Text")
	(:font (opal:get-standard-font :serif :bold :medium))
	(:left (o-formula (- (round (gvl :parent :window :width) 2) 
			     (round (opal:string-width (gvl :font)
						       (gvl :string)) 2))))
	(:top (o-formula (+ (g-value MAIN-MENU :top)
			    (g-value MAIN-MENU :height) 10))))

    ;; create a scrolling window to hold the text obj
    (create-instance 'TEXT-WIN
                     garnet-gadgets:Motif-Scrolling-Window-With-Bars
        (:left 0)
	(:top (o-formula (+ (g-value TEXT-LABEL :top)
			    (g-value TEXT-LABEL :height) 5)))
	(:h-scroll-bar-p NIL)
	(:width (o-formula (gvl :parent-window :width) 150))
	(:height 200)
	(:total-height 10000)
	(:v-scr-incr (opal:string-height (g-value TEXT :current-font) "T"))
	(:foreground-color opal:white)
	(:double-buffered-p double-buffered-p)
	(:parent-window TOP-WIN))

    (opal:update TEXT-WIN)  ;; update scrolling window after creation	
    (opal:add-component (g-value TEXT-WIN :inner-aggregate) TEXT)
    (opal:set-cursor-visible TEXT T)

    ;; create a gesture interactor to allow editing of the text window
    (create-instance 'TEXT-GESTURE-INTER inter:gesture-interactor
        (:window (g-value TEXT-WIN :inner-window))
        (:start-where (list :in (g-value TEXT-WIN :inner-window)))
        (:running-where (list :in (g-value TEXT-WIN :inner-window)))
        (:start-event :any-mousedown)
        (:min-non-ambig-prob .7)
        (:max-dist-to-mean 100)
        (:classifier  
	 (inter:gest-classifier-read 
	  (merge-pathnames "demo-unistrokes-edit.classifier"
			   common-lisp-user::Garnet-Gesture-Data-Pathname)))
        (:final-function #'handle-edit-gesture))

    ;; use red-line to show gesture icon currently selected
    (create-instance 'red-line-4 opal:line-style
        (:foreground-color (if *color-p* opal:red opal:black))
        (:line-thickness 4))

    ;; use orange-line to show direction of gestures
    (create-instance 'orange-line-1 opal:line-style
        (:foreground-color (if *color-p* opal:orange opal:black))
        (:line-thickness 1))

    (create-instance 'orange-line-2 opal:line-style
        (:foreground-color (if *color-p* opal:orange opal:black))
        (:line-thickness 2))

    (setf *max-name-chars*
      (truncate *example-icon-size* 
            (opal:string-width opal:default-font "W")))
    (setf *max-name-height*
      (opal:string-height opal:default-font "T"))
    
    (create-instance 'ALPHABET-LABEL opal:text
        (:string "Alphabet")
	(:font (opal:get-standard-font :serif :bold :medium))
	(:left (o-formula (- (round (gvl :parent :window :width) 2) 
			     (round (opal:string-width (gvl :font)
						       (gvl :string)) 2))))
	(:top (o-formula (+ (g-value TEXT-WIN :top)
			    (g-value TEXT-WIN :height) 10))))

    ;; create the prototype for a unistroke gesture icon
    (create-instance 'UNISTROKE-ICON-PROTO opal:aggregadget
        (:left 0)      ;; set left and top to 0 to avoid inheriting formula
        (:top 0)
        (:height (o-formula (+ (gvl :frame :height)
			       (gvl :gesture-name :height))))
        (:width *example-icon-size*)
        (:parts `((:frame ,opal:rectangle;; frame to box gesture
		     (:width ,*example-icon-size*) 
		     (:height ,*example-icon-size*)
		     (:left ,(o-formula (gvl :parent :left))) 
		     (:top  ,(o-formula (gvl :parent :top)))
		     (:filling-style ,opal:white-fill))

                  ;; name of the gesture class this icon represents
                  (:gesture-name ,opal:text
		     (:left 
		      ,(o-formula 
			(+ (gvl :parent :left)
			   (round (- *example-icon-size*
				     (opal:string-width opal:default-font 
							(gvl :string))) 
				  2))))
		     (:top  ,(o-formula (+ (gvl :parent :top)
					   (gvl :parent :frame :height))))
		     (:string 
		      ,(o-formula
			(let ((base-string
			       (coerce
				(gest-class-name
				 (nth (gvl :parent :rank) 
				      (gvl :parent :parent :items)))
				'string)))
			  (if (<= (length base-string) *max-name-chars*)
			      base-string
			    (string-upcase
			     (subseq base-string 0 *max-name-chars*)))))))

                  ;; for selected gesture icon  
                  (:feedback ,opal:rectangle  
		     (:obj-over NIL);; set by the interactor 
		     (:left ,(o-formula (gvl :parent :left)))
		     (:top ,(o-formula (gvl :parent :top)))
		     (:width ,(o-formula (gvl :parent :width)))
		     (:height ,(o-formula (- (gvl :parent :height)
					     *max-name-height*)))
		     (:visible ,(o-formula (gvl :parent :selected)))
		     (:line-style ,red-line-4)
		     (:draw-function :xor)
		     (:fast-redraw-p T))

                  ;; a polyline of scaled version of the gesture 
                  (:gesture ,opal:polyline 
		     (:x-scale ,(o-formula 
				 (ceiling 510 ; window size trained under!!!!
					  (gvl :parent :frame :width))))
		     (:y-scale ,(o-formula 
				 (ceiling 256 ; window size trained under!!!!
					  (gvl :parent :frame :height))))
		     (:point-list ,(o-formula  
		        (scale (gvl :x-scale) (gvl :y-scale)
			       (gvl :parent :left) (gvl :parent :top)
			       (first
				(gest-class-examples
				 (nth (gvl :parent :rank) 
				      (gvl :parent :parent :items))))))))

		  (:arrow ,opal:arrowhead
		     (:line-style ,orange-line-1)
		     (:length 6)
		     (:diameter 6)
		     (:points ,(o-formula 
				(gvl :parent :gesture :point-list)))
		     (:visible ,(o-formula ;; make sure points exist
				 (and (gvl :head-x) (gvl :head-y))))
		     (:from-x ,(o-formula (first (gvl :points))))
		     (:from-y ,(o-formula (second (gvl :points))))
		     (:head-x ,(o-formula (first (gvl :calc-head))))
		     (:head-y ,(o-formula (second (gvl :calc-head))))
		     ;; need to do this since don't filter repeated points
		     (:calc-head ,(o-formula 
				   (do* ((x1 (first (gvl :points)))
					 (y1 (second (gvl :points)))
					 (i 2 (+ 2 i))
					 (x (nth i (gvl :points))
					    (nth i (gvl :points)))
					 (y (nth (1+ i) (gvl :points))
					    (nth (1+ i) (gvl :points))))
				       ((not (and (eq x1 x) (eq y1 y))) 
					(list x y)))))))))

    ;; create the aggrelist for the unistroke icons 
    (create-instance 'UNISTROKE-ICON-AGGL opal:aggrelist
        (:top  0) 
        (:left 0) 
        (:width (o-formula (gvl :parent :window :width)))
        (:direction :horizontal)
        (:h-spacing 0)
        (:v-spacing 0)
        (:fixed-width-p T)
        (:fixed-width-size *example-icon-size*)
        (:fixed-height-p T)
        (:fixed-height-size (+ *example-icon-size*  *max-name-height*))
	(:rank-margin (o-formula (floor (gv UNISTROKE-ICON-WIN :width)
					*example-icon-size*)))
        (:items NIL)
        (:item-prototype UNISTROKE-ICON-PROTO)
        (:how-set :toggle)
	(:start-where (o-formula (list :element-of (gvl :operates-on)))))

    ;; create a window for the gesture examples icons 
    (create-instance 'UNISTROKE-ICON-WIN inter:interactor-window
        (:left 0)
	(:top (o-formula (+ (g-value ALPHABET-LABEL :top)
			    (g-value ALPHABET-LABEL :height) 5)))
        (:width (o-formula (gvl :parent :width) 150))
        (:height (o-formula (* 3 (+ *example-icon-size*  *max-name-height*))))
        (:double-buffered-p double-buffered-p)
        (:parent TOP-WIN))

    (opal:update UNISTROKE-ICON-WIN)

    (s-value UNISTROKE-ICON-WIN :aggregate
        (create-instance 'UNISTROKE-ICON-WIN-AGG opal:aggregate
            (:left 0) (:top 0)
            (:width (o-formula (gvl :window :width)))
            (:height (o-formula (gvl :window :height)))))

    (create-instance 'CANVAS-LABEL opal:text
        (:string "Canvas")
	(:font (opal:get-standard-font :serif :bold :medium))
	(:left (o-formula (- (round (gvl :parent :window :width) 2) 
			     (round (opal:string-width (gvl :font)
						       (gvl :string)) 2))))
	(:top (o-formula (+ (g-value UNISTROKE-ICON-WIN :top)
			    (g-value UNISTROKE-ICON-WIN :height) 10))))

    ;; create window for entering and displaying full-sized gestures 
    (create-instance 'CANVAS-WIN inter:interactor-window
        (:left 100) 
        (:top (o-formula (+ (g-value CANVAS-LABEL :top)
                            (g-value CANVAS-LABEL :height) 5)))
        (:width (o-formula (- (gvl :parent :width) 200) 150))
        (:height (o-formula (- (gvl :parent :height) (gvl :top) 20)))
        (:double-buffered-p double-buffered-p)
        (:border-width 2)
        (:visible t)
        (:omit-title-bar-p NIL)
        (:parent TOP-WIN))

    (opal:update CANVAS-WIN)

    (s-value CANVAS-WIN :aggregate
        (create-instance 'CANVAS-WIN-AGG opal:aggregate
            (:left 0) (:top 0)
            (:width (o-formula (gvl :window :width)))
            (:height (o-formula (gvl :window :height)))))

    ;; intial instructions as to what gesture window is for
    (create-instance 'INSTRUCTIONS-TEXT opal:text
	;; note: can't be constant (changes)
        (:string "Draw Unistrokes Corresponding to Letters Here")
	(:font (opal:get-standard-font :serif :bold :medium))
	(:left (o-formula (- (round (gvl :parent :window :width) 2) 
			     (round (opal:string-width (gvl :font)
						       (gvl :string)) 2))))
	(:top (o-formula (round (gvl :parent :window :height) 3))))

    ;; initialize globals
    (multiple-value-setq 
	(*unistroke-classifier* *unistroke-examples*)
	(inter:gest-classifier-read
	 (merge-pathnames "demo-unistrokes.classifier" common-lisp-user::Garnet-Gesture-Data-Pathname)))
    (s-value UNISTROKE-ICON-AGGL :items *unistroke-examples*)

    ;; create a gesture interactor to recognize the unistrokes
    (create-instance 'GESTURE-INTER inter:gesture-interactor
        (:window CANVAS-WIN)
        (:start-where (list :in CANVAS-WIN)) 
        (:running-where (list :in CANVAS-WIN))
        (:start-event :any-leftdown :any-middledown)
        (:classifier *unistroke-classifier*)
        (:final-function #'handle-unistroke)
        (:min-non-ambig-prob 0)
        (:max-dist-to-mean 1000000)
	(:start-action
	 #'(lambda (an-interactor object-under-mouse point)
	     ;; erase displayed gesture and instructions (if any)
	     (s-value INSTRUCTIONS-TEXT :visible NIL)
	     (call-prototype-method an-interactor object-under-mouse point))))

    ;; create the prototype for a punctuation icon
    (create-instance 'PUNCTUATION-ICON-PROTO opal:aggregadget
        (:height *punctuation-icon-size*)
        (:width *punctuation-icon-size*)
        (:parts `((:frame ,opal:rectangle   ;; frame to box symbol
                      (:width ,*punctuation-icon-size*) 
                      (:height ,*punctuation-icon-size*)
                      (:left ,(o-formula (gvl :parent :left))) 
                      (:top  ,(o-formula (gvl :parent :top)))
                      (:filling-style ,opal:white-fill))

                  ;; punctuation symbol
                  (:symbol ,opal:text
		      (:left 
		       ,(o-formula (+ (gvl :parent :left) 1
				      (- (floor *punctuation-icon-size* 2)
					 (floor (opal:string-width 
						 (gvl :font) 
						 (gvl :string)) 2)))))
		      (:top  
		       ,(o-formula (+ (gvl :parent :top)
				      (round *punctuation-icon-size* 3))))
		      (:font 
		       ,(opal:get-standard-font :fixed :bold :very-large))
                      (:string ,(o-formula
				 (nth (gvl :parent :rank) 
				      (gvl :parent :parent :items)))))

                  ;; for selected punctuation icon  
                  (:feedback ,opal:rectangle  
                      (:obj-over NIL)  ;; set by the interactor 
                      (:left ,(o-formula (gvl :parent :left)))
                      (:top ,(o-formula (gvl :parent :top)))
                      (:width ,(o-formula (gvl :parent :width)))
                      (:height ,(o-formula (gvl :parent :height)))
                      (:visible ,(o-formula (gvl :parent :interim-selected)))
                      (:line-style ,red-line-4)
                      (:draw-function :xor)
                      (:fast-redraw-p T)))))

    ;; create the aggrelist for the punctuation icons 
    (create-instance 'PUNCTUATION-ICON-AGGL opal:aggrelist
        (:top (o-formula (+ (g-value CANVAS-LABEL :top)
                            (g-value CANVAS-LABEL :height) 5)))
	(:left 40)
        (:direction :vertical)
        (:h-spacing 390)
        (:v-spacing 0)
        (:fixed-width-p T)
        (:fixed-width-size *punctuation-icon-size*)
        (:fixed-height-p T)
        (:fixed-height-size *punctuation-icon-size*)
	(:rank-margin 6)
	(:items '("," "." ";" ":" "!" "?" 
		  "'" "\"" "cr" "(" ")" "$"))
	(:item-prototype PUNCTUATION-ICON-PROTO)
        (:interactors 
	    ;; for selecting an icon
            `((:press ,inter:button-interactor   
		 (:window ,(o-formula (gv-local :self :operates-on :window)))
		 (:how-set :toggle)
		 (:start-where 
		  ,(o-formula (list :element-of (gvl :operates-on))))
		 (:stop-action
		  ,#'(lambda (an-interactor final-obj-over)
		       (call-prototype-method an-interactor final-obj-over)
		       (add-punctuation final-obj-over)))))))

    (opal:add-components (g-value CANVAS-WIN :aggregate)
	     INSTRUCTIONS-TEXT)
    (opal:add-component (g-value UNISTROKE-ICON-WIN :aggregate) 
             UNISTROKE-ICON-AGGL)
    (opal:add-components TOP-AGG
             MAIN-MENU TEXT-LABEL ALPHABET-LABEL CANVAS-LABEL 
	     PUNCTUATION-ICON-AGGL)

    (opal:update TOP-WIN) 

    ;; print out instructions
    (format t "  To use Demo-Unistrokes, draw any of the shorthand gestures given in the~%")
    (format t "\"Alphabet\" window.  The resulting letters will be displayed in the \"Text\"~%")
    (format t "window.  Use the left button for lower-case letters and the middle button for~%")
    (format t "upper-case.  You can delete text in the \"Text\" window by drawing a line through~%")
    (format t "the text.  The deleted text can be pasted back into the \"Text\" window by~%")
    (format t "drawing a caret gesture at the location to paste the text.  You can change~%")
    (format t "the cursor location by clicking at the desired location.  Punctuation~%")
    (format t "characters (and carriage returns via the \"CR\" icon) can be added by~%")
    (format t "clicking on the icons at the bottom.~%")

    (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))

(format t "Type (demo-unistrokes:do-go) to begin.~%")


