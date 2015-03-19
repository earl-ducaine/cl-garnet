;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; gestureinter.lisp
;;;
;;; This file contains the mouse interactors to handle gestures.
;;; It should be loaded after Interactor.lisp
;;;
;;; Designed and implemented by James A. Landay 

(in-package "INTERACTORS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(gesture-interactor

	    gest-classify                 ;; functions in classify.lisp
	    gest-new-classifier
	    make-gest-class
	    gest-class-name
	    gest-class-examples

	    gest-attributes-minx          ;; functions in features.lisp 
	    gest-attributes-maxx
	    gest-attributes-miny
	    gest-attributes-maxy
	    gest-attributes-initial-sin    
	    gest-attributes-initial-cos
	    gest-attributes-startx     
	    gest-attributes-starty
	    gest-attributes-endx       
	    gest-attributes-endy
	    gest-attributes-dx2           
	    gest-attributes-dy2          
	    gest-attributes-magsq2      
	    gest-attributes-path-r
	    gest-attributes-path-th
	    gest-attributes-abs-th
	    gest-attributes-sharpness

	    gest-classifier-read          ;; functions in fileio.lisp
	    gest-classifier-write
	    gest-classifier-convert)))

;;;============================================================
;;;============================================================
;;;============================================================


;;;============================================================
;;; Utility Procedures 
;;;============================================================

(defconstant POINT-ARRAY-SIZE 80)   ; initial size of point array
(defconstant EXTENTION-SIZE 30)     ; amount to extend by when full

;; global variables definitions
                                    ; the points gestured....
(defvar *points* (make-array POINT-ARRAY-SIZE :adjustable t 
                             :fill-pointer 0 :element-type 'integer))
(defvar *last-point* (list 0 0))    ; use these to avoid cons-ing
(defvar *cur-point*  (list 0 0))


;;;============================================================
;;; Graphics routines
;;;============================================================

;; Draw-Gesture-Line uses gem to quickly draw a line between the given points
;; with the given linestyle.  Both the Mac and X versions draw directly to the
;; window's actual drawable, not ever to its backing store.
;; 
;; Parameters:
;;    window     - window to draw in
;;    x1, y1     - point to draw from
;;    x2, y2     - point to draw to
;;    line-style - style to draw in
;;
(defmacro draw-gesture-line (window x1 y1 x2 y2 line-style)
  #-apple
  `(let ((the-window ,window))
    (gem:draw-line the-window ,x1 ,y1 ,x2 ,y2 :copy ,line-style
                   (g-value the-window :drawable)))

  #+apple
  `(let ((the-window ,window))
     (ccl:with-focused-view (g-value the-window :drawable)
       (gem:draw-line the-window ,x1 ,y1 ,x2 ,y2 :copy ,line-style))))


;; gesture-bbox returns the bounding box for the given array of points
;; 
;; Parameters:
;;    points - array of points to get the bounding box of (needs fill-pointer)
;;
(defun gesture-bbox (points)
  (let ((maxx -99999) (maxy -99999)
        (minx 99999) (miny 99999)
        (x 0) (y 0)
        (size (fill-pointer points)))
    (do* ((index 0 (+ index 2)))
         ((>= index size))       ;; exit condition

      (setf x (aref points index))
      (setf y (aref points (1+ index)))

      ;; check X coord
      (if (> x maxx)
        (setf maxx x))
      (if (< x minx)
        (setf minx x))
        	
      ;; check Y coord
      (if (> y maxy)
        (setf maxy y))
      (if (< y miny)
        (setf miny y)))
	
    ;; return the bounding box
    (opal::make-bbox :x1 minx :y1 miny :x2 maxx :y2 maxy :valid-p T)))

      
;; erase-path erases the lines drawn between the given points in the 
;; given window. Calculates the bbox, which can be repetitive (since it 
;; is USUALLY computed by the gesture recognizer, but NOT always.)
;;
;; Parameters:
;;    points - array of points to erase
;;    window - window to erase in
;;
(defun erase-path (points window)
  (let ((bbox (gesture-bbox points)))
    (opal::invalidate-bbox window 
                           (1- (opal::bbox-x1 bbox)) (1- (opal::bbox-y1 bbox))
                           (1+ (opal::bbox-x2 bbox)) (1+ (opal::bbox-y2 bbox)))
    (opal:update window)))
 	

;;;============================================================
;;; Gesture-Interactor
;;;============================================================


;;;============================================================
;;; Default Procedures to go into the slots
;;;============================================================


(declaim (special Gesture-Interactor))


(defun Gesture-Interactor-Initialize (new-Gesture-schema)
    (if-debug new-Gesture-schema 
              (format T "Gesture initialize ~s~%" new-Gesture-schema))

    (Check-Interactor-Type new-Gesture-schema inter:gesture-interactor)
    (Check-Required-Slots new-Gesture-schema)
    (Set-Up-Defaults new-Gesture-schema)
) ;end initialize procedure


;; Draws the latest point in the path
(defun Gesture-Int-Running-Action (an-interactor new-obj-over point)
  (if-debug an-interactor 
	    (format T "Gesture int-running, new-obj-over= ~s, point= ~s~%"
		    new-obj-over point))

  ;; draw the new point if trace is true and if didn't go outside 
  (when (and (g-value an-interactor :show-trace)
	     (not (g-value an-interactor :went-outside)))
    (draw-gesture-line (g-value an-interactor :current-window)
		       (first *last-point*) (second *last-point*)
		       (first point) (second point)
		       (g-value an-interactor :line-style)))
  
  ;; set the new last point without cons-ing 
  (setf (first *last-point*) (first point)) 
  (setf (second *last-point*) (second point)) 
  
  ;; add point to array of points
  (vector-push-extend (first point) *points* EXTENTION-SIZE) 
  (vector-push-extend (second point) *points*  EXTENTION-SIZE))


;; Executes the running action for the first point
(defun Gesture-Int-Start-Action (an-interactor obj-under-mouse point)
  (if-debug an-interactor 
	    (format T "Gesture int-start over ~s~%, point = ~s~%" 
		    obj-under-mouse point))

  ;; set the initial object that we started over
  (s-value an-interactor :first-obj-over obj-under-mouse) 

  ;; not outside...
  (s-value an-interactor :went-outside NIL)

  ;; reset the clip mask to the window size (may need to process lock this)
  (let* ((current-window (g-value an-interactor :current-window))
	 (clip-mask (list 0 0 (g-value current-window :width)
			  (g-value current-window :height)))
	 (display-info (g-value current-window :display-info))
	 (line-style-gc (gem:display-info-line-style-gc display-info))
	 (filling-style-gc (gem:display-info-filling-style-gc display-info)))

    (gem:set-clip-mask current-window clip-mask 
		       line-style-gc filling-style-gc))
    
  ;; set the initial last-point and make the set of points empty
  (setf (first *last-point*) (first point)) 
  (setf (second *last-point*) (second point)) 
  (setf (fill-pointer *points*) 0)

  ;; do the running action
  (kr-send an-interactor :running-action 
	   an-interactor obj-under-mouse point) )


;; Try to recognize the gesture and then erase the path
(defun Gesture-Int-Stop-Action (an-interactor final-obj-over point)
  (if-debug an-interactor 
	    (format T "Gesture int-stop over ~s~%" final-obj-over))

  ;; don't call final function or erase (already erased) if went outside
  (unless (g-value an-interactor :went-outside)

    ;; send the points to the classifier
    (let ((class-name nil)
	  (attributes nil)
	  (nap nil)
	  (dist nil))

      ;; add the latest point (if there is one) to the array 
      (when point
	(vector-push-extend (first point) *points* EXTENTION-SIZE) 
	(vector-push-extend (second point) *points*  EXTENTION-SIZE))
            
      ;; draw the new point if trace is true
      (when (g-value an-interactor :show-trace)
	(draw-gesture-line (g-value an-interactor :current-window)
			   (first *last-point*) (second *last-point*)
			   (first point) (second point)
			   (g-value an-interactor :line-style)))

      ;; classify the gesture
      (multiple-value-setq (class-name attributes nap dist)
	(gest-classify *points* 
		       (g-value an-interactor :classifier)
		       (g-value an-interactor :min-non-ambig-prob)
		       (g-value an-interactor :max-dist-to-mean)))
      
      ;; erase the line if :show-trace is true 
      ;; If :erase is false and not recognized, erase anyways
      (when (and (g-value an-interactor :show-trace) 
		 (or (g-value an-interactor :erase) (not class-name)))
	(erase-path *points* (g-value an-interactor :current-window)))

      (if-debug an-interactor
		(format T "Gesture classified as ~s~%" class-name))
      (if-debug an-interactor
		(format T "with probability ~s and distance ~s~%" 
			nap dist))
      
      (kr-send an-interactor :final-function an-interactor 
	       (g-value an-interactor :first-obj-over) 
	       class-name attributes *points* nap dist))))


;; don't do anything.... we want to wait for mouse up
(defun Gesture-Int-Back-Inside-Action (an-interactor new-obj-over)
  (if-debug an-interactor
	    (format T "Gesture int-back-inside, obj= ~s~%" new-obj-over)))


;; beep and erase the line if :show-trace is true
(defun Gesture-Int-Outside-Action (an-interactor prev-obj-over)
  (if-debug an-interactor 
	    (format T "Gesture int-outside, old = ~s~%" prev-obj-over))
  (inter:beep)
  (when (g-value an-interactor :show-trace)
    (erase-path *points* (g-value an-interactor :current-window)))
  (s-value an-interactor :went-outside T))


;; erase the gesture if it was visible and haven't been outside 
(defun Gesture-Int-Abort-Action (an-interactor)
  (if-debug an-interactor (format T "Gesture int-abort over ~%"))
  (when (and (g-value an-interactor :show-trace)
	     (not (g-value an-interactor :went-outside)))
    (erase-path *points* (g-value an-interactor :current-window))))


;;;============================================================
;;; Go procedure utilities
;;;============================================================


;;; if continuous: (remove from start level, add to stop and abort
;;;             levels, change state to running
;;;             *ALSO* fix running where to be the object started over)
;;; save object over, call start procedure.
(defun gesture-do-start (an-interactor new-obj-over event)
    (if-debug an-interactor 
              (format T "Gesture starting over ~s~%" new-obj-over))
        
    (setf (first *cur-point*) (event-x event))
    (setf (second *cur-point*) (event-y event)) 

    (if (g-value an-interactor :continuous)  ;then go to running state
        (progn
            (Fix-Running-Where an-interactor new-obj-over)
            (GoToRunningState an-interactor T)
            (kr-send an-interactor :start-action an-interactor 
                     new-obj-over *cur-point*)
        )
        ;; else call stop-action
        (progn
            (kr-send an-interactor :stop-action an-interactor 
                     new-obj-over *cur-point*)
            (GoToStartState an-interactor NIL)
        )
    )
)


(defun gesture-do-abort (an-interactor become-inactive event)
    (declare (ignore event become-inactive))
    (if-debug an-interactor (format T "Gesture aborting~%"))

    (GoToStartState an-interactor T)
    (kr-send an-interactor :abort-action an-interactor)
)


(defun gesture-do-outside (an-interactor)
    (if-debug an-interactor (format T "Gesture outside~%"))

    (s-value an-interactor :current-state :outside)
    (kr-send an-interactor :outside-action an-interactor
             (g-value an-interactor :outside))
)


; call abort
(defun gesture-do-outside-stop (an-interactor event)
    (if-debug an-interactor (format T "Gesture stop outside~%"))
    (gesture-do-abort an-interactor NIL event)
)


; call back-inside procedure, change state to running
(defun gesture-do-back-inside (an-interactor new-obj-over event)
    (declare (ignore event))
    (if-debug an-interactor 
              (format T "Gesture back-inside over ~s~%" new-obj-over))

    (s-value an-interactor :current-state :running)
    (kr-send an-interactor :back-inside-action an-interactor new-obj-over)
)


; get the new point and pass it to the running-action
(defun gesture-do-running (an-interactor new-obj-over event)
    (if-debug an-interactor 
              (format T "Gesture running over ~s~%" new-obj-over))

    (setf (first *cur-point*) (event-x event))
    (setf (second *cur-point*) (event-y event)) 
    (kr-send an-interactor :running-action an-interactor 
             new-obj-over *cur-point*)
)


;;; Will be inside
;;; Remove from running level, add to start level
;;; unless :self-deactivate, change state to start, call stop procedure
(defun gesture-do-stop (an-interactor new-obj-over event)
    (if-debug an-interactor 
              (format T "Gesture stop over ~s~%" new-obj-over))

    (setf (first *cur-point*) (event-x event))
    (setf (second *cur-point*) (event-y event)) 
    (GoToStartState an-interactor T)
    (kr-send an-interactor :stop-action an-interactor 
             new-obj-over *cur-point*)
)


;;; This is used if explicitly call Stop-Interactor.  
(defun gesture-explicit-stop (an-interactor)
    (if-debug an-interactor (format T "Gesture explicit stop~%"))

    (GoToStartState an-interactor T)
    (kr-send an-interactor :stop-action an-interactor NIL NIL)
)


;;;============================================================
;;; Gesture schema
;;;============================================================

(Create-Schema 'inter:gesture-interactor
        (:is-a inter:interactor)
        (:name :First-Gesture-interactor)
        (:start-action 'Gesture-Int-Start-Action)
        (:running-action 'Gesture-Int-Running-Action)
        (:stop-action 'Gesture-Int-Stop-Action)
        (:abort-action 'Gesture-Int-Abort-Action)
        (:outside-action 'Gesture-Int-Outside-Action)
        (:back-inside-action 'Gesture-Int-Back-Inside-Action)
        (:abort-event '(:control-g :control-\g))
        (:running-where T)             
        (:classifier NIL)              ; classifier to use
        (:show-trace T)                ; show trace of gesture?
        (:erase T)		       ; if showing trace, erase if recognized?
	(:line-style opal:default-line-style)
        (:min-non-ambig-prob nil)      ; non-ambiguity probability
        (:max-dist-to-mean nil)        ; distance to class mean 
        (:went-outside NIL)            ; set in outside action  
        (:first-obj-over NIL)          ; object started on
        (:Go 'General-Go)  ; proc executed when events happen
        (:Do-Start 'Gesture-Do-Start)     ; these are
        (:Do-Running 'Gesture-Do-Running) ;   called by GO
        (:Do-Explicit-Stop 'Gesture-Explicit-Stop) ;for stop-interactor
        (:Do-Stop 'Gesture-Do-Stop)       ;   to do
        (:Do-Abort 'Gesture-Do-Abort)     ;   the real work.
        (:Do-Outside 'Gesture-Do-Outside) ;   They call the
        (:Do-Back-Inside 'Gesture-Do-Back-Inside)  ; appropriate
        (:Do-Outside-Stop 'Gesture-Do-Outside-Stop); -action procedures
        (:initialize 'Gesture-Interactor-Initialize)) ;proc to call
                                               ; when created
