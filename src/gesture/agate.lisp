;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: AGATE; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; agate.lisp 
;;;
;;; This is AGATE: The Garnet Gesture Trainer application. It is used 
;;; for training gestures that are passed to the Garnet gesture 
;;; interactor.
;;;
;;; Designed and implemented by James A. Landay 
;;;
;;; Future work: 
;;;     cut, copy, & paste; undo
;;;
;;; Known bugs:
;;;     - problems with the scrolling windows (bugs are in the gadgets)
;;;     - when delete "prototype" example for a class, the arrow doesn't 
;;;       get updated after replacing it with new example.
;;;     - low-level file write (in fileio) doesn't handle failure (permission)
;;;     - not clear if resizing windows does the right thing
;;;     - need to make more things constant (but VERY carefully.)

(in-package :AGATE)

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

        ;; load gesture-loader
        (common-lisp-user::garnet-load "gestures:gesture-loader")

        ;; load training code
        (load (merge-pathnames "train"
			       common-lisp-user::Garnet-Gesture-PathName))))

;; global variables definitions

;; objects created in do-go
(declaim (special TOP-WIN CLASS-WORK-WIN EXAMPLES-WORK-WIN CLASSIFIER-MENU
		  CLASS-MENU EXAMPLES-MENU GESTURE-INTER GCLASS-NAME
		  GESTURE-NAME-IN GESTURE-NAME-OUT GESTURE-NAP GESTURE-DIST
		  ERROR-DIALOG QUIT-DIALOG RENAME-DIALOG TRAINING-DIALOG
		  SAVE-DIALOG LOAD-DIALOG MODE-TOGGLE CLASS-ICON-PROTO
		  CLASS-ICON-AGGL GESTURE-WORK-WIN EXAMPLE-ICON-PROTO
		  GESTURE-ICON-AGGL GESTURE-EXAMPLE-POLY INSTRUCTIONS-TEXT
		  RED-LINE-4 ORANGE-LINE-1 ORANGE-LINE-2 CLASSES-LABEL
		  EXAMPLES-LABEL CANVAS-LABEL TOP-AGG))

(defparameter *cur-classifier* NIL) ;; the current classifier being trained 
(defparameter *trained* NIL)    ;; has *cur-classifier* been trained
                                ;; since the last example?
(defparameter *saved* T)        ;; has current classifier been saved
                                ;; since last change? (not dirty yet)

(defparameter *cur-class-dirty* NIL)
                                ;; Is the class in the gesture agglist dirty?
                                ;; i.e., need to add to class agglist or
                                ;; modify class in class agglist (add/delete)

(defparameter *final-function* NIL) ;; function to call on quit
(defparameter *last-saved-filename* NIL) ;; last filename classifier saved to
(defparameter *last-filename* NIL)    ;; last filename saved or loaded 

(defvar *color-p* (g-value opal:color :color-p)) ;; is this a color screen?

(defparameter *example-icon-size* 60) ;; size of square gesture example icons
(defvar *max-name-chars*)             ;; max length of labels for class icons
(defvar *max-name-height*)            ;; max height for labels in class icons


;; do-clear sets sets the *cur-classifier* to a new classifier and clears
;; the items lists for the GESTURE-ICON-AGGL and CLASS-ICON-AGGL.  It will
;; prompt the user to save the current classifier if it is dirty.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-clear (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (let ((cancel nil))
    (unless *saved*
      (setf cancel (gg:save-file-if-wanted SAVE-DIALOG
					   *last-filename*
					   "Save classifier first?")))
    (unless (or (eq cancel :cancel) (eq cancel 'ERROR))
      (init-classifier))))


;; init-classifier sets sets the *cur-classifier* to a new classifier and 
;; clears the items lists for the GESTURE-ICON-AGGL and CLASS-ICON-AGGL.
;;
;; Parameters:
;;     none
;;
(defun init-classifier ()
  (setf *cur-classifier* (inter:gest-new-classifier))
  (setf *trained* T)
  (setf *saved* T)
  (s-value GCLASS-NAME :value "")
  (s-value GESTURE-INTER :classifier NIL) 
  (s-value MODE-TOGGLE :value "Train")        ;; switch back to train
  (do-train NIL NIL)
  (s-value CLASS-ICON-AGGL :items NIL)
  (s-value GESTURE-ICON-AGGL :items NIL))


;; do-error displays the ERROR-DIALOG with the given string displayed.
;; Does not continue until the user presses the OK button.
;;
;; Parameters:
;;     string - message to display
;;
(defun do-error (string)
  (garnet-gadgets:display-error-and-wait ERROR-DIALOG string))


;; set-gesture-name sets the value of the GESTURE-NAME-IN to the given
;; name and saves the previous value in the :old-value slot.
;;
;; Parameters:
;;    
(defun set-gesture-name (name)
  (s-value GESTURE-NAME-IN :old-value name)
  (s-value GESTURE-NAME-IN :value name))


;; check-rename asks the users if they'd like to rename the the gesture
;; class they are editing or whether to start a new class. It then takes
;; the appropriate action depending on the users answer.
;;
;; Parameters:
;;    gadget   - gadget being edited
;;    new-name - new class name
;;               
(defun check-rename (gadget new-name)
  (let ((old (string-trim '(#\Space) (g-value gadget :old-value)))
    (new (string-trim '(#\Space) new-name)))
    (if (equal old "")
    (s-value gadget :old-value new)
        (unless (equal (string-upcase old) (string-upcase new))
        
      ;; name has changed, see if they want to copy
      (let ((answer   
         (garnet-gadgets:display-query-and-wait RENAME-DIALOG 
               (format NIL
             "Select \"Rename\" to change the~a~%~a~s~a~s. ~a~%~a"
             " name of the gesture " "class from "
             old " to " new
             "Select \"New Class\""
             "to start training a new gesture class."))))
        (cond
         ((equal answer "Rename")
          (rename-class old new)
          (s-value gadget :old-value new))  
     
         ((equal answer "New Class")
          (s-value gadget :value old)
          (do-new-class nil nil)
          (s-value gadget :value new)
          (s-value gadget :old-value new))  
     
         (t
          ;; they hit cancel, so restore old name
          (s-value gadget :value old))))))))


;; rename-class changes the name of the class in the CLASS-ICON-AGGL
;; from old-name to new-name (if old-name is found in the list, otherwise
;; it does nothing.)
;;
;; Parameters:
;;    old-name - old class name
;;    new-name - new class name
;;
(defun rename-class (old-name new-name)
  ;; select the icon in class agglist corresponding to gesture
  (let ((index (position-if (class-name-equal old-name)
                (g-value CLASS-ICON-AGGL :items))))
    (if index
        (let ((item (nth index (g-value CLASS-ICON-AGGL :items))))
      (setf (gest-class-name item) new-name)
      (opal:change-item CLASS-ICON-AGGL item index)
      (setf *trained* NIL)
      (setf *saved* NIL)))))


;; do-load loads an existing classifier from a file. If the current 
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
					   "Save classifier first?")))
    (unless (or (eq cancel :cancel) (eq cancel 'ERROR))
      (gg:display-load-gadget-and-wait LOAD-DIALOG *last-filename*))))


;; load-classifier loads a classifier from the given file.
;;
;; Parameters:
;;    gadget - ignored
;;    file   - file to load classifier from  
;;
(defun load-classifier (gadget file)
  (declare (ignore gadget))
  (let ((classifier NIL)
    (class-examples nil))
    ;; read classifier and examples (if present)
    (opal:with-hourglass-cursor
     (multiple-value-setq 
     (classifier class-examples)
     (inter:gest-classifier-read file)))

    ;; make sure this is a legal classifier
    (cond 
     ((null classifier)
      (do-error (format NIL "ERROR: NIL classifier!~%~a" 
            "Press OK to continue.")))
      
     ((null class-examples)
      (do-error (format NIL "ERROR: File ~s ~%~a~%~a~%"
            (namestring file)
            "contains an incompatible classifier. "
            "It can only be loaded with Agate v1.0.")))

     (t
      (opal:with-hourglass-cursor
       (s-value CLASS-ICON-AGGL :items class-examples)
       (s-value GESTURE-ICON-AGGL :items NIL)
       (s-value GESTURE-ICON-AGGL :selected NIL)
       (setf *cur-classifier* classifier)
       (when (equal (g-value MODE-TOGGLE :value) "Recognize")
	     (s-value GESTURE-INTER :classifier *cur-classifier*))
       (s-value GCLASS-NAME :value (pathname-name file))
       (setf *last-filename* file)
       (setf *trained* T)
       (setf *saved* T)
       (opal:update CLASS-WORK-WIN))))))


;; do-save saves the current classifier to a file. Trains it
;; first if necessary.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-save (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (gg:display-save-gadget-and-wait SAVE-DIALOG *last-filename*))


;; save-classifier saves the current classifier to the given file. Trains
;; classifier if necessary. Returns 'ERROR if unable to train.
;;
;; Parameters:
;;    gadget   - ignored
;;    filename - file to save classifier to
;;
(defun save-classifier (gadget filename)
  (declare (ignore gadget))

  (let ((fail nil))
    ;; if the classifier hasn't been trained, do it!
    (unless *trained*
      (setf fail (train-classifier)))

    (unless fail
      (opal:with-hourglass-cursor
       (inter:gest-classifier-write *cur-classifier* filename
                    (g-value CLASS-ICON-AGGL :items)))
      (s-value GCLASS-NAME :value (pathname-name filename))
      (setf *last-saved-filename* filename)
      (setf *last-filename* filename)
      (setf *saved* T))

    fail))              ;; return failure code


;; do-test-classify sets the gesture interactor classifier slot to the 
;; currently trained classifier. If the classifier hasn't been trained,
;; it will train it.
;; 
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-test-classify (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))

  ;; disable the input gesture name field, NEW, DELETE, & SHOW buttons
  ;; enable the output field
  (s-value GESTURE-NAME-IN :visible NIL)

  ;; clear out shown example, if any
  (s-value GESTURE-EXAMPLE-POLY :poly :point-list NIL)
  (s-value GESTURE-NAME-OUT :value "")

  (if (not *trained*)
      (train-classifier)
      (progn                        ;;else already trained
    (s-value GESTURE-ICON-AGGL :items NIL)   ;; clear out examples
    (s-value GESTURE-INTER :classifier *cur-classifier*))))


;; train-classifier trains the current examples and sets the *cur-classifier*
;; to the resulting classifier. Returns NIL on sucess and 'ERROR on error.
;; 
(defun train-classifier ()
  (opal:with-hourglass-cursor
   (add-displayed-class)      ;; move the examples up to class aggl
   (garnet-gadgets:display-query TRAINING-DIALOG)

   ;; clear the classifier!!! 
   (setf *cur-classifier* (inter:gest-new-classifier))

   ;; for each class, add the examples
   (dolist (gesture-class (g-value CLASS-ICON-AGGL :items))
     (opal:with-hourglass-cursor
      (let ((name (gest-class-name gesture-class))
        (examples (gest-class-examples gesture-class)))
    (dolist (example examples)
      (inter:gest-add-example example
         (intern (string-upcase (string-trim '(#\Space) name)) 'keyword)
         *cur-classifier*))
    
    ;; modify training dialog to show progress
    (s-value TRAINING-DIALOG :value
         (concatenate 'string (g-value TRAINING-DIALOG :value) "."))
    (opal:update TOP-WIN))))

   ;; remove the training dialog
   (s-value TRAINING-DIALOG :window :visible NIL)
   (s-value TRAINING-DIALOG :value "")
   (opal:update TOP-WIN))
   
  ;; see if there is enough examples
  (let ((result (inter:gest-done-adding *cur-classifier*)))
    (if (null result)
    (progn 
      (do-error (format NIL 
                "ERROR: Can not train classifier.~%~a~%~a"
                "Try adding more examples."
                "Press OK to continue."))
      (s-value MODE-TOGGLE :value "Train")
      (do-train NIL NIL)
      (return-from train-classifier 'ERROR))
        (progn
      (setf *trained* T)      ;; we were able to train it 
      
      ;; warn if fix-classifier is called 
      (if (equal result 'INTERACTORS::FIX)
          (do-error (format NIL
                "WARNING: This classifier will work~%~a~%~a"
                                "better by training more gesture classes."
                "Press OK to continue.")))
      
      (s-value GESTURE-INTER :classifier *cur-classifier*)
      (return-from train-classifier 'NIL)))))


;; do-train sets the gesture interactor classifier slot to NIL, so that
;; handle-gesture will allow adding examples to the current classifier
;; without trying to classify them. In addition it activates the NEW,
;; DELETE, & SHOW buttons.
;; 
;; Parameters:
;;     gadgets-object 
;;     item-string
;;
(defun do-train (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (s-value GESTURE-INTER :classifier NIL) 

  ;; clear out examples
  (s-value GESTURE-ICON-AGGL :items NIL)
  (s-value GESTURE-EXAMPLE-POLY :poly :point-list NIL)

  ;; clear gesture name field
  (set-gesture-name "")

  ;; enable the input gesture name field, NEW, DELETE, & SHOW buttons
  ;; disable the output field
  (s-value GESTURE-NAME-IN :visible T))


;; show-icon makes sure that the given icon is visible in the given window
;; and if it is not, it will scroll to make it so.
;;
;; Parameters:
;;    scrolling-window - window to make visible in
;;    icon             - icon to make visible
;;
(defun show-icon (scrolling-window icon)
  (gg:show-box scrolling-window
           (g-value icon :left) 
           (g-value icon :top)
           (+ (g-value icon :left) (g-value icon :width))
           (+ (g-value icon :top) (g-value icon :height))))


;; add-example adds the new points as an another example to the current
;; gesture class and displays the given points as a small icon. It also
;; makes sure that the icon is displayed (by scrolling if necessary). The
;; first example is also added to the CLASS-ICON-AGGL.
;; 
;; Parameters:
;;     points - the points making up the example to add
;;
(defun add-example (points)
  (let ((firstp (null (g-value GESTURE-ICON-AGGL :items))))
    (setf *cur-class-dirty* T)
    (setf *trained* NIL)
    (setf *saved* NIL)
    (opal:add-item GESTURE-ICON-AGGL points)

    ;; if first example need to place in CLASS-ICON-AGGL
    (when firstp
      (let* ((name (g-value GESTURE-NAME-IN :value))
         (new-item (make-gest-class :name name :examples (list points)))
         (index 
          (position-if (class-name-equal name)
               (g-value CLASS-ICON-AGGL :items))))
    (setf *cur-class-dirty* NIL)

    ;; is it already there???
    (if index
        (progn
          (opal:change-item CLASS-ICON-AGGL new-item index)
          (show-icon CLASS-WORK-WIN 
             (nth index (g-value CLASS-ICON-AGGL :components))))
        (progn
          (opal:add-item CLASS-ICON-AGGL new-item)
          ;; *** WORK *** doesn't autoscroll properly without this update
          (opal:update CLASS-WORK-WIN)
          (show-icon CLASS-WORK-WIN (g-value CLASS-ICON-AGGL :tail))))))

    (opal:update EXAMPLES-WORK-WIN)
    (show-icon EXAMPLES-WORK-WIN (g-value GESTURE-ICON-AGGL :tail))))


;; handle-gesture is called by the gesture interactor after it
;; classifies a gesture. If we are currently training, then
;; handle-gesture will add the example to the trainer. If we are
;; testing then handle-gesture will output the name of the gesture
;; that was identified.
;;
;; Parameters:
;;    inter, first-obj-over, attribs - ignored
;;    class-name - name of recognized gesture (or NIL if unrecognized)
;;    points     - points in gesture 
;;    nap        - non-ambiguity of gesture
;;    dist       - distance from class-name
;;
(defun handle-gesture (inter first-obj-over class-name attribs
                       points nap dist)
  (declare (ignore inter first-obj-over attribs))
  (if (equal (g-value MODE-TOGGLE :value) "Train") 
      ;; in training mode
      (if (equal "" (string-trim '(#\Space) 
                   (g-value GESTURE-NAME-IN :value)))
    (do-error (format NIL "ERROR: Gesture Class Name is blank.~%~a"
              "Press OK to continue."))
        (add-example (copy-seq points)))

      ;; in test mode
      (let ((selected (g-value CLASS-ICON-AGGL :selected))) 
    (if (null class-name)
        (progn
          ;; de-sellect the selected icon in class agglist
          (when (schema-p selected)
        (s-value selected :selected NIL)
        (s-value CLASS-ICON-AGGL :selected NIL))
          (s-value GESTURE-NAME-OUT :value "Unrecognized")
          (s-value GESTURE-NAP :value " ")
          (s-value GESTURE-DIST :value " "))

        (progn
          ;; select the icon in class agglist corresponding to gesture
          (let* ((name (write-to-string class-name :escape nil))
             (index (position-if (class-name-equal name)
                     (g-value CLASS-ICON-AGGL :items)))
             (selected-icon 
              (if index
              (nth index (g-value CLASS-ICON-AGGL :components))
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

              (s-value selected-icon :selected T)
              (s-value CLASS-ICON-AGGL :selected selected-icon)
              (s-value GESTURE-NAME-OUT :value name)
              (s-value GESTURE-NAP :value (format nil "~,2f" nap))
              (s-value GESTURE-DIST :value (format nil "~,1f" dist))
              (show-icon CLASS-WORK-WIN selected-icon)))))))))


;; do-delete-class deletes the selected class from the classifier.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-delete-class (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (let ((cur (g-value CLASS-ICON-AGGL :selected)))
    (if (schema-p cur)
    (progn
      ;; see if the selected class icon is same as that displayed
      (let ((index (position cur (g-value CLASS-ICON-AGGL :components))))
        (when (funcall (class-name-equal (g-value GESTURE-NAME-IN :value))
               (nth index (g-value CLASS-ICON-AGGL :items)))
          (set-gesture-name "")
          (s-value GESTURE-ICON-AGGL :items NIL)))

      ;; delete any examples showing
      (s-value GESTURE-EXAMPLE-POLY :poly :point-list NIL)

      ;; delete from CLASS-ICON-AGGL
      (opal:remove-nth-item CLASS-ICON-AGGL (g-value cur :rank))
      (s-value CLASS-ICON-AGGL :selected NIL)
      (setf *trained* NIL)
      (setf *saved* NIL))
    ;; no example selected!
        (inter:beep))))


;; do-delete-example deletes the selected example from the current 
;; gesture class. If it is the first example in the class, the next
;; one will replace it in the CLASS-ICON-AGGL.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-delete-example (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (let ((cur (g-value GESTURE-ICON-AGGL :selected)))
    (if (schema-p cur)
    (let ((rank (g-value cur :rank))) 
      (s-value cur :selected NIL)
      (setf *cur-class-dirty* T)
      (setf *trained* NIL)
      (setf *saved* NIL)
      (s-value GESTURE-EXAMPLE-POLY :poly :point-list NIL)
      (opal:remove-nth-item GESTURE-ICON-AGGL rank)
      (s-value GESTURE-ICON-AGGL :selected NIL)
      
      ;; if this was the first example, replace it in CLASS-ICON-AGGL
      (when (eq rank 0)
        (let* ((name (g-value GESTURE-NAME-IN :value))
           (index (position-if (class-name-equal name)
                       (g-value CLASS-ICON-AGGL :items))))
          (if index
          (if (null (g-value GESTURE-ICON-AGGL :items))
              ;; if this was the first & last example, remove class
              (progn
            (s-value CLASS-ICON-AGGL :selected NIL)
            (opal:remove-nth-item CLASS-ICON-AGGL index)
            (opal:update CLASS-WORK-WIN))
            
              ;; else change existing one
              (let ((item (nth index 
                       (g-value CLASS-ICON-AGGL :items))))
            (setf (gest-class-examples item) 
                  (list (car (g-value GESTURE-ICON-AGGL :items))))
            (opal:change-item CLASS-ICON-AGGL item index)
;; *** WORK *** for some reason arrow doesn't get updated (not getting new pts)
            (opal:update CLASS-WORK-WIN)))))))

    ;; no examples selected!
    (inter:beep))))


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


;; add-displayed-class adds the examples currently being displayed (if there
;; are any and they are dirty) to the gesture class agglist.  If this gesture
;; class is already in the agglist, these examples will replace the old ones.
;;
;; Parameters:
;;    none
;;
(defun add-displayed-class ()
  ;; add previous class (if there was one) to classifier
  (if (and *cur-class-dirty*
       (g-value GESTURE-ICON-AGGL :items))
      ;; is this class already in class agglist? (i.e. need to change)
      (let ((index 
         (position-if (class-name-equal (g-value GESTURE-NAME-IN :value))
              (g-value CLASS-ICON-AGGL :items)))
        (new-item 
         (make-gest-class :name (g-value GESTURE-NAME-IN :value)
                  :examples (g-value GESTURE-ICON-AGGL :items))))

    (setf *cur-class-dirty* NIL)
    (opal:with-hourglass-cursor
     (if index
         (progn
           (opal:change-item CLASS-ICON-AGGL new-item index)
           (show-icon CLASS-WORK-WIN 
              (nth index (g-value CLASS-ICON-AGGL :components))))
          (progn
        (opal:add-item CLASS-ICON-AGGL new-item)
        (opal:update CLASS-WORK-WIN)
        (show-icon CLASS-WORK-WIN (g-value CLASS-ICON-AGGL :tail)))))))

  ;; initialize the GESTURE-ICON-AGGL and GESTURE-NAME-IN
  (s-value GESTURE-ICON-AGGL :items NIL)
  (set-gesture-name ""))


;; do-new-class initializes the gesture-icon-aggl to take new examples for
;; a new gesture class. If there are examples from another gesture class
;; already displayed, they will be removed and a representative
;; example will be displayed with the set of class icons.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-new-class (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))

  ;; deselect any selected classes
  (let ((selected (g-value CLASS-ICON-AGGL :selected))) 
    ;; de-sellect the selected icon in class agglist
    (when (schema-p selected)
      (s-value selected :selected NIL)
      (s-value CLASS-ICON-AGGL :selected NIL))

    ;; clear out shown example, if any
    (s-value GESTURE-EXAMPLE-POLY :poly :point-list NIL)

    ;; add previous class (if there was one) to classifier
    (add-displayed-class)))
    

;; do-show-example draws the full-size image of the selected gesture
;; example icon.  The image will be erased when the user starts to
;; draw another example in the work window or does a show example on
;; another icon.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-show-example (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (let ((selected (g-value GESTURE-ICON-AGGL :selected)))

    ;; check if any are selected
    (if (schema-p selected)
    (let ((example (nth (g-value selected :rank)
                (g-value GESTURE-ICON-AGGL :items))))

      ;; draw a full-sized image
      (s-value GESTURE-EXAMPLE-POLY :poly :point-list
           (coerce example 'list)))
        ;; else error, none selected
        (inter:beep))))


;; do-show-class draws the icons of the currently selected gesture class
;; in the examples window.
;;
;; Parameters:
;;     gadgets-object (ignored)
;;     item-string (ignored)
;;
(defun do-show-class (gadgets-object item-string)
  (declare (ignore gadgets-object item-string))
  (let ((selected (g-value CLASS-ICON-AGGL :selected)))

    ;; check if any are selected
    (if (schema-p selected)
    (opal:with-hourglass-cursor

     ;; add the class currently being trained (if any) to the gesture
     ;; classes agglist and display the selected one instead

     (add-displayed-class)   
     
     (let ((gclass (nth (g-value selected :rank)
                (g-value CLASS-ICON-AGGL :items))))

       ;; clear out shown example, if any
       (s-value GESTURE-EXAMPLE-POLY :poly :point-list NIL)
         
       (s-value GESTURE-ICON-AGGL :items (gest-class-examples gclass))
       (set-gesture-name (gest-class-name gclass))

       ;; make sure the scrolling window gets updated (*** WORK *** bug)
       ;;     (opal:update EXAMPLES-WORK-WIN))
       (opal:notice-items-changed GESTURE-ICON-AGGL)))

        ;; none selected
        (inter:beep))))


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
					 *last-filename*
					 "Save classifier first?"))
    (unless (or (eq cancel :cancel) (eq cancel 'ERROR))
      (do-stop)))))


;; do-stop destroys the application window and everything beneath it.
;;
;; Parmeters:
;;     none
;;
(defun do-stop ()
  (let ((cur-examples 
     (if (and (boundp 'CLASS-ICON-AGGL)
          (schema-p CLASS-ICON-AGGL))
         (g-value CLASS-ICON-AGGL :items)
         NIL)))

    (opal:destroy TOP-WIN)
    (opal:destroy CLASS-ICON-PROTO)
    (opal:destroy EXAMPLE-ICON-PROTO)
    (opal:destroy RED-LINE-4)
    (opal:destroy ORANGE-LINE-1)
    (opal:destroy ORANGE-LINE-2)
    (if *final-function* ;; need to call passed in final function
    ;; don't exit-main-event-loop if there is a *final-function*
    (funcall *final-function* *last-saved-filename* *cur-classifier*
         cur-examples *saved* *trained*)
     #-cmu (inter:exit-main-event-loop))))


;; do-go creates the necessary windows and Garnet objects, and 
;; then starts the application.
;;
;; Parameters (all keyword):
;;    dont-enter-main-event-loop - if T, don't enter the main event loop
;;    double-buffered-p          - if T, use double buffered windows
;;    initial-classifier         - initial classifier to use
;;    initial-examples           - initial examples to display
;;    initial-gesture-name       - name to fill in gesture class name field
;;    final-function             - function to call on quit
;;  
(defun do-go (&key dont-enter-main-event-loop double-buffered-p
                   initial-classifier initial-examples initial-gesture-name
           final-function)
    (if (and (boundp 'TOP-WIN) (schema-p TOP-WIN))
	(do-stop))

    ;; create top-level window
    (create-instance 'TOP-WIN inter:interactor-window
        (:left 280) (:top 120)
        (:double-buffered-p double-buffered-p)
        (:width 510) (:height 800)
        (:title "AGATE: Garnet Gesture Trainer (v. 2.0)")
        (:icon-title "AGATE")
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

    ;; create a query dialog for quiting before save
    (create-instance 'QUIT-DIALOG garnet-gadgets:motif-query-gadget
        (:parent-window TOP-WIN)
        (:button-names '("YES" "NO" "CANCEL")))

    ;; create a query dialog for checking if they want to rename a class
    (create-instance 'RENAME-DIALOG garnet-gadgets:motif-query-gadget
        (:parent-window TOP-WIN)
    (:justification :left)
        (:button-names '("Rename" "New Class" "Cancel")))

    ;; create a training dialog/notifier
    (create-instance 'TRAINING-DIALOG garnet-gadgets:motif-query-gadget
        (:parent-window TOP-WIN)
    (:beep-p NIL)
    (:modal-p NIL)
    (:string (o-formula (concatenate 'string "Training" (gvl :value))))
    (:value "")
    (:justification :center)
    (:background-color opal:cyan)
    (:window-width 300)
    (:button-names NIL))             

    ;; create a save dialog
    (create-instance 'SAVE-DIALOG garnet-gadgets:motif-save-gadget
        (:parent-window TOP-WIN)
    (:query-message "replace existing file")
    (:modal-p t)
    (:initial-directory 
     (namestring common-lisp-user::Garnet-Gesture-Data-PathName))
    (:selection-function #'save-classifier))

    ;; create a load dialog
    (create-instance 'LOAD-DIALOG garnet-gadgets:motif-load-gadget
        (:parent-window TOP-WIN)
    (:modal-p t) 
    (:initial-directory 
     (namestring common-lisp-user::Garnet-Gesture-Data-PathName))
    (:selection-function #'load-classifier))

    ;; create classifier menu 
    (create-instance 'CLASSIFIER-MENU garnet-gadgets:motif-text-button-panel
        ;; can't be constant since we want to center it!
        (:items '(
            ("New Classifier" do-clear)
            ("Load Classifier" do-load)
            ("Save Classifier" do-save)
            ("Quit" do-quit)))
        (:left (o-formula (- (round (gvl :parent :window :width) 2)
                 (round (gvl :width) 2))))
        (:top 5)
        (:direction :horizontal)
        (:h-align :center))

    ;; create the class name output field
    (create-instance 'GCLASS-NAME opal:text
        (:left 2) 
    (:top (o-formula (+ (g-value CLASSIFIER-MENU :top)
                (g-value CLASSIFIER-MENU :height) 10)))
        (:width (o-formula (gvl :parent :window :width)))
    (:font (opal:get-standard-font :serif :bold :medium))
        (:string (o-formula
          (concatenate 'string "Classifier Name:  " (gvl :value))))
        (:value ""))

    (create-instance 'CLASSES-LABEL opal:text
        (:string "Gesture Classes")
    (:font (opal:get-standard-font :serif :bold :medium))
    (:left (o-formula (- (round (gvl :parent :window :width) 2) 
                 (round (opal:string-width (gvl :font)
                               (gvl :string)) 2))))
    (:top (o-formula (+ (g-value GCLASS-NAME :top)
                (g-value GCLASS-NAME :height) 10))))

    (opal:update TOP-WIN) 

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
    
    ;; create the prototype for a gesture class icon
    (create-instance 'CLASS-ICON-PROTO opal:aggregadget
    (:left 0)      ;; set left and top to 0 to avoid inheriting formula
    (:top 0)
        (:height (o-formula (+ (gvl :frame :height)
                   (gvl :gesture-name :height))))
        (:width *example-icon-size*)
        (:parts `((:frame ,opal:rectangle   ;; frame to box gesture
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
                      (:obj-over NIL)  ;; set by the interactor 
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
                          (ceiling (g-value GESTURE-WORK-WIN :width)
                                   (gvl :parent :frame :width))))
                      (:y-scale ,(o-formula 
                          (ceiling (g-value GESTURE-WORK-WIN :height) 
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

    
    ;; create the aggrelist for the gesture class icons 
    (create-instance 'CLASS-ICON-AGGL opal:aggrelist
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
    (:rank-margin (o-formula (floor (gv CLASS-WORK-WIN :inner-width)
                    *example-icon-size*)))
    (:items nil)                ;; add lists of examples as we go...
    (:item-prototype CLASS-ICON-PROTO)
        (:interactors 
          ;; for selecting a class icon
            `((:press ,inter:button-interactor   
                (:window ,(o-formula (gv-local :self :operates-on :window)))
        (:how-set :toggle)
        (:active 
         ,(o-formula (equal (gv MODE-TOGGLE :value) "Train") T))
                (:start-where 
                    ,(o-formula (list :element-of (gvl :operates-on))))
        (:stop-action
         ,#'(lambda (an-interactor final-obj-over)
              (call-prototype-method an-interactor final-obj-over)
              (do-show-class nil nil)))))))

    ;; create scrolling window for the gesture class icons 
    (create-instance 'CLASS-WORK-WIN
                     garnet-gadgets:Motif-Scrolling-Window-With-Bars
        (:left 0)
    (:top (o-formula (+ (g-value CLASSES-LABEL :top)
                (g-value CLASSES-LABEL :height) 5)))
        (:h-scroll-bar-p NIL)
        (:width (o-formula (gvl :parent-window :width) 150))
        (:height 
            (o-formula (* 2 (+ (g-value CLASS-ICON-PROTO :frame :width) 
                   *max-name-height*))))
    (:total-height (o-formula (gvl :inner-aggregate :height) 
                  (* 2 (+ *example-icon-size* 
                      *max-name-height*))))
    (:v-scr-incr (+ *example-icon-size* *max-name-height*))
        (:double-buffered-p double-buffered-p)
        (:parent-window TOP-WIN))
    (opal:update CLASS-WORK-WIN)  ;; update scrolling windows after creation

    ;; create class menu
    (create-instance 'CLASS-MENU garnet-gadgets:motif-text-button-panel
        ;; note: can't be constant since we change the active-p slot
        (:items '(
        ("New Class" do-new-class)
            ("Delete Class" do-delete-class)))
        (:left (o-formula (- (round (gvl :parent :window :width) 2)
                 (round (gvl :width) 2))))
    (:top (o-formula (+ (g-value CLASS-WORK-WIN :top)
                (g-value CLASS-WORK-WIN :height) 10)))
    (:active-p (o-formula (gv GESTURE-NAME-IN :visible)))
    (:inactive-items 
     (o-formula 
      (if (not (and (gv CLASS-ICON-AGGL :selected)
            (schema-p (gv CLASS-ICON-AGGL :selected))))
          (list "Delete Class" "Show Class"))))
        (:h-spacing 25)
        (:direction :horizontal)
        (:h-align :center))

    ;; create the gesture name input/output box
    (create-instance 'GESTURE-NAME-IN gg:motif-scrolling-labeled-box
        ;; note: can't be constant since we change the visible slot
        (:left 2)
    (:top (o-formula (+ (g-value CLASS-MENU :top)
                (g-value CLASS-MENU :height) 10)))
        (:width (o-formula (gvl :parent :window :width)))
        (:label-string "Gesture Class Name:")
        (:value "")
    (:old-value "")
    ;; *** WORK *** what is key-value for????
        (:key-value 
            (o-formula (intern (string-upcase 
                                    (string-trim '(#\Space) (gvl :value)))
                               'keyword)))
    (:selection-function #'check-rename))

    ;; create the gesture name output field
    (create-instance 'GESTURE-NAME-OUT opal:text
        ;; note: can't be constant since we change the visible slot
        (:string (o-formula
          (concatenate 'string "Gesture Class Name: " (gvl :value))))
        (:left 4)
    (:top (o-formula (+ (g-value GESTURE-NAME-IN :top) 4)))
        (:value "")
    (:visible (o-formula (not (gv GESTURE-NAME-IN :visible))))
    (:font (o-formula (g-value GESTURE-NAME-IN :label-font))))

    ;; create the gesture non-ambiguity probability output field
    (create-instance 'GESTURE-NAP opal:text
        ;; note: can't be constant since we change the visible slot
        (:string (o-formula
          (concatenate 'string "Probability: " (gvl :value))))
        (:left 240)
    (:top (o-formula (+ (g-value GESTURE-NAME-IN :top) 4)))
        (:value "")
    (:visible (o-formula (not (gv GESTURE-NAME-IN :visible))))
    (:font (o-formula (g-value GESTURE-NAME-IN :label-font))))

    ;; create the gesture distance output field
    (create-instance 'GESTURE-DIST opal:text
        ;; note: can't be constant since we change the visible slot
        (:string (o-formula
          (concatenate 'string "Distance: " (gvl :value))))
        (:left 385)
    (:top (o-formula (+ (g-value GESTURE-NAME-IN :top) 4)))
        (:value "")
    (:visible (o-formula (not (gv GESTURE-NAME-IN :visible))))
    (:font (o-formula (g-value GESTURE-NAME-IN :label-font))))

    (create-instance 'EXAMPLES-LABEL opal:text
        (:string "Examples")
    (:font (opal:get-standard-font :serif :bold :medium))
    (:left (o-formula (- (round (gvl :parent :window :width) 2) 
                 (round (opal:string-width (gvl :font)
                               (gvl :string)) 2))))
    (:top (o-formula (+ (g-value GESTURE-NAME-IN :top)
                (g-value GESTURE-NAME-IN :height) 10))))
    

    ;; create the prototype for a gesture icon
    (create-instance 'EXAMPLE-ICON-PROTO opal:aggregadget
    (:left 0)      ;; set left and top to 0 to avoid inheriting formula
    (:top 0)
    (:height (o-formula (gvl :frame :height)))
    (:width *example-icon-size*)
        (:parts `((:frame ,opal:rectangle   ;; frame to box gesture
                      (:width ,*example-icon-size*) 
                      (:height ,*example-icon-size*)
                      (:left ,(o-formula (gvl :parent :left))) 
                      (:top  ,(o-formula (gvl :parent :top)))
                      (:filling-style ,opal:white-fill))

                  (:feedback ,opal:rectangle  ;; for selected gesture icon 
                      (:obj-over NIL)  ;; set by the interactor 
                      (:left ,(o-formula (gvl :parent :left)))
                      (:top ,(o-formula (gvl :parent :top)))
                      (:width ,(o-formula (gvl :parent :width)))
                      (:height ,(o-formula (gvl :parent :height)))
                      (:visible ,(o-formula (gvl :parent :selected)))
                      (:line-style ,red-line-4)
                      (:draw-function :xor)
                      (:fast-redraw-p T))

                  ;; a polyline of scaled version of the gesture 
                  (:gesture ,opal:polyline 
                      (:x-scale ,(o-formula 
                          (ceiling (g-value GESTURE-WORK-WIN :width)
                                   (gvl :parent :frame :width))))
                      (:y-scale ,(o-formula 
                          (ceiling (g-value GESTURE-WORK-WIN :height) 
                                   (gvl :parent :frame :height))))
                      (:point-list ,(o-formula 
                  (scale (gvl :x-scale) (gvl :y-scale)
                 (gvl :parent :left) (gvl :parent :top)
                 (nth (gvl :parent :rank) 
                      (gvl :parent :parent :items))))))

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


    ;; create the aggrelist for the example icons 
    (create-instance 'GESTURE-ICON-AGGL opal:aggrelist
        (:top  0) 
        (:left 0) 
        (:width (o-formula (gvl :parent :window :width)))
        (:direction :horizontal)
        (:h-spacing 0)
        (:v-spacing 0)
        (:fixed-width-p T)
        (:fixed-width-size *example-icon-size*)
        (:fixed-height-p T)
        (:fixed-height-size *example-icon-size*)
    (:rank-margin (o-formula (floor (gv EXAMPLES-WORK-WIN :inner-width)
                    *example-icon-size*)))
        (:items nil)                ;; add lists of examples as we go...
        (:item-prototype EXAMPLE-ICON-PROTO)
        (:interactors 
              ;; for selecting an example icon
            `((:press ,inter:button-interactor
                (:window ,(o-formula (gv-local :self :operates-on :window)))
        (:how-set :toggle)
                (:start-where 
                    ,(o-formula (list :element-of (gvl :operates-on))))
        (:stop-action
         ,#'(lambda (an-interactor final-obj-over)
              (call-prototype-method an-interactor final-obj-over)
              (do-show-example nil nil)))))))

    ;; create scrolling window for the gesture examples icons 
    (create-instance 'EXAMPLES-WORK-WIN
                     garnet-gadgets:Motif-Scrolling-Window-With-Bars
        (:left 0)
    (:top (o-formula (+ (g-value EXAMPLES-LABEL :top)
                (g-value EXAMPLES-LABEL :height) 5)))
        (:h-scroll-bar-p NIL)
        (:width (o-formula (gvl :parent-window :width) 150))
        (:height 
            (o-formula (* 2 (g-value EXAMPLE-ICON-PROTO :frame :width))))
    (:total-height (o-formula (gvl :inner-aggregate :height) 
                  (* 2 *example-icon-size*)))
    (:v-scr-incr *example-icon-size*)
        (:double-buffered-p double-buffered-p)
        (:parent-window TOP-WIN))

    (opal:update EXAMPLES-WORK-WIN)  ;; update scrolling window after creation

    ;; create examples menu
    (create-instance 'EXAMPLES-MENU garnet-gadgets:motif-text-button-panel
        (:constant T :except :left :active-p)
        (:items '(
            ("Delete Example" do-delete-example)))
        (:left (o-formula (- (round (gvl :parent :window :width) 2)
                 (round (gvl :width) 2))))
    (:top (o-formula (+ (g-value EXAMPLES-WORK-WIN :top)
                (g-value EXAMPLES-WORK-WIN :height) 10)))
    (:active-p (o-formula 
            (and (gv GESTURE-NAME-IN :visible)
             (schema-p (gv GESTURE-ICON-AGGL :selected))))))

    (create-instance 'MODE-TOGGLE garnet-gadgets:motif-radio-button-panel
        (:constant T :except :left)
        (:left (o-formula (- (round (gvl :parent :window :width) 2)
                 (round (gvl :width) 2))))
    (:top (o-formula (+ (g-value EXAMPLES-MENU :top)
                (g-value EXAMPLES-MENU :height) 10)))
        (:items '(
            ("Train" do-train)
            ("Recognize" do-test-classify)))
        (:direction :horizontal)
    (:h-spacing 30)
        (:text-offset 10)
    (:button-width 18))

    ;; select Train mode to start...
    (g-value MODE-TOGGLE :value)
    (s-value MODE-TOGGLE :value "Train") 

    (create-instance 'CANVAS-LABEL opal:text
        (:string "Canvas")
    (:font (opal:get-standard-font :serif :bold :medium))
    (:left (o-formula (- (round (gvl :parent :window :width) 2) 
                 (round (opal:string-width (gvl :font)
                               (gvl :string)) 2))))
    (:top (o-formula (+ (g-value MODE-TOGGLE :top)
                (g-value MODE-TOGGLE :height) 10))))

    ;; create window for entering and displaying full-sized gestures 
    (create-instance 'GESTURE-WORK-WIN inter:interactor-window
        (:left 0) 
        (:top (o-formula (+ (g-value CANVAS-LABEL :top)
                            (g-value CANVAS-LABEL :height) 5)))
        (:width (o-formula (gvl :parent :width) 150))
        (:height (o-formula (- (gvl :parent :height) (gvl :top))))
        (:double-buffered-p double-buffered-p)
        (:border-width 2)
        (:visible t)
        (:omit-title-bar-p NIL)
        (:parent TOP-WIN))

    (opal:update GESTURE-WORK-WIN)

    (s-value GESTURE-WORK-WIN :aggregate
        (create-instance 'GESTURE-WORK-WIN-AGG opal:aggregate
            (:left 0) (:top 0)
            (:width (o-formula (gvl :window :width)))
            (:height (o-formula (gvl :window :height)))))

    ;; create a polyline for display large gesture examples in GESTURE-WORK-WIN
    (create-instance 'GESTURE-EXAMPLE-POLY opal:aggregadget
        (:parts `((:poly ,opal:polyline
             (:point-list NIL))
          (:arrow ,opal:arrowhead
              (:line-style ,orange-line-2)
              (:length 18)
              (:diameter 14)
              (:points ,(o-formula 
                 (gvl :parent :poly :point-list)))
              (:visible ,(o-formula ;; make sure points exist
                  (and (gvl :points) 
                       (gvl :head-x) (gvl :head-y))))
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

    ;; intial instructions as to what gesture window is for
    (create-instance 'INSTRUCTIONS-TEXT opal:text
    ;; note: can't be constant (changes)
        (:string "Draw Examples of Gestures Here")
    (:font (opal:get-standard-font :serif :bold :medium))
    (:left (o-formula (- (round (gvl :parent :window :width) 2) 
                 (round (opal:string-width (gvl :font)
                               (gvl :string)) 2))))
    (:top (o-formula (round (gvl :parent :window :height) 3))))

    ;; create a gesture interactor that uses a nil classifier to get
    ;; back the mouse trace.
    (create-instance 'GESTURE-INTER inter:gesture-interactor
        (:window GESTURE-WORK-WIN)
        (:start-where (list :in GESTURE-WORK-WIN)) 
        (:running-where (list :in GESTURE-WORK-WIN))
        (:start-event :any-mousedown)
        (:classifier NIL) 
        (:final-function #'handle-gesture)
        (:min-non-ambig-prob 0)
        (:max-dist-to-mean 10000)
    (:start-action
     #'(lambda (an-interactor object-under-mouse point)
         ;; erase displayed gesture and instructions (if any)
         (s-value INSTRUCTIONS-TEXT :visible NIL)
         (s-value GESTURE-EXAMPLE-POLY :poly :point-list NIL)
         (call-prototype-method an-interactor object-under-mouse point))))

    (opal:add-components (g-value GESTURE-WORK-WIN :aggregate)
             GESTURE-EXAMPLE-POLY INSTRUCTIONS-TEXT)
    (opal:add-component (g-value CLASS-WORK-WIN :inner-aggregate) 
                        CLASS-ICON-AGGL)
    (opal:add-component (g-value EXAMPLES-WORK-WIN :inner-aggregate) 
            GESTURE-ICON-AGGL)
    (opal:add-components TOP-AGG
             CLASSIFIER-MENU GCLASS-NAME CLASSES-LABEL
             CLASS-MENU GESTURE-NAME-IN GESTURE-NAME-OUT 
             GESTURE-NAP GESTURE-DIST
             EXAMPLES-LABEL EXAMPLES-MENU MODE-TOGGLE CANVAS-LABEL)

    ;; initialize globals
    (init-classifier)
    (setf *last-filename* NIL)

    ;; check for optional classifier to preload
    (when (and initial-classifier initial-classifier)
      (s-value CLASS-ICON-AGGL :items initial-examples)
      (setf *cur-classifier* initial-classifier))

    ;; check for initial gesture name
    (when initial-gesture-name
      (s-value GESTURE-NAME-IN :value initial-gesture-name))

    ;; remember the final-function to call
    (when final-function
      (setf *final-function* final-function))

    (opal:update TOP-WIN) 

    ;; print out instructions
    (format t "  To use AGATE, the Garnet gesture trainer, type the name of a gesture~%")
    (format t "in the \"Gesture Class Name\" field and then give about 15 examples of the~%")
    (format t "gesture on the \"Canvas\".  You can then press the \"New Gesture\" button,~%")
    (format t "type a new gesture name, and give examples for it.  At any point, you can~%")
    (format t "try out the gestures trained so far, by switching to \"Recognize\" mode by~%")
    (format t "pressing the \"Recognize\" button.  After you give a gesture in \"Recognize\"~%")
    (format t "mode, AGATE will print the name in the \"Gesture Name\" field.~%")

    (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))

(format t "Type (agate:do-go) to begin.~%")

