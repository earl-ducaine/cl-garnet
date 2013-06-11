;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  CHANGE LOG:
;;;
;;;  07/14/93 amickish - Declared TWO-POINT-INTER-MENU special in
;;;                      Two-Point-Feedback-Obj-Fn

(in-package "LAPIDARY")

(defvar *TWO-POINT-INTER-QUEUE* NIL)

;; accessor functions for various aggregates in the two-point interactor
;; menu


(defmacro TWO-POINT-START-WHERE ()
  `(g-value TWO-POINT-INTER-MENU :start-where))
(defmacro TWO-POINT-GRAY-SHADE1 ()
  `(g-value TWO-POINT-INTER-MENU :non-line-parameters :contents :gray-shade))
(defmacro TWO-POINT-FEEDBACK-OBJ ()
  `(g-value TWO-POINT-INTER-MENU :feedback-obj))



;;; *********************************
;;; Change the name of the interactor
;;; *********************************
;;;    -- interactor-name is a string
(defun TWO-POINT-INTERACTOR-NAME-FN (gadget interactor-name)
  (declare (special two-point-inter-menu))
  (declare (ignore gadget))
      (dialog-enqueue :known-as
                      (if (string/= "" interactor-name)
                          (read-from-string
                           (concatenate 'string ":" interactor-name)))
                      *TWO-POINT-INTER-QUEUE*))

(defun TWO-POINT-START-ANYWHERE (button button-label)
  (declare (special two-point-inter-menu))
  (declare (ignore button))
  (let ((start-where (TWO-POINT-START-WHERE)))
    (s-value start-where :field-string nil)
    (s-value start-where :type nil)
    (s-value start-where :value button-label)
    (dialog-enqueue :start-where t *two-point-inter-queue*)))

;;;    :start-where is in a single object
(defun TWO-POINT-IN-BOX (button button-label)
  (declare (special two-point-inter-menu))
  (declare (ignore button))
  (let ((selection (car (g-value *SELECTION-INFO* :selected)))
	(start-where (TWO-POINT-START-WHERE)))
    (if selection
	(progn
	  (s-value start-where :field-string (name-for-schema selection))
	  (s-value start-where :value button-label)
	  (s-value start-where :type nil)
	  (dialog-enqueue :start-where
			  `(:in-box ,selection)
			  *TWO-POINT-INTER-QUEUE*))
	(progn
	  (s-value start-where :field-string nil)
	  (s-value start-where :value nil)
	  (s-value start-where :type nil)))))



(defun OBJECT-TYPE (button button-label)
  (declare (special two-point-inter-menu))
  (cond ((string= button-label "Create Line")
;;          (s-value (TWO-POINT-GRAY-SHADE1) :visible t) or
            (s-value (g-value TWO-POINT-INTER-MENU :non-line-parameters 
                                             :contents :gray-shade) :visible t)
            (s-value (g-value TWO-POINT-INTER-MENU :line-parameters 
                                             :contents :gray-shade) :visible nil)
            (dialog-enqueue :line-p t *TWO-POINT-INTER-QUEUE*))

         ((string= button-label "Create Non-Line")
            (s-value (g-value TWO-POINT-INTER-MENU :non-line-parameters 
                                             :contents :gray-shade) :visible nil)
            (s-value (g-value TWO-POINT-INTER-MENU :line-parameters 
                                             :contents :gray-shade) :visible t)
            (dialog-enqueue :line-p nil *TWO-POINT-INTER-QUEUE*))

         (t 
            (s-value (g-value TWO-POINT-INTER-MENU :non-line-parameters 
                                             :contents :gray-shade) :visible nil)
            (s-value (g-value TWO-POINT-INTER-MENU :line-parameters 
                                             :contents :gray-shade) :visible nil)
	    (create-custom-inter-constraint (g-value button :window :inter) 
					    :line-p
					    '*TWO-POINT-INTER-QUEUE*))))


(defun NON-LINE-MIN-WIDTH (gadget value)
  (setf *two-point-inter-queue*
	(enqueue-int-value gadget value :min-width *two-point-inter-queue*)))

(defun NON-LINE-MIN-HEIGHT (gadget value)
  (setf *two-point-inter-queue*
	(enqueue-int-value gadget value :min-height *two-point-inter-queue*)))

(defun NON-LINE-MAY-FLIP-OVER (button button-label)
  (declare (ignore button))
  (dialog-enqueue :flip-if-change-side button-label *TWO-POINT-INTER-QUEUE*))


(defun LINE-MIN-LENGTH (gadget value)
  (setf *two-point-inter-queue*
	(enqueue-int-value gadget value :min-length *two-point-inter-queue*)))



(defun ABORT-INCREASE (button button-label)
  (declare (ignore button))
  (cond ((string= button-label "Abort if Too Small")
            (dialog-enqueue :abort-if-too-small t *TWO-POINT-INTER-QUEUE*))

         (t 
           (dialog-enqueue :abort-if-too-small nil *TWO-POINT-INTER-QUEUE*))))

(defun TWO-POINT-FEEDBACK-OBJ-FN (feedback-obj-box button-label)
  (declare (special *selection-info* TWO-POINT-INTER-MENU))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (cond ((null selection)
	   (s-value (TWO-POINT-FEEDBACK-OBJ) :field-string nil)
	   (s-value (TWO-POINT-FEEDBACK-OBJ) :value nil)
	   (lapidary-error "please make a selection, then press the
interim feedback button again"))
	((null (cdr selection)) ; only one selection
	 (s-value (TWO-POINT-FEEDBACK-OBJ) :field-string
		  (name-for-schema (car selection)))
	 (s-value (TWO-POINT-FEEDBACK-OBJ) :value button-label)
	 (dialog-enqueue :feedback-obj selection *TWO-POINT-INTER-QUEUE*))
	(t ; multiple selections
	 (s-value (TWO-POINT-FEEDBACK-OBJ) :field-string
		  (princ-to-string selection))
	 (s-value (TWO-POINT-FEEDBACK-OBJ) :value button-label)
	 ;; pop up C32 and ask the user to enter a formula that
	 ;; selects which feedback object to use
	 (get-inter-feedback-formula (g-value feedback-obj-box :window :inter)
				     :feedback-obj
				     selection
				     '*two-point-inter-queue*)))))

(defun FEEDBACK-OBJ-STANDARD (button button-label)
  (declare (special two-point-inter-menu))
  (declare (ignore button))
  (s-value (g-value TWO-POINT-INTER-MENU :feedback-obj) :value button-label)
  (s-value (g-value TWO-POINT-INTER-MENU :feedback-obj) :field-string nil)
  (dialog-enqueue :feedback-obj "standard" *TWO-POINT-INTER-QUEUE*))



(defun FEEDBACK-OBJ-NONE (button button-label)
  (declare (special two-point-inter-menu))
  (declare (ignore button))
  (s-value (g-value TWO-POINT-INTER-MENU :feedback-obj) :value button-label)
  (s-value (g-value TWO-POINT-INTER-MENU :feedback-obj) :field-string nil)
  (dialog-enqueue :feedback-obj nil *TWO-POINT-INTER-QUEUE*))


(defun FINAL-FUNCTION-SEL (button button-label)
  (declare (special two-point-inter-menu))
  (declare (ignore button))
#|
  (s-value (g-value TWO-POINT-INTER-MENU :final-function :contents :prototype) 
                    :value nil)
|#
  (dialog-enqueue :final-function 
		  (if (string= "" button-label) 
		      nil
		      (read-from-string button-label))
                  *TWO-POINT-INTER-QUEUE*))



(defun FINAL-FUNCTION-PROTOTYPE (button button-label)
  (declare (special two-point-inter-menu))
  (declare (ignore button))
  (s-value (g-value TWO-POINT-INTER-MENU :final-function :contents :final-function-box) 
                    :value nil)
  (dialog-enqueue :final-function (read-from-string button-label)
                    *TWO-POINT-INTER-QUEUE*))

;;;*****************
;;;   UN-TITLED-FRAME
;;;*****************

(create-instance 'UN-TITLED-FRAME opal:aggregadget
   (:maybe-constant '(:left :top :width :height))
   (:left (o-formula (gvl :parent :left)))
   (:top (o-formula (gvl :parent :top)))
   (:width (o-formula (+ 30 (gvl :parent :contents :width))))
   (:height (o-formula (+ 30 (gvl :parent :contents :height))))
   (:string "")
   (:font *bold-font*)
   (:parts
    `((:frame ,opal:rectangle
                (:left ,(o-formula (gvl :parent :left)))
                (:top ,(o-formula (gvl :parent :top)))
                (:width ,(o-formula (gvl :parent :width)))
                (:height ,(o-formula (+ 20 (gvl :parent :parent :contents
                                                :height))))))))




(defun two-point-inter-do-go ()
  (let ((kr::*constants-disabled* nil))
  (two-point-inter-do-stop)

(create-instance 'TWO-POINT-INTER-WIN inter:interactor-window
  (:title "two point interactor")
  (:queue '*two-point-inter-queue*)
  (:left 950)
  (:top 125)
  (:width 625)
  (:height 597)) 

(opal:update TWO-POINT-INTER-WIN)

(create-instance 'TWO-POINT-INTER-MENU opal:aggregadget
   (:constant '(:left :top :width :height))
   (:left 10)
   (:top 10)
   (:parts
    `((:title ,opal:text
	  (:constant (t))
          (:left ,(o-formula (gvl :parent :left)))
          (:top ,(o-formula (gvl :parent :top)))
          (:string "Two Point Interactor") 
          (:font ,*very-large-bold-italic-serif-font*))


      (:known-as ,garnet-gadgets:labeled-box
	  (:constant (t))
          (:left ,(o-formula (+ 10 (gvl :parent :left))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :title)))))
          (:label-string "Interactor Name:")
          (:value NIL)
          (:min-frame-width 150)
          (:selection-function TWO-POINT-INTERACTOR-NAME-FN))


      (:start-where ,START-WHERE
	  (:constant (t))
          (:items (("Start Anywhere in Window" TWO-POINT-START-ANYWHERE)
		   ("Start in Box" TWO-POINT-IN-BOX))))

      (:act-buttons ,ACT-BUTTONS
	  (:constant (t))
          (:queue *TWO-POINT-INTER-QUEUE*)
          (:left ,(o-formula (+ 200 (opal:gv-right (gvl :parent :known-as))))))
      
      (:line-p ,garnet-gadgets:radio-button-panel
	  (:constant (t))
          (:left ,(o-formula (+ 5 (gvl :parent :left))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :start-where)))))
          (:width ,(o-formula (let ((width 0))
                                (gvl :radio-button-list :components)
                                (opal:do-components (gvl :radio-button-list)
                                 #'(lambda (button)
                                     (setf width
                                           (+ width (g-value button :width)))))
                                (+ (gvl :v-spacing) width))))
          (:height ,(o-formula (gvl :fixed-height-size)))
          (:direction :horizontal)
          (:fixed-width-p NIL)
          ;; (:value "Create Line")                            ;; default
          (:items ("Create Line" "Create Non-Line" "<Formula>"))
          (:font ,opal:default-font)
          ;; need an extra bit of function here since :value formula was
          ;; overridden with initial value
          (:selection-function OBJECT-TYPE)) 


      (:non-line-parameters ,opal:aggregadget
	  (:constant (:left :top :width :height))
          (:left ,(o-formula (+ 10 (gvl :parent :left))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :line-p)))))
          (:parts
           ((:un-titled-frame ,UN-TITLED-FRAME
		(:constant (t))
                (:string ""))
            (:contents ,opal:aggregadget
          	(:constant (:left :top :width :height))
                (:width ,(o-formula (+ (gvl :min-height :width) 10))) 
                (:height ,(o-formula (+ (gvl :title :height)
                                        10 (gvl :min-width :height)
                                        10 (gvl :min-height :height)
                                        2 (gvl :may-flip-over :height))))
                (:value ,(o-formula (gvl :parent :value)))
                (:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
                 ((:title ,opal:text
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top ,(o-formula (+ 15 (gvl :parent :parent :top))))
                      (:string "Non-Line Parameters"))
                  (:min-width ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top ,(o-formula (+ 5 (opal:gv-bottom
                                              (gvl :parent :title)))))
                      (:label-string "Min-Width")
		      (:old-value "")
                      (:label-font ,opal:default-font)
                      (:min-frame-width 100)
                      (:value NIL)
                      (:selection-function NON-LINE-MIN-WIDTH))
                  (:min-height ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top ,(o-formula (+ 5 (opal:gv-bottom
                                              (gvl :parent :min-width)))))
                      (:label-string "Min-Height")
		      (:old-value "")
                      (:label-font ,opal:default-font)
                      (:min-frame-width 100)
                      (:value NIL)
                      (:selection-function NON-LINE-MIN-HEIGHT))
                  (:may-flip-over ,garnet-gadgets:x-button
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top  ,(o-formula (+ 5  (opal:gv-bottom
                                              (gvl :parent :min-height)))))
                      (:button-width 20)
                      (:button-height 20)
                      (:shadow-offset 5)
                      (:text-offset 5)
                      (:gray-width 3)
                      (:text-on-left-p T)
                      (:string "May Flip Over")
                      (:value-obj NIL)
                      (:selection-function NON-LINE-MAY-FLIP-OVER))
                  (:gray-shade ,opal:rectangle
		      (:constant (t :except :visible))
                      (:left ,(o-formula (- (gvl :parent :parent :left) 1)))
                      (:top ,(o-formula (- (gvl :parent :parent :top) 1)))
                      (:width ,(o-formula (+ 2 (gvl :parent :parent :width))))
                      (:height ,(o-formula (+ 2 (gvl :parent :parent :height))))
                      (:visible nil)
                      (:draw-function :and)
                      (:filling-style ,opal:gray-fill))
            )))))) 



      (:line-parameters ,opal:aggregadget
	  (:constant (:left :top :width :height))
          (:left ,(o-formula (+ 20 (opal:gv-right
                                    (gvl :parent :non-line-parameters)))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :line-p)))))
          (:parts
           ((:un-titled-frame ,UN-TITLED-FRAME
		(:constant (t))
                (:string ""))
            (:contents ,opal:aggregadget
		(:constant (:left :top :width :height))
                (:width ,(o-formula (+  (gvl :min-length :width) 10 )))
                (:height ,(o-formula (+ (gvl :title :height)
                                        10 (gvl :min-length :height))))
                (:value ,(o-formula (gvl :parent :value)))
                (:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
                 ((:title ,opal:text
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top ,(o-formula (+ 15 (gvl :parent :parent :top))))
                      (:string "Line Parameters"))
                  (:min-length ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top ,(o-formula (+ 5 (opal:gv-bottom
                                              (gvl :parent :title)))))
                      (:label-string "Min-Length")
		      (:old-value "")
                      (:label-font ,opal:default-font)
                      (:min-frame-width 100)
                      (:value NIL)
                      (:selection-function LINE-MIN-LENGTH))
                  (:gray-shade ,opal:rectangle
		      (:constant (t :except :visible))
                      (:left ,(o-formula (- (gvl :parent :parent :left) 1)))
                      (:top ,(o-formula (- (gvl :parent :parent :top) 1)))
                      (:width ,(o-formula (+ 2 (gvl :parent :parent :width))))
                      (:height ,(o-formula (+ 2 (gvl :parent :parent :height))))
                      (:visible nil)
                      (:draw-function :and)
                      (:filling-style ,opal:gray-fill))

            ))))))


      (:abort-if-too-small ,garnet-gadgets:radio-button-panel
	  (:constant (t))
          (:left ,(o-formula (+ 20 (gvl :parent :left))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :non-line-parameters)))))
          (:width ,(o-formula (let ((width 0))
                                (gvl :radio-button-list :components)
                                (opal:do-components (gvl :radio-button-list)
                                 #'(lambda (button)
                                     (setf width
                                           (+ width (g-value button :width)))))
                                (+ (gvl :v-spacing) width))))
          (:height ,(o-formula (gvl :fixed-height-size)))
          (:direction :horizontal)
          (:fixed-width-p NIL)
          ;; (:value "or Increase to Min Size")               ;; default
          (:items ("Abort if Too Small" "or Increase to Min Size"))
          (:font ,opal:default-font)
          ;; need an extra bit of function here since :value formula was
          ;; overridden with initial value
          (:selection-function ABORT-INCREASE)) 




      (:feedback-obj ,opal:aggregadget
	  (:constant (:left :top :width :height))
          (:left ,(o-formula (+ 10 (gvl :parent :left))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :abort-if-too-small)))))
          (:parts
           ((:titled-frame ,TITLED-FRAME
		(:constant (t))
                (:string ":feedback-obj"))
            (:contents ,opal:aggregadget
		(:constant (:left :top :width :height))
                (:width ,(o-formula (+ (gvl :feedback-obj-box :width)
                                       10 (gvl :standard-feedback :width))))
                (:height ,(o-formula (+ (gvl :standard-feedback :height)
                                     -10  (gvl :none-button :height))))
                (:value ,(o-formula (gvl :parent :value)))
                (:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
		 ((:feedback-obj-box ,SELECT-BOX
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top ,(o-formula (+ 10 (gvl :parent :parent :top))))
		      (:string "Interim Feedback")
		      (:value ,(o-formula (gvl :parent :value)))
		      (:min-frame-width 125)
		      (:selection-function TWO-POINT-FEEDBACK-OBJ-FN))
                  (:standard-feedback ,garnet-gadgets:radio-button
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (opal:gv-right
                                                 (gvl :parent :feedback-obj-box))))) 
                      (:top ,(o-formula (+ 10 (gvl :parent :parent :top))))
                      (:string "Standard Feedback")	
		      (:value ,(o-formula (if (string= (gvl :parent :value)
						       "Standard Feedback")
					      "Standard Feedback")))
                      (:selection-function FEEDBACK-OBJ-STANDARD))
                  (:none-button ,garnet-gadgets:radio-button
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (opal:gv-right
                                                 (gvl :parent :feedback-obj-box))))) 
                      (:top ,(o-formula (+ 0 (opal:gv-bottom
                                                (gvl :parent :standard-feedback)))))
                      (:string "None")
		      (:value ,(o-formula (if (string= (gvl :parent :value)
						       "None")
					      "None")))
                      (:selection-function FEEDBACK-OBJ-NONE))

     ))))))



      (:final-function ,garnet-gadgets:labeled-box
		      (:constant (t))
		      (:left ,(o-formula (+ 10 (gvl :parent :left))))
		      (:top ,(o-formula (+ 10 (opal:gv-bottom
					       (gvl :parent :feedback-obj)))))
                      (:label-string "Final Function:")
                      (:label-font ,opal:default-font)
                      (:min-frame-width 75)
                      (:value NIL)
                      (:selection-function FINAL-FUNCTION-SEL))

#|
      (:final-function ,opal:aggregadget
	  (:constant (:left :top :width :height))
          (:left ,(o-formula (+ 10 (gvl :parent :left))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :feedback-obj)))))
          (:parts
           ((:titled-frame ,TITLED-FRAME
		(:constant (t))
                (:string ":final-function"))
            (:contents ,opal:aggregadget
	        (:constant (:left :top :width :height))
                (:width ,(o-formula (+ (gvl :final-function-box :width)
                                       10 (gvl :prototype :width))))
                (:height ,(o-formula (+ 2 (gvl :prototype :height))))
                (:value ,(o-formula (gvl :parent :value)))
                (:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
                 ((:final-function-box ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
                      (:top ,(o-formula (+ 15 (gvl :parent :parent :top))))
                      (:label-string "Final Function:")
                      (:label-font ,opal:default-font)
                      (:min-frame-width 75)
                      (:value NIL)
                      (:selection-function FINAL-FUNCTION-SEL))
                  (:prototype ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (opal:gv-right
                                                (gvl :parent :final-function-box)))))
                      (:top ,(o-formula (+ 15 (gvl :parent :parent :top))))
                      (:label-string "or Prototype:")
                      (:label-font ,opal:default-font)
                      (:min-frame-width 75)
                      (:value NIL)
                      (:selection-function FINAL-FUNCTION-PROTOTYPE))

      ))))))
|#





      (:event-panel ,event-panel
	  (:constant (t))
          (:left ,(o-formula (+ 20 (gvl :parent :left))))
          (:top ,(o-formula (+ 10 (opal:gv-bottom
                                   (gvl :parent :final-function)))))
          (:queue *TWO-POINT-INTER-QUEUE*))



   )))



(opal::fix-update-slots (g-value two-point-inter-menu :start-where :contents
				:select-box-panel))

(s-value TWO-POINT-INTER-WIN :aggregate two-point-inter-menu)

(opal:update TWO-POINT-INTER-WIN)))

(defun two-point-inter-do-stop ()
  (when (boundp 'TWO-POINT-INTER-WIN)
    (opal:destroy TWO-POINT-INTER-WIN)))



