;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Write Aggregadgets to a file.  
;;; 
;;; Roger B. Dannenberg, 1990
;;;
;;; $Id::                                                             $

#| Implementation details:

There are two issues to worry about: first, we have to figure out
what to save and in what format, e.g. anything that gets created
automatically as a consequence of instantiation should not be
written out, but everything else has to be specified using the
aggregadget syntax.

The second issue is that we cannot just write out parts as we come
to them.  If we did, then we would end up writing out a skeleton
for the entire tree structure.  This is semantically correct, but
one of the design goals is NOT to write out the structure when it
would be created automatically anyway.  So, we only want to write
out enough of the skeleton to hold slot specifications and new part
specifications that would not occur in a default instantiation.
Unfortunately, we do not know whether a parent should be written out
until we have looked at the children.

Rather than making two passes over the aggregadgets, I use a form
of lazy evaluation: no output occurs until the depth-first traversal
of the aggregadget discovers something that must be written out.
Then, output is generated along the most direct path from where
slots were last written to where slots must now be written.
For example, the output can skip over subtrees that are just
instances of the prototype.

Thus, there are two traversals going on: a complete depth-first
traversal that examines the entire structure to determine what to
write out, and a partial traversal that generates output.  The
complete traversal is performed using ordinary recursion.  Progress
of the traversal is recorded in a stack called *obj-stack* that
contains the path from the root to the current object.  The
output traversal is not recursive and it uses a stack called 
*output-stack* and a parallel structure called *output-state*.

The biggest trick in generating output is getting all of the indentation
and parens right as you move around in the tree generating names of
objects.  *output-stack* is used to keep track of the current location,
and *output-state* is used to keep track of what has been written for
that location.  Here is an example output:

      (create-instance 'my-button a-button
        (:left 10)
        (:parts `(
          (:shad ,circle
            (:left 15))
          (:box ,circle
            (:left 10)
            (:parts (
              (:check-mark :modify
                (:height 12))))))))

The *output-state* can be one of the following:
   NIL -- nothing has been written for the current part
   :NAME -- the open-paren and name has been written for the current part
   :CLASS -- the class has been specified
   :PARTS -- the string "(:parts `(" has been written
   :ITEM-PROTOTYPE -- the string "(:item-prototype `" has been written
   :INTERACTORS -- the string "(interactors (" has been written
   :FINISHED -- close parens have been written for the current part
      (this state is not used because the part is popped when finished)
I think the *output-state* is implied by the state of the stacks and
where you are in the code, but I found the coding to be very tricky.
It is much simpler to just keep an explicit record of where you are,
so I introduced *output-state* to keep track of things.

Here is the algorithm for deciding what to write out:
Don't save standard slots that are created automatically.
Inherited formulas are also created automatically, so don't write them out.
Links to named components and behaviors are created automatically, so
    don't write them out.
After writing new slots, run through components and behaviors:
  We write out a component or behavior if it is not 
    exactly what we would get if the parent were instantiated.
  If we write out any part, we will write out at least the names
    of every sibling in the parts list so that parts will be
    created in the right order.

Aggrelists.
    Aggrelists are similar to aggregadgets except for all the
extra slots that maintain screen position of the list elements.
There are two types, normal and itemized aggrelists.  
Normal aggrelists will be written out as if they were aggregadgets,
except since instantiation does not create components by default,
each component will be explicitly listed in the :parts list.  The
class will be given explicitly since there is no default prototype
for an element.
Itemized aggrelist elements have a default prototype and either an
implied or a specified number, based on the :items slot.  For
itemized aggrelists, there will be no :parts list, and components
MUST be instances of the :item-prototype-object, which is an aggrelist or
an aggregadget.  The :item-prototype will be handled as a special
case; the syntax is that of one item of a :parts list, and
the default prototype is the :item-prototype-object of the prototype
aggrelist.  
|#

(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(write-gadget e-formula *verbose-write-gadget*
	    *required-names* *standard-names* *defined-names*)))

(defmacro Write-Slots (agget normal-proto components behaviors
			     suppress-children?)
  (let ((the-agget (gensym))
	(the-normal-proto (gensym))
	(the-components (gensym))
	(the-behaviors (gensym))
	(the-suppress-children? (gensym)))
    `(let ((,the-agget ,agget)
	   (,the-normal-proto ,normal-proto)
	   (,the-components ,components)
	   (,the-behaviors ,behaviors)
	   (,the-suppress-children? ,suppress-children?))
      (kr-send ,the-agget :write-slots ,the-agget ,the-normal-proto
                                       ,the-components ,the-behaviors
                                       ,the-suppress-children?))))

(defvar *defined-names* nil)

;;; These schema should not be translated but instead dumped as is:
;;;   schema that have standard definitions in Garnet.
;;;
(defparameter *standard-names* 
  '(opal:graphic-quality opal:filling-style opal:default-filling-style 
    opal:purple-fill opal:cyan-fill opal:orange-fill opal:yellow-fill
    opal:blue-fill opal:green-fill opal:red-fill
    opal:black-fill opal:white-fill opal:gray-fill opal:light-gray-fill  
    opal:dark-gray-fill opal:motif-gray-fill opal:motif-blue-fill
    opal:motif-orange-fill opal:motif-green-fill
    opal:motif-light-gray-fill opal:motif-light-blue-fill
    opal:motif-light-orange-fill opal:motif-light-green-fill
    opal:motif-light-gray opal:motif-light-blue opal:motif-light-orange
    opal:motif-light-green opal:line-style opal:default-line-style 
    opal:thin-line opal:dotted-line opal:dashed-line
    opal:purple-line opal:orange-line opal:yellow-line opal:cyan-line
    opal:blue-line opal:green-line opal:red-line
    opal:line-0 opal:line-1 opal:line-2 opal:line-4 opal:line-8 
    opal:color opal:black opal:white opal:orange opal:purple opal:cyan
    opal:yellow opal:blue opal:green opal:red opal:motif-gray opal:motif-blue
    opal:motif-orange opal:motif-green
    opal:font opal:font-from-file opal:default-font
    opal:aggregate opal:rectangle opal:line opal:roundtangle opal:circle
    opal:text opal:multi-text opal:cursor-text opal:cursor-multi-text
    opal:oval opal:arc opal:bitmap opal:multipoint opal:polyline
    opal:pixmap opal:aggregadget opal:aggrelist opal:null-object
    opal::multifont-text

    inter:Angle-Interactor inter:button-interactor inter:interactor-window
    inter:priority-level inter:normal-priority-level inter:high-priority-level
    inter:running-priority-level inter:interactor inter:menu-interactor
    inter:Move-Grow-Interactor inter:text-interactor inter:Two-Point-Interactor
    inter::gesture-interactor

    GG::V-SCROLL-BAR GG::H-SCROLL-BAR GG::V-SLIDER GG::H-SLIDER
    GG::TRILL-DEVICE GG::GAUGE
    GG::ARROW-LINE GG::DOUBLE-ARROW-LINE
    GG::GRAPHICS-SELECTION GG::MULTI-GRAPHICS-SELECTION
    GG::RADIO-BUTTON GG::RADIO-BUTTON-PANEL
    GG::TEXT-BUTTON GG::TEXT-BUTTON-PANEL
    GG::X-BUTTON GG::X-BUTTON-PANEL
    GG::OPTION-BUTTON  GG::POPUP-MENU-BUTTON
    GG::LABELED-BOX GG::SCROLLING-LABELED-BOX GG::SCROLLING-INPUT-STRING
    GG::MENU GG::SCROLLING-MENU GG::MENUBAR
    GG::ERROR-GADGET GG::QUERY-GADGET GG::BROWSER-GADGET

    GG::MOTIF-GADGET-PROTOTYPE GG::MOTIF-BACKGROUND GG::MOTIF-V-SCROLL-BAR
    GG::MOTIF-H-SCROLL-BAR GG::MOTIF-SLIDER GG::MOTIF-GAUGE
    GG::MOTIF-TEXT-BUTTON GG::MOTIF-TEXT-BUTTON-PANEL
    GG::MOTIF-RADIO-BUTTON GG::MOTIF-RADIO-BUTTON-PANEL
    GG::MOTIF-CHECK-BUTTON GG::MOTIF-CHECK-BUTTON-PANEL
    GG::MOTIF-OPTION-BUTTON  GG::MOTIF-MENUBAR  GG::MOTIF-TRILL-DEVICE
    GG::MOTIF-MENU GG::MOTIF-MENU-INTER GG::MOTIF-SCROLLING-MENU
    GG::MOTIF-SCROLLING-LABELED-BOX  GG::MULTIFONT-GADGET
    GG::MOTIF-LOAD-GADGET GG::MOTIF-SAVE-GADGET
    GG::MOTIF-ERROR-GADGET GG::MOTIF-QUERY-GADGET

    GG::MOTIF-SELECTION-BOX  GG::MOTIF-RECT

    GARNETDRAW::MOVING-AGG GARNETDRAW::MOVING-LINE GARNETDRAW::MOVING-ARROWLINE
    GARNETDRAW::MOVING-DOUBLEARROWLINE GARNETDRAW::MOVING-RECT
    GARNETDRAW::MOVING-ROUNDTANGLE GARNETDRAW::MOVING-OVAL
    
    GILT::type-text-button-panel  GILT::type-x-button-panel
    GILT::type-radio-button-panel  GILT::type-scrolling-menu
    GILT::type-okcancel  GILT::type-okapplycancel  GILT::type-menu
    GILT::type-h-scroll-bar  GILT::type-h-slider
    GILT::type-v-scroll-bar  GILT::type-v-slider
    GILT::type-trill-device  GILT::type-labeled-box  GILT::type-gauge
    GILT::type-scrolling-labeled-box  GILT::type-rectangle  GILT::type-line
    GILT::type-text  GILT::type-bitmap  GILT::type-generic

    GILT::type-motif-text-button-panel  GILT::type-motif-check-button-panel
    GILT::type-motif-radio-button-panel  GILT::type-motif-menu
    GILT::type-motif-h-scroll-bar  GILT::type-motif-v-scroll-bar
    GILT::type-motif-slider  GILT::type-motif-gauge  GILT::type-line
    GILT::type-motif-scrolling-labeled-box  GILT::type-rectangle
    GILT::type-motif-background
    
    )
  "what name to output for standard schema")

;;; :output-descriptor slot tells what the prototype should be for a
;;; graphic-quality, the slots to write, and their default values
;;;
(progn
  (s-value line-style :output-descriptor 
	   `(,line-style 
	     (:stipple nil)
	     (:background-color ,white)
	     (:foreground-color ,black)
	     (:dash-pattern nil)
	     (:join-style :miter)
	     (:cap-style :butt)
	     (:line-style :solid)
	     (:line-thickness 0)))
  (s-value color :output-descriptor
	   `(,color
	     (:color-name NIL)
	     (:blue 1.0)
	     (:green 1.0)
	     (:red 1.0)))
  (s-value filling-style :output-descriptor
	   `(,filling-style
	     (:stipple nil)
	     (:background-color ,white)
	     (:foreground-color ,black)
	     (:fill-rule :even-odd)
	     (:fill-style :solid)))
  (s-value font :output-descriptor
	   `(,font
	     (:size :medium)
	     (:face :roman)
	     (:family :fixed)))
  (s-value font-from-file :output-descriptor
	   `(,font-from-file
	     (:font-name (g-value font-from-file :font-name))
	     (:font-path nil))))

(defmacro defined-name-p (schema)
  `(car (member (kr::schema-name ,schema) *defined-names*)))

(defvar *kr-debug-package* (find-package "KR-DEBUG"))

(defvar *verbose-write-gadget* nil) ;; controls debugging output

;; This is a list of non-standard schema that have been output.
;; A warning is issued the first time.
(defvar *required-names* nil)

;; test to see if expression contains reference to an object
;;
(defun contains-reference (expression)
  (cond ((or (formula-p expression) (schema-p expression)) t)
	((consp expression) 
	 (or (contains-reference (car expression))
	     (contains-reference (cdr expression))))
	(t nil)))


(defun write-gadget (gadget file-name &optional (initialize? t))
  (when initialize?
    (setf *defined-names* *standard-names*)
    (setf *required-names* nil))
  ;; force argument to be a list
  (if (not (listp gadget)) (setf gadget (list gadget)))
  (if (eq file-name t)
      (dolist (g gadget) 
	(print-gadget g)
	(terpri)
	(terpri))
      (with-open-file (*standard-output* file-name :direction :output
					 :if-exists :supersede)
	(dolist (g gadget) 
	  (print-gadget g) 
	  (terpri)
	  (terpri)))))


(defun e-formula (expression)
  "evaluates an expression to create a formula, saving the expression
  so that the formula can be saved to a file"
  (let ((f (eval expression)))
    ;; dzg - the following is broken and will NOT work with KR 2.0.5
    (s-value f :source expression)
    f))


(defun unreadable-structure (v)
  (and (> (length v) 2)
       (char= (schar v 1) #\#)
       (or (char= (schar v 2) #\<)
	   (char= (schar v 2) #\S))))


(defun output-cached-value (cv)
  (when cv
    (if (listp cv)            ; quote the cv if appropriate
	(format t " '~S" cv)
        (format t " ~S" cv))))


;;; Output the meta-information associated with the <formula>, if any.  Do
;;; not do this if the information is the same as in the parent formula.
;;;
(defun output-meta-if-needed (formula)
  (let ((meta (kr::a-formula-meta formula)))
    (when (and meta (null (g-value meta :is-a)))
      (write-string " NIL")
      (write-slots meta nil nil nil nil))))


;;; output a formula
;;; 2/19/1992  dzg - rewrote to work with KR 2.0.5
;;; 3/11/1991  dzg - changed to fix problem with saving bitmaps
;;; 3/30/1992  dzg - fixed to dump O-FORMULAS properly.
;;; 4/27/1992  dzg - fixed to dump formulas whose value is a list properly.
;;;
(defun output-formula (value)
  (let ((f (if (listp (g-formula-value value :FUNCTION))
               (g-formula-value value :LAMBDA)))
        (cv (g-formula-value value :CACHED-VALUE))
        (isa (g-formula-value value :FORMULA-IS-A))
        std-name e-formula-source)
    ;; first see if this formula is inherited from a standard one
    ;; like :visible from opal:aggregate
    (when (and isa (formula-p isa))
      ;; go up the is-a links to the top
      (do ((a-formula isa (g-formula-value isa :FORMULA-IS-A)))
          ((null a-formula))
        (setf isa a-formula))
      (setf std-name (defined-name-p isa)))

    ;; don't write cached value if it contains a reference to an object
    (if (contains-reference cv) (setf cv nil))

    ;; Make sure we do not output an initial value which is not readable
    ;; (for example, an XLIB:IMAGE object), i.e., whose printed
    ;; representation begins with #S or #<
    (if cv
        (let ((v (format nil " ~S" cv)))
          (if (unreadable-structure v)
              (setf cv NIL)))
        (setf cv NIL))

    (cond (std-name
	   (format t "(formula ~S" std-name)
	   (output-cached-value cv)
	   (format t ")"))

          (f
           (format t "(formula `")
           (output-value f t)
	   (output-cached-value cv)
	   (format t ")"))

          ((setf f (g-formula-value value :lambda)) ;; a compiled formula
           ;; WARNING: f had better not create a closure!
           (format t "(o-formula ~S" f)
	   (output-cached-value cv)
	   (output-meta-if-needed value)
	   (format t ")"))

          ;;; DZG - this is broken.  Commented out.
          ((setf e-formula-source NIL #+COMMENT (g-value value :source))
           (format t "(e-formula d")
           (output-value e-formula-source t)
           (format t ")"))

          (t ;; this should never happen, save the cached value
           (output-value (g-formula-value value :CACHED-VALUE) nil)))))


;;; See whether a filling-style can be printed as a call to opal:halftone.
(defun is-a-halftone (val)
  (and (is-a-p val filling-style)
       (eq (g-value val :fill-style) :opaque-stippled)
       (g-value val :stipple)
       (g-value val :stipple :percent)))

;;; Output a filling-style as a call to opal:halftone.
(defun output-halftone (val)
  (let ((foreground-color (g-value val :foreground-color))
	(background-color (g-value val :background-color)))
    (format t "(opal:halftone ~D" (g-value val :stipple :percent))
    (unless (eq foreground-color black)
      (format t " :foreground-color ")
      (output-value foreground-color nil))
    (unless (eq background-color white)
      (format t " :background-color ")
      (output-value background-color nil))
    (format t ")")))


;;; output a graphic-quality
;;;
(defun output-graphic-quality (val)
  (if (and (is-a-p val opal:font) (g-value val :standard-p))
      (format t "(opal:get-standard-font ~S ~S ~S)"
	      (g-value val :family) (g-value val :face) (g-value val :size))
      (let ((descriptor (g-value val :output-descriptor)))
	(let ((kr::*print-as-structure* nil))
	  (format t "(create-instance nil ~S" (car descriptor)))
	(indent) (indent) (indent)
	(dolist (des (cdr descriptor))
	  (let* ((slot (car des))
		 (default (cadr des))
		 (value (g-value val slot)))
	    (cond ((not (eql value default))
		   (tab)
		   (format t "(~S " slot)
		   (output-value value nil)
		   (format t ")")))))
	(format t ")")
	(outdent) (outdent) (outdent))))


;;; output a value, translate schema to schema names, special handling of
;;;    graphical qualities
;;;
(defun output-value (val back-quoted)
  (let (std-name)
    (cond ((or (numberp val) (keywordp val)
	       (eq val T) (null val) (stringp val))
	   (format t "~S" val)
	   (return-from output-value))
	  ((characterp val)
	   (format t "~@C" val)
	   (return-from output-value))
	  ((symbolp val)
	   (if (not back-quoted) (format t "`"))
	   (format t "~S" val)
	   (return-from output-value))
	  ((null val) (format t "nil")
	   (return-from output-value))
	  ((and (consp val) (eq (car val) 'QUOTE)
		(eq (length val) 2))
	   (format t "'")
	   (output-value (cadr val) back-quoted)
	   (return-from output-value))
	  ((consp val)
	   (if (not back-quoted) (format t "`"))
	   (format t "(")
	   (do ((elem val (cdr elem)))
	       ((not (consp elem)))
	     (output-value (car elem) t)
	     (format t " ")
	     (cond ((not (listp (cdr elem)))
		    (format t ". ")
		    (output-value (cdr elem) t))))
	   (format t ")")
	   (return-from output-value))
	  ((not (or (formula-p val) (schema-p val)))
	   (let ((v (format nil " ~S" val)))
 	     (if (unreadable-structure (format nil v))
	         (format t "")
	         (format t "~S" v))
	     (format *error-output* "Warning: don't know how to save ~S~%" v)
	     )
	   (return-from output-value)))

    (if back-quoted (format t ","))
					;translate into standard name
    (setf std-name (defined-name-p val))
    (cond (std-name 
	   (format t "~S" std-name))
	  ((formula-p val)
	   (output-formula val))
	  ((is-a-halftone val)
	   (output-halftone val))
	  ((is-a-p val graphic-quality)
	   (output-graphic-quality val))
	  (t
	   (let ((kr::*print-as-structure* nil))
	     (format t "~S" val);; side-effect: kr gives val a symbol name
	     (setf val (kr::schema-name val))
	     (cond ((member val *required-names*))
		   (t (push val *required-names*)
		      (format *error-output* 
			      "Warning: non-standard schema written as ~S~%"
			      val))))))))


;;; set some slots in classes:
;;;   :DO-NOT-DUMP-SLOTS  are slots that should not be dumped; these are
;;;          recomputed when the object is loaded
;;;   :LAPIDARY-ONLY-SLOTS are slots that support lapidary, but need not
;;;          be dumped for other applications
(progn
  (s-value opal:view-object :DO-NOT-DUMP-SLOTS
	   '(:PARENT :DEPENDENTS :DEPENDED-SLOTS :IS-A 
	     :WINDOW :UPDATE-SLOTS :UPDATE-SLOTS-VALUES
	     :UPDATE-INFO :IS-A-INV :INTERIM-SELECTED
	     :KNOWN-AS :CHILD
	     :GG-SELECTED :GG-INTERIM-SELECTED))
	      
  (s-value opal:aggregate :DO-NOT-DUMP-SLOTS
	   (append '(:COMPONENTS)
		   (g-value opal:view-object :DO-NOT-DUMP-SLOTS)))

  (s-value opal:aggregadget :DO-NOT-DUMP-SLOTS
	   (append '(:BEHAVIORS :PARTS :INTERACTORS)
		   (g-value opal:aggregate :DO-NOT-DUMP-SLOTS)))

  (s-value opal:aggrelist :DO-NOT-DUMP-SLOTS
	   (append '(:HEAD :TAIL :ITEM-PROTOTYPE
		     :ITEM-PROTOTYPE-OBJECT :BEHAVIORS :PARTS
		     :INTERACTORS :FORCE-COMPUTATION?)
		   (g-value opal:aggregate :DO-NOT-DUMP-SLOTS)))

  (s-value opal::window :DO-NOT-DUMP-SLOTS
	   (append '(:MAPPED :DISPLAY)
		   (g-value opal:view-object :DO-NOT-DUMP-SLOTS)))

  (s-value inter:interactor-window :DO-NOT-DUMP-SLOTS
	   (append '(:KEY-ACTORS :MOUSE-MOVED-ACTORS :BUTTON-UP-ACTORS
		     :BUTTON-DOWN-ACTORS)
		   (g-value opal::window :DO-NOT-DUMP-SLOTS)))

  (s-value opal:graphical-object :DO-NOT-DUMP-SLOTS
	   (append '(:X-TILES :X-DRAW-FUNCTION)
		   (g-value opal:view-object :DO-NOT-DUMP-SLOTS)))

  (s-value opal:text :DO-NOT-DUMP-SLOTS
	   (append '(:XFONT :HEIGHT :WIDTH :CUT-STRINGS :CUT-STRING-STRUCTS
		     :IN-KILL-MODE :CURSOR-INDEX :SAVED-CURSOR-INDEX
		     :LINE-NUMBER :LINE-HEIGHT :PREV-LEN)
		   (g-value opal:graphical-object :DO-NOT-DUMP-SLOTS)))

  (s-value opal:bitmap :DO-NOT-DUMP-SLOTS
	   (g-value opal:view-object :DO-NOT-DUMP-SLOTS))

  (s-value inter:interactor :DO-NOT-DUMP-SLOTS
                      '(:GENERATED-STOP-EVENT :KNOWN-AS
                       :DEPENDED-SLOTS :IS-A :OPERATES-ON
                       :IS-A-INV :PARENT :DRAWABLE :DISPLAY-INFO
                       :CURRENT-STATE :CURRENT-PRIORITY-LEVEL :CURRENT-OBJ-OVER
                       :COPY-OLD-WINDOW :FINAL-FEED-AVAIL :X-OFF
                       :Y-OFF :PREV-X :PREV-Y :START-CHAR
                       :FIRST-OBJ-OVER :MAIN-AGGREGATE :CURRENT-WINDOW
                       :FINAL-FEED-INUSE :REMEMBERED-LAST-OBJECT
                       :WHERE-HIT-ATTACH :ORIG-X-DIST :ORIG-Y-DIST
                       :GENERATED-RUNNING-WHERE :OBJ-BEING-CHANGED
                       :SAVED-ORIGINAL-POINTS :SAVED-ORIGINAL-ANGLE
                       :OBJ-BEING-ROTATED :CENTER-TO-USE
                       :SAVED-LAST-ANGLE :STARTX :STARTY :ORIGINAL-STRING
		       :REMEMBERED-OBJECT))

  (s-value opal:graphic-quality :DO-NOT-DUMP-SLOTS
	   '(:IS-A :IS-A-INV :OUTPUT-DESCRIPTOR :DEPENDED-SLOTS))

  (s-value opal:font :DO-NOT-DUMP-SLOTS
	   (append '(:FONT-FROM-FILE)
		   (g-value opal:graphic-quality :DO-NOT-DUMP-SLOTS)))

  (s-value opal:line-style :DO-NOT-DUMP-SLOTS
	   (g-value opal:graphic-quality :DO-NOT-DUMP-SLOTS))

  (s-value opal:filling-style :DO-NOT-DUMP-SLOTS
	   (g-value opal:graphic-quality :DO-NOT-DUMP-SLOTS))

  (s-value opal:color :DO-NOT-DUMP-SLOTS
	   (append '(:COLORMAP-INDEX :XCOLOR)
		   (g-value opal:graphic-quality :DO-NOT-DUMP-SLOTS)))

  (setf *standard-element-slots*
	'(:next :prev :prev-item :fixed-width :fixed-height))
)


(defvar *save-debug* nil)

(defun save-debug (&rest args)
  (if *save-debug* (apply #'format args)))


;;;
;;; These are definitions for managing output
;;;

(defvar *output-stack* (make-array '(10) :initial-element nil
					 :adjustable t :fill-pointer 0))
(defvar *output-state* (make-array '(10) :initial-element nil :adjustable t))
(defvar *obj-stack* (make-array '(10) :initial-element nil
				      :adjustable t :fill-pointer 0))

(defun show-stacks ()
  (save-debug t "~%   obj: ~S~%   out: " *obj-stack*)
  (dotimes (i (fill-pointer *output-stack*))
    (save-debug t "(~S ~S) " 
	    (aref *output-stack* i) (aref *output-state* i))))

;;; stack-top returns the current object, the last in the vector
;;;
(defun stack-top (vector) (aref vector (1- (fill-pointer vector))))

;;; current-state returns the state of the current output object
;;;
(defun current-state ()  (aref *output-state* (1- (fill-pointer *output-stack*))))

;;; set the output state for the current output object
;;;
(defun set-state (val) 
  (setf (aref *output-state* (1- (fill-pointer *output-stack*))) val))

;;; empty the stacks to be safe
;;;
(defun reset-stacks ()
  (setf (fill-pointer *output-stack*) 0)
  (setf (fill-pointer *obj-stack*) 0))


;;; find the depth at which the two stacks diverge:
;;;    find max i s.t. for j < i, *output-stack*[j] = *obj-stack*[j]
;;;
(defun compute-shared-depth ()
  (let ((max-depth (min (fill-pointer *output-stack*)
			(fill-pointer *obj-stack*))))
    (or (dotimes (i max-depth)
	  (cond ((not (eq (aref *output-stack* i)
			  (aref *obj-stack* i)))
		 (return  i))))
	max-depth)))


;;; manage indentation
;;;
(defvar *save-indent* 0)

(defun reset-indent () (setf *save-indent* 0))
(defun indent () (incf *save-indent*))
(defun outdent () (decf *save-indent*))
;; tab writes a newline and tabs to current indentation level
(defun tab () 
  (format t "~%")
  (dotimes (i *save-indent*) (write-string "  ")))


(defun omit-first-behavior-if-necessary ()
  ;; if first interactor of prototype is not an interactor, explicitly omit it or
  ;; else the instancing code will include all prototype interactors
  (let ((proto (car (g-local-value (stack-top *obj-stack*) :is-a)))
	proto-interactors first-proto-inter first-name interactors)
    (when (null proto)
      (format *error-output*
	      "Error: ~S has no :is-a; Write-Gadget terminated~%"
	      (stack-top *obj-stack*))
      (throw 'write-gadget-failure t))

    (setf proto-interactors (g-local-value proto :behaviors))
	
    (when proto-interactors
      (setf first-proto-inter (car proto-interactors))
      (setf first-name (g-value first-proto-inter :known-as))
      (setf interactors (g-local-value (stack-top *obj-stack*) :behaviors))
      (dolist (inter interactors)
	(cond ((eq first-name (g-value inter :known-as))
	       (return-from omit-first-behavior-if-necessary))))
      (prepare-to-write-slot)
      (output-interactors)
      (tab)
      (format t "(~S :omit)" first-name)
      ;; if there are no interactors, then output-interactors just
      ;; pushed nil onto the stack, clean it up now:
      (when (null (stack-top *output-stack*))
	(vector-pop *output-stack*)
	(format t "))")
	(set-state :class)
	(outdent)))))


(defun omit-first-component-if-necessary ()
  ;; if first part of prototype is not a component of the current
  ;; aggregate, explicitly omit it by writing :omit or
  ;; else the instancing code will include it and all prototype components
  (let ((proto (car (g-local-value (stack-top *obj-stack*) :is-a)))
	proto-components first-proto-comp first-name components)

    (when (null proto)
      (format *error-output*
	      "Error: ~S has no :is-a; Write-Gadget terminated~%"
	      (stack-top *obj-stack*))
      (throw 'write-gadget-failure t))
	   
    (setf proto-components (g-local-value proto :components))

    (when proto-components
      (setf first-proto-comp (car proto-components))
      (setf first-name (g-value first-proto-comp :known-as))
      (setf components (g-local-value (stack-top *obj-stack*) :components))
      (dolist (comp components)
	(if (eq first-name (g-value comp :known-as))
	    (return-from omit-first-component-if-necessary)))
      (prepare-to-write-slot)
      (output-parts)
      (tab)
      (format t "(~S :omit)" first-name)
      ;; if there are no components, then output-parts just
      ;; pushed nil onto the stack, clean it up now:
      (when (null (stack-top *output-stack*))
	(vector-pop *output-stack*)
	(set-state :class)
	(format t "))")
	(outdent)))))


;;; finish outputing the current object so we can move on
;;;
;;; NOTE: if this object is not at the end of a components or interactors
;;;   list, then the next component must be pushed immediately
;;;
(defun output-finished ()
  (let ((obj (stack-top *output-stack*))
	(state (current-state)))
    (case state
      ((nil) ;; just output the name and we're done
       (tab)
       (format t "~S" (g-value obj :known-as)))
      (:name
       (format *error-output* "output-finish not expecting :name state ~A~%"
	       state))
      (:class
       (write-char '#\) )
       (save-debug t "finished from :class")
       (show-stacks)
       (outdent))
      (:item-prototype
       (write-string "))")
       (save-debug t "finished from :item-prototype")
       (outdent)
       (outdent))
      (:parts
       (write-string ")))")
       (save-debug t "finished from :parts")
       (outdent)
       (outdent))
      (:interactors
       (write-string ")))")
       (save-debug t "finished from :interactors")
       (outdent)
       (outdent)))
    (vector-pop *output-stack*)))
	

;;; make sure (:interactors `( has been written
;;;
(defun output-interactors ()
  (output-name)
  (let ((state (current-state)))
    (when (eq (current-state) :name)
      (format t ":modify")
      (indent)
      (setf state :class))
    (cond ((eq state :parts)
	   (format t "))")
	   (outdent)
	   (setf state :class))
	  ((eq state :item-prototype)
	   (format t ")")
	   (outdent)
	   (setf state :class)))
    (cond ((eq state :class)
	   (tab)
	   (if (eql 1 (fill-pointer *output-stack*))
	       (format t "(:interactors `(")
	       (format t "(:interactors ("))
	   (set-state :interactors)
	   (indent)
	   (vector-push-extend (first-interactor) *output-stack*)
	   (set-state nil)
	   (show-stacks))
	  (t
	   ;;; should have been in state :class
	   (error "internal inconsistency detected")))))


;;; make sure "(:item-prototype `" has been written
;;;
(defun output-item-prototype (item-prototype)
  (output-name)
  (when (eq (current-state) :name)
    (format t ":modify")
    (indent)
    (set-state :class))
  (cond ((eq (current-state) :class)
	 (tab)
	 (if (eql 1 (fill-pointer *output-stack*))
	     (format t "(:item-prototype `")
	     (format t "(:item-prototype "))
	 (set-state :item-prototype)
	 (indent)
	 (vector-push-extend item-prototype *output-stack*)
	 (set-state nil)
	 (show-stacks))
	(t
	 ;; should have been state :class
	 (error "internal inconsistency while printing item-prototype"))))


;;; make sure the name for the current object has been output
;;;
(defun output-name ()
  (let ((obj (stack-top *output-stack*)))
    (when (null (current-state))
      (tab)
      ;; do not output a name for :item-prototype's:
      (if (eq :item-prototype
	      (aref *output-state* (- (fill-pointer *output-stack*) 2)))
	  (format t "(")
	  (format t "(~S "
		  (or (g-value obj :known-as)
		      (let ((parent (g-value obj :parent)))
			(if parent
			    (position obj (g-value parent :components))
			    (let ((op-on (g-value obj :operates-on)))
			      (if op-on
				  (position obj (g-value op-on :behaviors)))))))))
      (set-state :name))))


;;; precondition: comp is next component or interactor after top of *output-stack*
;;;
(defun output-next-comp (comp)
  (output-finished)
  (vector-push-extend comp *output-stack*)
  (set-state nil))


;;; make sure the :parts slot has been opened for the current output object
;;;
(defun output-parts ()
  (save-debug t "~%output-parts:") (show-stacks)
  (output-name)
  (when (eq (current-state) :name)
    (format t ":modify")
    (indent)
    (set-state :class))
  (cond ((eq (current-state) :class)
	 (tab)
	 (if (eql 1 (fill-pointer *output-stack*))
	     (format t "(:parts `(")
	     (format t "(:parts ("))
	 (set-state :parts)
	 (indent)
	 (vector-push-extend (first-component) *output-stack*)
	 (set-state nil))
	(t
	 ;; should have been in state :class
	 (error "internal inconsistency while printing parts"))))


;;; bring the *output-stack* up to *obj-stack*
;;; when we return, we will be positioned to write the class of
;;; the current element of *obj-stack*
;;;
(defun prepare-to-write-part ()
  (let (shared-depth)
    (save-debug t "~%prepare-to-write-part:")
    (show-stacks)
    (when (not (eq (stack-top *obj-stack*) (stack-top *output-stack*)))
      (setf shared-depth (compute-shared-depth))
      ;; if output is deeper than shared-depth, close parens and move up
      ;;    to beneath shared parent
      (reduce-depth-to shared-depth)
      ;; if obj-stack is deeper than output-stack, move down to it
      (increase-depth-to-obj-stack))
    (save-debug t "~%after prepare-to-write-part ")
    (show-stacks)))



(defun prepare-to-write-class ()
  (prepare-to-write-part)
  (output-name))


(defun prepare-to-write-slot ()
  (prepare-to-write-class)
  (when (eq (current-state) :name)
    (format t ":modify")
    (indent)
    (set-state :class)))


;;; precondition: *output-stack* is a prefix of *obj-stack* except for
;;;    possibly the last element of *output-stack*
;;; postcondition: we are positioned to write the top of *obj-stack*
;;;
(defun increase-depth-to-obj-stack ()
  (save-debug t " increase-depth:")
  (show-stacks)
  (if (not (eq (stack-top *output-stack*)
	       (aref *obj-stack* (1- (fill-pointer *output-stack*)))))
      (move-laterally)) ;;output any siblings that come before *obj-stack*
  
  (if (> (fill-pointer *obj-stack*) (fill-pointer *output-stack*))
      (move-down-toward-obj-stack)))


;;; get the first interactor of the object on top of *output-stack*
;;;
(defun first-interactor ()
  (car (g-local-value (stack-top *output-stack*) :behaviors)))


;;; get the first component of the object on top of *output-stack*
;;;
(defun first-component ()
  (car (g-local-value (stack-top *output-stack*) :components)))


;;; precondition: *output-stack* is a proper prefix of *obj-stack*,
;;;               and *output-stack* is not empty
;;;
(defun move-down-toward-obj-stack ()
  (let ((target (aref *obj-stack* (fill-pointer *output-stack*)))
	(parent (aref *obj-stack* (1- (fill-pointer *output-stack*))))
	item-prototype)
    (save-debug t "~%move-down: target is ~S" target)
    (cond ((is-a-p target inter:interactor)
	   (output-interactors))
	  ((and (is-a-p parent aggrelist)
		(not (g-value parent :dump-children-as-parts))
		(setf item-prototype (g-value parent :item-prototype-object)))
	   (output-item-prototype item-prototype))
	  (t
	   (output-parts)))
    (increase-depth-to-obj-stack)))


;;; output part names between the *output-stack* and the *obj-stack*
;;; postcondition: the object just before the *obj-stack* object at the
;;;    same level as *output-stack* has been written, and 
;;;    *output-stack* is a prefix of *output-stack*
;;;
(defun move-laterally ()
  (save-debug t "~%move-laterally ")
  (show-stacks)
  (if (is-a-p (stack-top *output-stack*) inter:interactor)
      (move-laterally-from-interactor)
      (move-laterally-from-component)))


(defun move-laterally-from-component ()
  (let ((components (g-local-value 
		     (aref *output-stack* (- (fill-pointer *output-stack*) 2))
		     :components))
	target)
    ;; first go laterally in tree to branch that leads to obj:
    (setf target (aref *obj-stack* (1- (fill-pointer *output-stack*))))
    (save-debug t "~%move-laterally-from-component target is ~S" target)
    (do ((comp-list 
	  (cdr (member (stack-top *output-stack*) components))
	  (cdr comp-list)))
	((or (eq (stack-top *output-stack*) target)
	     (null comp-list)))
      (save-debug t "~%moving laterally, comp-list is ~S" comp-list)
      (output-next-comp (car comp-list)))
    (when (not (eq (stack-top *output-stack*) target))
      (if (not (is-a-p target inter:interactor))
	  (error "expected target to be an interactor"))
      (output-finished)
      (output-interactors)
      (move-laterally-from-interactor))
    (save-debug t "~%done with move-lat-from-comp")
    (show-stacks)))


(defun move-laterally-from-interactor ()
  (let ((behaviors (g-local-value 
		    (aref *output-stack* (- (fill-pointer *output-stack*) 2))
		    :behaviors))
	target)
    ;; first go laterally in tree to branch that leads to obj:
    (setf target (aref *obj-stack* (1- (fill-pointer *output-stack*))))
    (save-debug t "move-laterally-from-interactor target is ~S" target)
    (do ((inter-list 
	  (cdr (member (stack-top *output-stack*) behaviors))
	  (cdr inter-list)))
	((or (eq (stack-top *output-stack*) target)
	     (null inter-list)))
      (save-debug t "~%moving laterally, inter-list is ~S" inter-list)
      (output-next-comp (car inter-list)))
    (save-debug t "~%done with move-lat-from-inter")
    (show-stacks)))


;;; post-condition: the *output-stack* has at most depth+1 elements
;;;
(defun reduce-depth-to (depth)
  (when (> (fill-pointer *output-stack*) (1+ depth))
    (let ((state (aref *output-state* (- (fill-pointer *output-stack*) 2))))
      (case state
	(:parts (finish-parts))
	(:interactors (finish-interactors))))
    (output-finished)
    (reduce-depth-to depth)))


;;; output the rest of the components at this level
;;
(defun finish-parts ()
  (let ((components
	 (g-local-value 
	  (aref *output-stack* (- (fill-pointer *output-stack* ) 2))
	  :components)))
    (setf components (member (stack-top *output-stack*) components))
    (dolist (comp (cdr components))
      (output-next-comp comp))))


;;; output the rest of the interactors at this level
;;
(defun finish-interactors ()
  (let ((interactors
	 (g-local-value 
	  (aref *output-stack* (- (fill-pointer *output-stack* ) 2))
	  :behaviors)))
    (setf interactors (member (stack-top *output-stack*) interactors))
    (dolist (inter (cdr interactors))
      (output-next-comp inter))))


;;; see if component lists are similar
;;;
(defun different-order (list1 list2)
  (dolist (item list1)
    (if (null list2) (return t))
    (if (not (eq (g-value item :known-as)
		 (g-value (car list2) :known-as)))
	(return t))
    (setf list2 (cdr list2))))


;;; has-normal-name -- try to determine if schema has a user-given name
;;;
;;;    assume user-given unless name is in the kr-debug package or is a number
;;;
(defun has-normal-name (schema)
  (let ((name (kr::schema-name schema)))
    (not (or (numberp name) (eq (symbol-package name) *kr-debug-package*)))))


;;; determine if a formula was created using :inherit keyword in parts list
;;;   This code must remain consistent with the implementation of
;;;   Inherit-Values in aggregadgets.lisp, where inherited formulas are
;;;   created.
;;;
(defun is-an-inherit-formula (slot value normal-proto)
  (and (formula-p value)
       (let ((formula-is-a (g-formula-value value :is-a)))
	 (and formula-is-a
	      (or (eq formula-is-a *inherit-formula*)
		  (and normal-proto
		       (eq formula-is-a
			   (get-value normal-proto slot))))))))


;;; Print type declarations for an object to standard output.  Only type
;;; declarations that differ from the parent's are printed.
;;;
(defun write-types (object)
  (let ((types nil)
	(parent (car (g-value object :is-a)))
	(do-not-dump (kr::g-value-no-copy object :do-not-dump-slots)))
    (kr::iterate-slot-value (object nil T nil)
      kr::value				; suppress warning
      (let ((slot kr::slot))
	(if (not (member slot do-not-dump))
	  (let ((type (g-type object slot)))
	    ;; Check whether type is different.
	    (if (and type
		     (or (not parent)
			 (not (eq type (g-type parent slot)))))
	      (let ((entry (assoc type types)))
		(if (null entry)
		  (push (setf entry (list type)) types))
		(push slot (cdr entry))))))))
    (if types
      (format t "~%  :declare (:type~{ ~S~})" types))))


;;; print an aggregadget to standard output
;;; 
(defun print-gadget (agget)
  (let ((components (g-local-value agget :components))
	(behaviors (g-local-value agget :behaviors))
	agget-name)
    (catch 'write-gadget-failure
      (cond ((has-normal-name agget)
	     (setf agget-name (kr::schema-name agget))
	     (push agget-name *defined-names*))
	    (t
	     (setf agget-name nil)))
      (reset-indent)
      (reset-stacks)
      (vector-push-extend agget *obj-stack*)
      (vector-push-extend agget *output-stack*)
      (set-state :class)

      (save-debug t "*output-stack* ~S at save-agg~%" *output-stack*)
      (let ((kr::*print-as-structure* nil))
	(if agget-name
	    (format t "(create-instance '~S " agget-name)
	    (format t "(create-instance NIL ")))
      (indent)
      (let ((proto (car (g-local-value agget :is-a))))
	(output-value proto nil))

      (if kr::*types-enabled*
	(write-types agget))
      (write-slots agget nil components behaviors nil)
      
      (reduce-depth-to 0)
      (output-finished)
      (save-debug t "~%pop in print-aggregate")
      (vector-pop *obj-stack*))))



;; on entry, agget is the object to be saved,
;;           top of *obj-stack* is the parent of agget
;; first-flag is set if agget is the first component
;; item-proto? is set if we are writing out an item-prototype
;; 	     rather than a "real" component (item-prototypes
;;	     have different default prototypes)
;;
(defun print-component (agget first-flag item-proto? suppress-children?)
  (let* ((components (g-local-value agget :components))
	 (behaviors (g-local-value agget :behaviors))
	 (known-as (if item-proto? nil (g-value agget :known-as)))
	 (parent (stack-top *obj-stack*))
	 (parent-proto (car (g-local-value parent :is-a)))
	 ;; the corresponding component in the parent's prototype
	 (normal-proto (if parent-proto
			   (if item-proto? 
			       (g-local-value parent-proto
					      :item-prototype-object)
			       (g-local-value parent-proto known-as))))
	 (proto (if (and (is-a-p parent aggrelist)
			 (g-value parent :dump-children-as-parts))
		    :modify
		    (car (g-local-value agget :is-a)))))
    (vector-push-extend agget *obj-stack*)

    (unless (eq agget normal-proto) ; item-prototype is inherited
      (unless (or (eq proto normal-proto)  ; part will be created by
		                           ; aggregadgets
		  ;; TEMPORARY -- When write-gadget can write out functions,
		  ;; this clause of the 'or' should be replaced by the code
		  ;; below.
		  (let ((sc (g-value agget :*special-creator*)))
		    (if sc
			(if normal-proto
			    (if (eq sc (g-value normal-proto
						:*special-creator*))
				; don't write inherited special-creator
				T
				; local sc overrode prototype's sc, so write it
				(progn (warn "Can't write part-function ~S" sc) NIL))
			    ; no prototype, so write new special creator
			    (progn (warn "Can't write part-function ~S" sc) NIL)))))
		  
	;; This code is supposed to help you write out part-generating
	;; functions, but as of 7/8/92 opal:write-gadget will not write out
	;; functions.  It should be merged into the 'or' above.
;		  (let ((sc (g-value agget :*special-creator*)))
;		    (if sc
;			(if normal-proto
;			    (if (eq sc (g-value normal-proto
;						:*special-creator*))
;				; don't write inherited special-creator
;				T
;				; local sc overrode prototype's sc, so write it
;				(not (setf proto sc)))
;			    ; no prototype, so write new special creator
;			    (not (setf proto sc)))))
			    
	(prepare-to-write-class)
	(output-value proto t)
	(set-state :class)
	(indent))

      (write-slots agget normal-proto components behaviors
		   suppress-children?))

    ;; on the first component, check to see if instance component order
    ;; matches the order in the prototype.  If not, then make sure at
    ;; least the names of all components get written.  This is the only
    ;; time this algorithm has to look ahead in the structure.
    (if first-flag 
	(let* ((parent-comps (g-local-value parent :components))
	       (proto-comps (g-local-value parent-proto :components)))
	  (cond ((different-order parent-comps proto-comps)
		 ;; this opens the :part spec, insuring that at least
		 ;; all names will be written out
		 (prepare-to-write-part)))))

    (vector-pop *obj-stack*)
    ))


(defun print-behavior (inter first-flag suppress-children?)
  (let* ((known-as (g-value inter :known-as))
	 (parent (stack-top *obj-stack*))
	 (parent-proto (car (g-local-value (stack-top *obj-stack*) :is-a)))
	 ;; the corresponding interactor in the parent's prototype
	 (normal-proto (g-local-value parent-proto known-as))
	 (proto (car (g-local-value inter :is-a))))
    (vector-push-extend inter *obj-stack*)
    (when (not (eq proto normal-proto))
      (prepare-to-write-class)
      (output-value proto t)
      (set-state :interactor-class)
      (indent))

    (write-slots inter normal-proto nil nil suppress-children?)

    (if first-flag 
	(let* ((parent-inters (g-local-value parent :behaviors))
	       (proto-inters (g-local-value parent-proto :behaviors)))
	  (cond ((different-order parent-inters proto-inters)
		 ;; this opens the :interactors spec, insuring that at least
		 ;; all names will be written out
		 (prepare-to-write-part)))))

    ;;; Next three lines added to fix bug that occured
    ;;; when printing interactors of top-level aggregadget
    (when (eq (current-state) :interactor-class)
      (format T ")")
      (outdent))

    (vector-pop *obj-stack*)
    ))


;;; see if slot would be created automatically by create-instance
;;;
(defun slot-has-an-inherited-formula (slot inst-formula proto)
  (let (proto-formula)
    (and (formula-p inst-formula)
	 (setf proto-formula (g-formula-value inst-formula :is-a))
	 (eq proto-formula (get-local-value proto slot)))))


;;; see if slot would be created automatically by create-instance
;;;
(defun slot-has-an-inherited-part (agget slot proto)
  (let ((inst-part (get-local-value agget slot)))
    (if (or (formula-p inst-part) (schema-p inst-part))
	(let ((proto-part (car (g-local-value inst-part :is-a))))
	  (eq proto-part (get-local-value proto slot))))))


;;; write out slot values, :parts slot, and :interactors slot for
;;; aggregadget
;;;
;;; Note: components and behaviors are parameters because when
;;;  saving interactors, there are no components or behaviors,
;;;  thus we save some lookups.
;;;  Similarly, other parameters are available at some calling
;;;  sites, so they are passed in.
;;;
(defun standard-write-slots (agget normal-proto components behaviors
				   suppress-children?)
  (let ((proto (car (g-local-value agget :is-a)))
	(writing-inherited-slots nil)
	values value item-prototype
	;; dzg - do not copy down this slot into agget
	(standard-slots (kr::g-value-no-copy agget :do-not-dump-slots))
	(do-not-dump-objects (kr::g-value-no-copy agget :do-not-dump-objects)))
    (when *verbose-write-gadget*
      (dotimes (i (1- (fill-pointer *obj-stack*)))
	(format *error-output* "  "))
      (format *error-output* "Writing ~S~%" agget))

    (doslots (slot agget)
	           ;;; don't write out automatically generated slots
	     (cond ((member slot standard-slots))
		   ;;; don't write out the function that created the part
		   ;;; Note:  this is not in DO-NOT-DUMP-SLOTS because we DO
		   ;;; want this slot to be copied by copy-gadget!
		   ((or (eq slot :*special-creator*)
			(eq slot :old-items)))
		   ;;; don't write out aggrelist element slots (:left and
		   ;;; :top are considered in another clause below)
		   ((member slot *standard-element-slots*))
		   ;;; don't write out parts or behaviors (yet):
		   ((progn
		      (setf values (get-local-value agget slot))
		      (setf value (if (consp values) (car values) values))
		      (and values value (or (formula-p value) (schema-p value))
			   (or (member value components)
			       (member value behaviors))))
		    (if (not (eq slot (g-value value :known-as)))
			(format *error-output*
				"~S ~S of ~S = ~S ~S ~S ~S ~S~%"
				"Warning: slot" slot agget value 
				"not saved because :known-as of" value
				"is not" slot)))
		   ;;; don't write out inherited formulas:
		   ((slot-has-an-inherited-formula slot value proto))
		   ;;; test to see if this is an :inherit formula:
		   ((is-an-inherit-formula slot value normal-proto)
		    (cond ((null writing-inherited-slots)
			   (prepare-to-write-slot)
			   (tab)
			   (format t ":inherit (")
			   (setf writing-inherited-slots t))
			  (t
			   (format t " ")))
		    (format t "~S" slot))
		   ;; Don't write out the fact that :parent is constant
		   ((and (eq slot :constant)
			 (or (eq values :parent)
			     (if (consp values)
				 (null (setf values
					     (remove :parent values)))))))
		   ;;; write out anything that's left:
		   (t
		    (prepare-to-write-slot)
		    (if writing-inherited-slots (format t ")"))
		    (setf writing-inherited-slots nil)
		    (tab)
		    (format t "(~S " slot)
		    (output-value values (> (fill-pointer *output-stack*) 1))
		    (format t ")"))))

    (if writing-inherited-slots (format t ")"))
    (setf writing-inherited-slots nil)

    ;; look at do-not-dump-objects to decide when to stop writing
    (if (or suppress-children? (eq do-not-dump-objects :me))
	(return-from standard-write-slots))

    (setf suppress-children? (eq do-not-dump-objects :children))
	   
    ;; save :parts for aggregadgets and "normal" aggrelists
    (cond ((or (is-a-p agget aggregadget)
	       (and proto		;; dzg - used for meta-objects
		    (if (g-value agget :item-prototype-object)
			(g-value agget :dump-children-as-parts)
			T)))
	   ;; if first component of the prototype is not a member
	   ;; of the aggregadget, then explicitly :omit it, 
	   ;; otherwise all prototype components will automatically
	   ;; be included by instantiation method
	   (omit-first-component-if-necessary)
	   (dolist (comp components)
	     (print-component comp (eq comp (car components)) NIL
			      suppress-children?)))
	  ((and (is-a-p agget aggrelist)
		;; no parts, just write out :item-prototype
		(setf item-prototype 
		      (g-local-value agget :item-prototype-object)))
	   (let ((std-name (defined-name-p item-prototype)))
	     (cond (std-name
		    ;; if :item-prototype is a standard object, then 
		    ;; just write the reference to the object  
		    (prepare-to-write-slot)
		    (tab)
		    (format t "(:item-prototype ")
		    (if (> (fill-pointer *output-stack*) 1)
			(format t ","))
		    (format t "~S)" std-name))
		   (t 	;; otherwise, write out using part-like
		        ;; syntax
		    (print-component item-prototype nil t
				     suppress-children?))))))
    (if proto				;; dzg - added for meta-objects
	(omit-first-behavior-if-necessary))
    (dolist (inter behaviors)
      (print-behavior inter (eq inter (car behaviors))
		      suppress-children?))))

(s-value opal:view-object :write-slots #'standard-write-slots)
(s-value inter:interactor :write-slots #'standard-write-slots)

