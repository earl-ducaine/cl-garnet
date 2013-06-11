;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple CLOS class browser using Garnet, by Jose E. Hernandez, LLNL ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load garnet-aggregraphs-loader)
  (load (merge-pathnames "motif-scrolling-window-loader"
			 Garnet-Gadgets-PathName)))

(defparameter *clos-class-browser-max-width* 900) ;max width for the window
(defparameter *clos-class-browser-max-height* 700) ;max height for the window


;;; Make a graph of the subclasses or superclasses of a CLOS object.
;;; The argument can be either a CLOS object or the symbol naming a CLOS
;;; class.  The keyword direction takes the values :down or :up, and
;;; controls wether the subclasses (default) or superclasses will be shown.
;;; The keyword depth controls the depth of the graph. If nil (default),
;;; all the graph nodes will be shown.  The size of the window is set
;;; automatically by the size of the graph when the width and height are
;;; not specified.  If the graph is too large, then the size is contolled
;;; by the above parameters.   An interactor allows the user to click on a
;;; class node to instantiate a new graph starting with this class.  The
;;; left button displays the superclasses  and the right button displays
;;; the subclasses.  The middle button will list all the direct methods of
;;; that class.  The function DESTROY-BROWSER-WINDOWS will destroy all the
;;; current window objects.
;;; Example:
;;; (browse-clos-class 'stream)
;;; (browse-clos-class (make-instance 'stream))
;;; (destroy-browser-windows)
;;; NOTE: This code make use of the following MOP symbols:
;;;       clos::standard-object, clos::class-direct-superclasses,
;;;       clos::class-direct-subclasses, clos::class-direct-methods

(Let ((-clos-class-browser-window-objects- ()))
      
      (defun browse-clos-class (arg &key ((:depth max-depth) nil)
					 (message t) (left 100) (top 100)
					 (direction :down) width height)
	(declare (special *clos-class-browser-max-width*
			  *clos-class-browser-max-height*))
	"make a graph of the subclasses or superclasses a CLOS class"
	
	(let ((class-object (cond
			     ((symbolp arg) (find-class arg))
			     ((subtypep (type-of arg) 'standard-object)
			      (class-of arg))
			     (t (error "Illegal object."))))
	      (window (gensym)))
	  
	  ;; create the window to hold the graph
	  (kr:create-instance window garnet-gadgets:motif-scrolling-window-with-bars
	      (:left left) (:top top) (:visible nil)
	      (:width (if width width
			(kr:o-formula
			 (max 100
			      (min *clos-class-browser-max-width*
				   (+ (kr:gvl :inner-aggregate :width)
				      30))))))
	      (:height (if height height
			 (kr:o-formula
			  (max 100
			       (min *clos-class-browser-max-height*
				    (+ (kr:gvl :inner-aggregate :height)
				       30))))))
	      (:total-width (kr:o-formula (kr:gvl :inner-aggregate :width)
					  200))
	      (:total-height (kr:o-formula (kr:gvl :inner-aggregate :height)
					   200))
	      (:title (format nil "~a Hierarchy for: ~s"
			      (case direction
				(:down "Subclass")
				(t "Superclass"))
			      (class-name class-object))))
	  (when message
	    (format t "~%Generating Graph for Class ~s, Please Wait!"
		    (class-name class-object)))
	  (opal:update (eval window))
	  
	  (let ((childrens-of
		 (case direction
		   (:up #'sb-mop:class-direct-superclasses)
		   (:down #'sb-mop:class-direct-subclasses)
		   (t (error "Illegal keyword for direction, must be :up or :down")))))
	    
	    ;; create the graph
	    (opal:add-components (kr:g-value (eval window) :inner-aggregate)
		  (kr:create-instance nil opal:aggregraph
		      (:children-function #'(lambda (class depth)
					      (if max-depth
						  (when (< depth max-depth)
						    (funcall childrens-of class))
						(funcall childrens-of class))))
		      (:info-function #'(lambda (class)
					  (string (class-name class))))
		      (:source-roots `(,class-object))
		      (:node-prototype
		       (kr:create-instance nil opal:aggregraph-node-prototype
			   (:interim-selected nil)
			   (:parts `((:box :modify
					   (:filling-style
					    ,(kr:o-formula
					      (if (kr:gvl :parent
							  :interim-selected)
						  opal:black-fill
						opal:white-fill)))
					   (:draw-function :xor)
					   (:fast-redraw-p t))
				     :text-al))))
		      (:interactors
		       ;; start a new graph with subclasses for selected graph
		       `((:right-press ,inter:menu-interactor
			     (:start-event :any-rightdown)
			     (:window ,(kr:o-formula (kr:gv-local :self
								  :operates-on
								  :window)))
			     (:start-where ,(kr:o-formula
					     (list :element-of
						   (kr:gvl :operates-on :nodes)
						   )))
			     (:final-function
			      ,#'(lambda (inter node)
				   (declare (ignore inter))
				   (push
				    (browse-clos-class
				     (class-name (kr:g-value node :source-node))
				     :direction :down
				     :depth max-depth
				     :message nil)
				    -clos-class-browser-window-objects-))))
			 
			 ;; start a new graph with superclasses
			 (:left-press ,inter:menu-interactor
			     (:start-event :any-leftdown)
			     (:window ,(kr:o-formula (kr:gv-local :self
								  :operates-on
								  :window)))
			     (:start-where ,(kr:o-formula
					     (list :element-of
						   (kr:gvl :operates-on :nodes)
						   )))
			     (:final-function
			      ,#'(lambda (inter node)
				   (declare (ignore inter))
				   (push
				    (browse-clos-class
				     (class-name (kr:g-value node :source-node))
				     :direction :up
				     :depth max-depth
				     :message nil)
				    -clos-class-browser-window-objects-))))
			 
			 ;; print direct methods of the selected class
			 (:middle-press ,inter:menu-interactor
			     (:start-event :any-middledown)
			     (:window ,(kr:o-formula (kr:gv-local :self
								  :operates-on
								  :window)))
			     (:start-where ,(kr:o-formula
					     (list :element-of
						   (kr:gvl :operates-on :nodes)
						   )))
			     (:final-function
			      ,#'(lambda (inter node)
				   (declare (ignore inter))
				   (mapc #'print
					 (sb-mop:specializer-direct-methods
					  (kr:g-value node :source-node)))
				   (format t "~%")))))))))
	  
	  (kr:s-value (eval window) :visible t)
	  (opal:update (eval window))

	  ; save the window object created by the browser in a global list
	  (push (eval window) -clos-class-browser-window-objects-)

	  ; return the window object
	  (eval window)))
      
      
      (defun destroy-browser-windows ()
	"destroy current browser windows"
	(dolist (object -clos-class-browser-window-objects-)
	  (opal:destroy object))
	(setq -clos-class-browser-window-objects- nil))
      
      ) ; Let

