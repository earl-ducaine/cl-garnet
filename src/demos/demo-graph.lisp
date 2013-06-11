;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-GRAPH; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;

;;;  DEMO-GRAPH
;;
;;  Designed and written by Andrew Mickish and Bryan Loyall


;;; CHANGE LOG:
;;   05/29/94 Geier/Mickish - Fixed main window position; load to garnet-load
;;              in demo-graph-init;  Changed color of nodes instead of XOR
;;   06/01/93 Andrew Mickish - Removed Verify-Binding and called Careful-Eval
;;              instead;  fixed problem with setting new root caused by
;;              kr-send being an unsafe macro; made error-gadget window be
;;              top-level
;;   03/25/92 Andrew Mickish - Get-Values ---> G-Value
;;   03/14/91 Andrew Mickish - Created


(in-package :DEMO-GRAPH)

;; Load necessary files
;;
(defvar DEMO-GRAPH-INIT
  (progn
    (load common-lisp-user::Garnet-Aggregraphs-Loader)
    (dolist (file '("text-buttons-loader" "scrolling-labeled-box-loader"
		    "error-gadget-loader"))
   (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file)))))


(declaim (special DEMO-GRAPH-WIN DEMO-GRAPH-TOP-AGG SCHEMA-GRAPH
		  DEMO-GRAPH-ERROR-GADGET RELAYOUT ROOT-BOX))

(defvar *bold-font* (create-instance NIL opal:font (:face :bold)))


;; Called by the graph interactor to delete a node and all its children.
;;
(defun RECURSIVE-NODE-DELETE (graph node)
  (let* ((links (g-value node :links-from-me))
	 (child-nodes (mapcar #'(lambda (link)
				  (g-value link :to))
			      links)))
    ;; Recursively delete children
    (mapcar #'(lambda (child)
		(recursive-node-delete graph child))
	    child-nodes)
    ;; Now delete current node
    (kr-send graph :delete-node graph node)))


;; Resets the :reachable slot of all nodes to be NIL, then sets the :reachable
;; slot of the root and all its children to be T, then deletes all nodes of the
;; graph whose :reachable slot is still NIL.
;;
(defun REMOVE-ALL-BUT-REACHABLE (graph root)
  (opal:do-components (g-value graph :nodes)
    #'(lambda (n) (s-value n :reachable NIL)))
  (MARK-REACHABLE-FROM-ROOT root)
  ; Now delete all nodes that were set
  (let ((nodes (copy-list (g-value graph :nodes :components))))
    ; Can't do do-components and a destructive operation on the :components
    ; slot simultaneously, so use dolist instead
    (dolist (n nodes)
      (unless (g-value n :reachable)
	(kr-send graph :delete-node graph n)))))


;; Sets the :reachable slot of root and all its children
;;
(defun MARK-REACHABLE-FROM-ROOT (root)
  (s-value root :reachable t)
  (dolist (l (g-value root :links-from-me))
    (let ((n (g-value l :to)))
      (unless (g-value n :reachable)
	(MARK-REACHABLE-FROM-ROOT n)))))


;;;
;;;  DO-GO
;;;

(defun DO-GO (&key dont-enter-main-event-loop double-buffered-p)

  (create-instance 'DEMO-GRAPH-WIN inter:interactor-window
     (:left 150)(:top 40)(:width 400)(:height 400)
     (:double-buffered-p double-buffered-p)
     (:title "Aggregraph Demo") (:icon-title "Demo-Graph"))
  (s-value DEMO-GRAPH-WIN
	   :aggregate
	   (create-instance 'DEMO-GRAPH-TOP-AGG opal:aggregate))
  (opal:update DEMO-GRAPH-WIN)


  (create-instance 'SCHEMA-GRAPH opal:aggregraph
     (:left 10) (:top 50)
     (:children-function #'(lambda (obj depth)
			     (if (> depth 0) NIL
				 (g-value obj :is-a-inv))))
     (:info-function #'(lambda (obj)
			 (string-capitalize
			  (kr:name-for-schema obj))))
     (:source-roots (list opal:view-object))
     (:add-back-pointer-to-nodes-function #'(lambda (source-node graph-node)
					      (s-value source-node :graph-node
						       graph-node)))
     (:node-prototype 
      (create-instance NIL opal:aggregraph-node-prototype
	 (:has-children-not-in-graph-p
	  (o-formula
	   (let* ((graph (gvl :parent))
		  (source-node (gvl :source-node))
		  ; Source-Children should be computed using kr-send, but this
		  ; doesn't seem to work:
		  ; (source-children (kr-send graph :children-function
		  ;                           source-node 0))
		  (source-children (g-value source-node :is-a-inv))
		  (graph-children (mapcar #'(lambda (link)
					      (g-value link :to))
					  (gvl :links-from-me))))
	     (gv graph :children-function)
	     (> (length source-children) (length graph-children)))))
         (:parts
	  `((:box :modify
	     (:filling-style ,(o-formula (if (gvl :parent :interim-selected)
					     opal:black-fill
					     opal:white-fill)))
             (:fast-redraw :rectangle)
             (:fast-redraw-filling-style ,opal:white-fill)
;;;	     (:draw-function :xor) (:fast-redraw-p T)
             )
	    (:text-al :modify
             (:fast-redraw :rectangle)
             (:fast-redraw-filling-style ,opal:white-fill)
	     (:line-style ,(o-formula (if (gvl :parent :interim-selected)
                                          opal:white-line
                                          opal:default-line-style)))
	     (:font ,(o-formula (if (gvl :parent :has-children-not-in-graph-p)
				    *bold-font*
				    opal:default-font))))))))
     (:interactors
      `((:press ,inter:menu-interactor
	 (:window ,(o-formula (gv-local :self :operates-on :window)))
	 (:start-where ,(o-formula (list :element-of (gvl :operates-on :nodes))))
	 (:start-event (:leftdown :middledown :rightdown))
	 (:final-function
	  ;;; Mnemonics:
	  ;;;     "Source" is a node in the original graph that we are modeling
	  ;;;   (the Garnet hierarchy) such as opal:graphical-object.
	  ;;;     "Source Children" is a list of nodes in the original graph
	  ;;;   that are children of "Source".
	  ;;;     
	  ,#'(lambda (inter node)
	       (let* ((graph (g-value node :parent :parent))
		      (source (g-value node :source-node))
		      (char (g-value inter :start-char)))
		 (case char
		   ;; Add first child that has not already been added to graph
		   (:leftdown
		    (let* ((source-children (kr-send graph :children-function
						     source 0))
			   (new-child
			    (find-if-not
			      #'(lambda (source-child)
				  (kr-send graph :source-to-graph-node
					   graph source-child))
			      source-children)))
		      (when new-child
			(kr-send graph :add-node
				 graph new-child (list node) NIL))))

		   (:middledown
		    (garnet-debug:inspector source))
		   
		   ;; Delete the current node and all its children
		   (:rightdown
		    ; Don't delete the root
		    (unless (eq source (first (g-value graph :source-roots)))
		      (RECURSIVE-NODE-DELETE graph node)))))))))))

  
  ;;; An error-gadget to tell the user when a non-schema was typed into
  ;;; the labeled box as a new root
  ;;;
  (create-instance 'DEMO-GRAPH-ERROR-GADGET garnet-gadgets:error-gadget
    (:window-left (o-formula (opal::gv-center-x-is-center-of DEMO-GRAPH-WIN)))
    (:window-top (o-formula (opal::gv-center-y-is-center-of DEMO-GRAPH-WIN)))
    )

  
  ;;; Press this button to relayout the graph
  ;;;
  (create-instance 'RELAYOUT garnet-gadgets:text-button
     (:left 10) (:top 10)
     (:string "Relayout")
     (:final-feedback-p NIL)
     (:shadow-offset 5)
     (:gray-width 3)
     (:text-offset 3)
     (:selection-function #'(lambda (gadget value)
			      (declare (ignore gadget value))
			      (kr-send SCHEMA-GRAPH :layout-graph
				       SCHEMA-GRAPH))))


  ;;; Used to set the root of the graph
  ;;; 
  (create-instance 'ROOT-BOX garnet-gadgets:scrolling-labeled-box
     (:left 100) (:top 10) (:width 250)
     (:label-string "Schema Root:")
     (:value "opal:view-object")
     (:old-value "opal:view-object")
     (:selection-function
      #'(lambda (gadget string)
	  (multiple-value-bind (value errorp)
	      (gg:careful-string-eval string DEMO-GRAPH-ERROR-GADGET
				      (concatenate 'string
						   "Unable to access schema "
						   string))
	    (if (and (not errorp) (schema-p value))
	        ;;; The user typed a valid schema, so make it the new root
		(let* ((old-root (first (g-value SCHEMA-GRAPH :source-roots)))
		       (existing-node-to-be-root
			; This will get a graph node if one exists for value
			(opal:source-to-graph-node SCHEMA-GRAPH value)))
		  ;; Put new root in root lists and remove all nodes that are
		  ;; not children of the new root.
		  (cond
		    (existing-node-to-be-root
		     (unless (eq old-root existing-node-to-be-root)
		       (opal:remove-root SCHEMA-GRAPH old-root)
		       (opal:make-root SCHEMA-GRAPH existing-node-to-be-root)
		       (REMOVE-ALL-BUT-REACHABLE SCHEMA-GRAPH
						 existing-node-to-be-root)))
		    (t
		     (REMOVE-ALL-BUT-REACHABLE SCHEMA-GRAPH old-root)
		     (kr-send schema-graph :add-node schema-graph value NIL NIL)
		     (let ((new-root (g-value value :graph-node)))
		       (opal:make-root SCHEMA-GRAPH new-root))))
		  (opal:layout-graph SCHEMA-GRAPH)
		  (s-value gadget :old-value string))
		;; The value was invalid, so restore old value
		(s-value gadget :value (g-value gadget :old-value)))
	    ))))

  
  (opal:add-components DEMO-GRAPH-TOP-AGG
		       RELAYOUT ROOT-BOX SCHEMA-GRAPH)
  (opal:update DEMO-GRAPH-WIN)
  

  (format t "~%Demo-Graph:
      This graph shows the Garnet schema hierarchy.
      Press on a node with the left mouse button to display
   another child of the node (if possible).  Nodes that can be
   expanded further appear in bold font.
      Press on a node with the middle mouse button to bring up
   the inspector on that node.
      Press on a node with the right mouse button to delete the
   node and all its children.
      Pushing the relayout button will reorganize the graph neatly.
      Typing the name of a garnet object in the labeled box will
   put that garnet object at the root.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  
  )


(defun DO-STOP ()
  (opal:destroy DEMO-GRAPH-WIN)
  (opal:destroy DEMO-GRAPH-ERROR-GADGET))

