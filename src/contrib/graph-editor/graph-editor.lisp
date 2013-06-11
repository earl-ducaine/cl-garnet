;;; graph-editor.lisp, contributed by Jim Davis <davis@DRI.cornell.edu>,
;;; is an example of an aggregraph that shows the hierarchy of your directory
;;; tree.  The nodes of the aggregraph can be moved with the mouse.  A demo is
;;; included at the end of the file: execute (ge:graph-edit-directory).


;;;; Editor for graphs.
; copyright 1992 by Xerox.

;(in-package :graph-editor :use '(:lisp :clos) :nicknames '(ge))
(defpackage "GRAPH-EDITOR"
  (:nicknames "GE")
  (:use common-lisp kr)
  (:export graph-node
	   node-highlight-interactor link-highlight-interactor
	   move-node-interactor delete-node-interactor delete-link-interactor
	   node-select-link-origin-interactor node-select-link-dest-interactor
	   graph-edit-agg 
	   define-cursor
	   menubar-menu-without-inverse graph-editor-menubar message-pane 
	   graph-editor graph-editor-graph-agg
	   ged-delete-node ged-delete-link ged-add-link
	   ged-double-click
	   graph-edit-directory))

(in-package "GE")

;;(use-package :kr)		;Garnet



;; This instruction from kr.lisp may need to be reevaluated
(set-dispatch-macro-character #\# #\f #'kr::o-formula-reader)

; This is built from aggregraphs, with some modifications.  The basic abstraction here
; is GRAPH-EDIT-AGG, which is an aggregraph that also defines a set of interactors
; on the nodes and links of the graph.  Most likely, the nodes of your graph stand
; for some data structure or objects in your application, and that application may
; have its own rules about whether an editing operation is legal (e.g. if nodes
; are typed, it might not be legal to connect nodes of differing types) and it
; may have additional operations that must be performed when an edit occurs (e.g.
; if nodes are files, then deleting a node means deleting the file).  There are
; ways to cause the graph-edit-agg to call functions to perform these tests
; and operations.

; There is also GRAPH-EDITOR, which shows a graph, also has a menubar
; of editing operations and a message pane for user interaction.  The
; GRAPH-EDIT-AGG is actually an graph-editor-graph-agg, specialized
; from GRAPH-EDIT-AGG.

; written by Jim Davis at the Design Research Institute at Cornell University Oct 1992

; To do:

; Add code to allow one to add a node.  It should go into the root list
; Maybe add scroll bars.
; Clip the graph display (within graph-editor) so that nodes outside the visible area of the
; aggregate are not drawn.  need to use a window to do this.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (and (find-symbol "AGGREGRAPH" :opal)
	       (boundp (find-symbol "AGGREGRAPH" :opal))
	       (kr:schema-p (symbol-value (find-symbol "AGGREGRAPH" :opal))))
    (load cl-user::Garnet-Aggregraphs-Loader))
  (cl-user::garnet-load "gadgets:menubar-loader"))



; This definition is copied from AGGREGRAPH-NODE-PROTOTYPE but differs
; in that:
; 1) the outline is called "outline", not "box" so that one can use
;   the move-grow-interactor with it.  m-g-i tries to set the value
;   of the :box slot, so another name is needed
; 2) The filling style depends on whether the node is :interim-selected, this
; is to provide feedback to the user when using the mouse.

(create-instance 'graph-node opal:aggregadget
   ;; necessary slots for an aggregraph node prototype.
   (:left 0)
   (:top 0)
   (:info "") ;; string to be displayed.  returned by :info-function
              ;; in aggregraph object
   (:source-node nil)
   (:links-to-me nil)
   (:links-from-me nil)

   ;; internal bookkeeping for default layout function

   (:layout-info-appears-in-top-sortp nil)
   (:layout-info-left-setp nil)
   (:layout-info-top-setp nil)

   ;; for interactors
   (:interim-selected nil)
   (:box nil)

   ;; graphics

  (:parts
   `(
     (:outline ,opal:roundtangle
	       (:filling-style ,(o-formula (if (gvl :parent :interim-selected)
					     opal:black-fill
					     opal:white-fill)))
	       (:draw-function :xor)
	       (:fast-redraw-p t)
	       (:top ,(kr:o-formula (kr:gvl :parent :top)))
	       (:left ,(kr:o-formula (kr:gvl :parent :left)))
	       (:width ,(kr:o-formula (+ (kr:gvl :parent :text-al :width) 8)))
	       (:height ,(kr:o-formula (+ (kr:gvl :parent :text-al :height) 8)))
	       (:radius 5))
     (:text-al ,opal:multi-text
	       (:left ,(kr:o-formula (+ (kr:gvl :parent :left) 4)))
	       (:top ,(kr:o-formula (+ (kr:gvl :parent :top) 4)))
	       (:string ,(kr:o-formula (kr:gvl :parent :info)))))))



;; Allows aggregate to change the cursor while running

(define-method :set-cursor opal:aggregate (self cursor &optional mask)
  (when (null mask)
    (setf mask (g-value cursor :mask)))
  (s-value (g-value self :window) :cursor (cons cursor mask))
  (opal:update (g-value self :window)))


;; Save load directory.
(defparameter *loaded-from-pathname* ".")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *loaded-from-pathname* (print *load-pathname*)))

; convenient way to define a cursor with a mask
(defun define-cursor (name path mask-path)
  (create-instance name opal:bitmap
    (:image (opal:read-image 
	     (merge-pathnames path *loaded-from-pathname*)))
    (:mask (create-instance nil opal:bitmap
	     (:image (opal:read-image
		      (merge-pathnames
		       mask-path *loaded-from-pathname*)))))))


;; define some cursors for this window

(create-instance 'move-cursor opal:arrow-cursor
  (:mask opal:arrow-cursor-mask))

(define-cursor 'delete-cursor "ged-delete-cursor.bm" "ged-delete-mask.bm")
(define-cursor 'link-cursor "ged-link-cursor.bm" "ged-link-mask.bm")
(define-cursor 'unlink-cursor "ged-unlink-cursor.bm" "ged-unlink-mask.bm")



;; Interactors on the DAG graph

; Highlights nodes and links when mouse passes over them start-event
; is T so that interactor is always running, so that interim-select is
; set whenever mouse is over object, regardless of mouse buttons.

;; This seems to be broken, it prevents other interactors from running
;; so so :ACTIVE is NIL, and I have removed it from the :interactors
;; of the aggregate, and removed all calls which might have enabled it.

(create-instance 'node-highlight-interactor inter:menu-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:active nil)
  (:start-event t)
  (:stop-event t)			;never stops
;  (:start-where (o-formula (list :element-of (gvl :operates-on :nodes))))
  (:start-where (o-formula (list :in (gvl :operates-on))))
  (:running-where (o-formula (list :in (gvl :operates-on))))
)

;; BROKEN RIGHT NOW SO :ACTIVE is NIL
(create-instance 'link-highlight-interactor inter:menu-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:active nil)
  (:start-event t)
  (:stop-event t)
  (:start-where (o-formula (list :element-of (gvl :operates-on :links)))))

(create-instance 'move-node-interactor inter:move-grow-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:active t)
  (:start-where (o-formula (list :element-of (gvl :operates-on :nodes))))
  (:line-p nil)
  (:grow-p nil)
  (:start-action
   #'(lambda (self obj points)
       ;; The position of the node was first set by the layout function, so
       ;; change the formula to make it depend upon the :BOX slot, thus
       ;; allowing the interactor to move it
       (s-value obj :left (o-formula (first (gvl :box))))
       (s-value obj :top (o-formula (second (gvl :box))))
       (call-prototype-method self obj points))))

; All the final functions follow this same pattern.
; they first call a "prohibitor" function which can check for a reason
; not to allow the operation.  If an error string is returned the message
; is printed and the operation is aborted.  Otherwise the operation is done.

(defmacro def-ged-interactor-final-function (prohibitor doit)
  `#'(lambda (inter thing)
       (let* ((graph (g-value inter :operates-on))
	      (whynot (kr-send graph ,prohibitor graph thing)))	;is it forbidden?
	 (if whynot
	   (kr-send graph :complaint graph whynot thing) ;print complaint
	   (kr-send graph ,doit graph  thing))))) ;else doit
		  
(create-instance 'delete-node-interactor inter:menu-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:start-where (o-formula (list :element-of (gvl :operates-on :nodes))))
  (:active nil)
  (:final-function
   (def-ged-interactor-final-function :delete-node-prohibit :delete-node)))

(create-instance 'node-select-link-origin-interactor inter:menu-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:start-where (o-formula (list :element-of (gvl :operates-on :nodes))))
  (:active nil)
  (:final-function
   (def-ged-interactor-final-function :link-origin-prohibit :select-link-origin)))

(create-instance 'node-select-link-dest-interactor inter:menu-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:start-where (o-formula (list :element-of (gvl :operates-on :nodes))))
  (:active nil)
  (:final-function
   (def-ged-interactor-final-function :link-dest-prohibit :select-link-dest)))

(create-instance 'delete-link-interactor inter:menu-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:start-where (o-formula (list :element-of (gvl :operates-on :links))))
  (:active nil)
  (:final-function
   (def-ged-interactor-final-function :delete-link-prohibit :delete-link)))


(create-instance 'double-click-node-interactor inter:button-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:continuous nil)
  (:start-where (o-formula (list :element-of (gvl :operates-on :nodes))))
  (:start-event :double-leftdown)
  (:final-function
   #'(lambda (inter node)
       (ged-double-click (g-value node :source-node)
			 (g-value inter :window)))))

; By default, double click does nothing but you can define this CLOS
; method to get something else to happen
(defmethod ged-double-click ((something t) opal-window)
  (declare (ignore opal-window))  )


 
; displays the DAG, has a set of interactors to edit it.
; Switches interactors on and off depending on mode.
; There is no provision in THIS object for selecting the current mode,
; that belongs to graph-editor. (see below)  Likewise no way to print complaints.

(create-instance 'graph-edit-agg opal:aggregraph
  (:read-only-p nil)
  (:mode :MOVE)
  (:link-origin nil)
  (:node-prototype graph-node)		;override opal:aggregraph's default
  (:interactors
   `(;(:node-highlight ,node-highlight-interactor)
     ;(:link-highlight ,link-highlight-interactor)
     (:node-mover ,move-node-interactor)
     (:node-deleter ,delete-node-interactor)
     (:node-select-link-origin ,node-select-link-origin-interactor)
     (:node-select-link-dest ,node-select-link-dest-interactor)
     (:link-unlinker ,delete-link-interactor)
     (:double-click-node ,double-click-node-interactor)
     ))
  )

(define-method :complaint graph-edit-agg (self string object)
  (declare (ignore object))
  (interactors:beep)
  (kr-send self :set-message self string)
  (kr-send self :restart-mode self))

; while debugging, this is helpful.  Most likely you will include
; this in a larger object anyway.
(define-method :set-message graph-edit-agg (self string)
  (declare (ignore self))
  (print string))

; When entering a mode, change the cursor, and enable the appropriate
; interactors.
(define-method :set-mode graph-edit-agg (self mode)
  (ecase mode
    (:MOVE
     (kr-send self :set-cursor self move-cursor)
     (kr-send self :enable-interactors self
	      '(:NODE-MOVER :double-click-node )))
    (:DELETE
     (kr-send self :set-cursor self delete-cursor)
     (kr-send self :enable-interactors self
	      '( :double-click-node  :NODE-DELETER)))
    (:LINK
     (kr-send self :set-link-origin self nil)
     (kr-send self :set-cursor self link-cursor)
     (kr-send self :enable-interactors self
	      '( :double-click-node  :NODE-SELECT-LINK-ORIGIN)))
    (:UNLINK
     (kr-send self :set-cursor self unlink-cursor)
     (kr-send self :enable-interactors self
	      '(:double-click-node :LINK-UNLINKER )))
    )
  (s-value self :mode mode)
  )

(define-method :set-link-origin graph-edit-agg (self node)
  (when (g-value self :link-origin)
    (s-value (g-value self :link-origin) :interim-selected nil))
  (s-value self :link-origin node)
  (when node
    (s-value node :interim-selected t)))
    
(define-method :restart-mode graph-edit-agg (self)
  ;; reset state.  This is probably only useful for the LINK mode which has two sub-states.
  (kr-send self :set-mode self (g-value self :mode)))

(define-method :enable-interactors graph-edit-agg (self enabled)
  "Enable the interactors in ENABLED, disable the rest"
  (dolist (interactor-info (g-value self :interactors))
    (let ((symbol (first interactor-info)))
      (s-value (g-value self symbol)
	       :active
	       ;; must be T or NIL and only T or NIL
	       (not (null (find symbol enabled)))))))


;; Functions called by the interactors to actually do the work
;; You will want to redefine them to add application-specific constraints
;; and/or change your application data structures.  But note also that 
;; if you want the ability to "revert" changes made in the editor, you
;; might want to defer actual changes to your data until the user
;; clicks on Save.


;; Delete NODE

(define-method :delete-node-prohibit graph-edit-agg (self node)
  (declare (ignore node))
  (cond
   ((g-value self :read-only-p)
    "Read only")
   (t 
    nil)))

(define-method :delete-node graph-edit-agg (self node)
  (when (find (g-value node :source-node) (g-value self :source-roots))
    (kr-send self :remove-graph-root self node))
  (ged-delete-node (g-value node :source-node) (g-value self :window))
  (call-prototype-method self node))

(defmethod ged-delete-node ((something t) opal-window)
  (declare (ignore opal-window))  )


;; CREATE LINK

(define-method :link-origin-prohibit graph-edit-agg (self node)
  (declare (ignore node))
  (cond
   ((g-value self :read-only-p)
    "Read only")
   (t 
    nil)))

(define-method :select-link-origin  graph-edit-agg (self node)
  (kr-send self :set-link-origin self node)
  (kr-send self :enable-interactors self '( :NODE-SELECT-LINK-DEST)))

(defun link-exists-p (origin dest)
  (find dest (g-value origin :links-from-me) :key #'(lambda (link) (g-value link :to))))

(define-method :link-dest-prohibit graph-edit-agg (self dest)
  (let ((origin (g-value self :link-origin)))
    (cond
     ((eq dest origin)
      "Cant link to self")
     ((link-exists-p dest origin)
      "No circular links!")
     ;; add test here: not exists node linked from origin and linked to dest (big circles)
     ((link-exists-p origin dest)
      "Link already exists")
     (t
      nil))))

(define-method :select-link-dest graph-edit-agg (self dest)
  (let ((origin (g-value self :link-origin)))
    (ged-add-link (g-value origin :source-node) (g-value dest :source-node)
		  (g-value self :window))
    (kr-send self :add-link self origin dest)
    (when (find (g-value dest :source-node) (g-value self :source-roots))
      (kr-send self :remove-graph-root self dest))
    (kr-send self :restart-mode self)))

(defmethod ged-add-link ((source t) (dest t) opal-window)
  (declare (ignore opal-window)))

;; Delete LINK

(define-method :delete-link-prohibit graph-edit-agg (self link)
  (declare (ignore link))
  (cond
   ((g-value self :read-only-p)
    "Read only")
   (t 
    nil)))

(define-method :delete-link graph-edit-agg (self link)
  (ged-delete-link (g-value link :from :source-node)
		   (g-value link :to :source-node)
		   (g-value self :window))
  (let ((dest (g-value link :to)))
    (call-prototype-method self link)
    (when (null (g-value dest :links-to-me)) ;has no incoming links?
      (kr-send self :add-graph-root self dest))))

(defmethod ged-delete-link (from to opal-window)
  (declare (ignore from to opal-window)))

;; Undo

(define-method :undo graph-edit-agg (self)
  (kr-send self :complaint self "Undo is not implemented yet" nil))

;; Some general utilities on the graph

(define-method :add-graph-root graph-edit-agg (self node)
  (push node (g-value self :graph-roots))
  (push (g-value node :source-node)  (g-value self :source-roots)))

(define-method :remove-graph-root graph-edit-agg (self node)
  (s-value self :graph-roots (remove node (g-value self :graph-roots)))
  (s-value self :source-roots
	   (remove (g-value node :source-node) (g-value self :source-roots))))

(define-method :map-graph-nodes graph-edit-agg (self function)
  (dolist (node (g-value self :graph-roots))
    (map-graph-nodes node function)))


(defun map-graph-nodes (node function)
  (funcall function node)
  (dolist (link (g-value node :links-from-me))
    (map-graph-nodes (g-value link :to) function)))

; Sanity check: one-to-one correspondance between graph-roots and
; source-roots.
(define-method :assert-source-and-graph-duality opal:aggregraph (self)
  (let* ((source-roots (g-value self :source-roots))
	 (graph-roots (g-value self :graph-roots))
	 (nodes (remove-if #'(lambda (node) (find (g-value node :source-node) source-roots))
			   graph-roots))
	 (things (remove-if  #'(lambda (thing)
				 (find thing graph-roots
				       :key #'(lambda (node) (g-value node :source-node))))
			     source-roots)))
    (when (not (null nodes))
      (cerror "Continue" 
	      "Nodes ~S are in GRAPH-ROOTS but the corresponding ~
               SOURCE-NODE is not in SOURCE-ROOTS"
	      nodes))
    (when (not (null things))
      (cerror "Continue"
	      "Items ~A are in SOURCE-ROOTS but have no corresponding node in GRAPH-ROOTS"
	      things))
    ))



;;; EDITOR for Graphs.  Includes menubar and message pane.

;; Menubar. The normal menubar menu shows itself in inverse video when the mouse is
; over it.  I don't link that behavior.

(create-instance 'menubar-menu-without-inverse garnet-gadgets:bar-item
  (:text-spacing 20)
  (:width #f (+ (gvl :text :width)
		(gvl :text-spacing)))
  (:parts
   `((:text :modify
      (:line-style ,opal:default-line-style))
     (:cover ,opal:rectangle
	     (:left ,(o-formula (gvl :parent :left)))
	     (:top ,(o-formula (gvl :parent :top)))
	     (:height ,(o-formula (gvl :parent :text :height)))
             (:width ,(o-formula (+ (* 2 (gvl :parent :spacing))
				    (gvl :parent :text :width))))
	     (:draw-function ,(o-formula (if (gvl :parent :enabled)
					   :xor :and)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula (if (gvl :parent :enabled)
					   opal:white-fill
					   opal:gray-fill)))))))

; menubar for the editor
(defparameter *graph-editor-menubar-items*
  `(
    ("Edit" nil
     (("Undo" ,#'(lambda (menubar menu item) 
		(declare (ignore menu item))
		(kr-send (g-value menubar :parent :graph) :UNDO
			 (g-value menubar :parent :graph))))
      ("Save" ,#'(lambda (menubar menu item) 
		(declare (ignore menu item))
		(kr-send (g-value menubar :parent)  :save (g-value menubar :parent))))
      ))
    ("Tools" nil
     (("Move" ,#'(lambda (menubar menu item) 
		(declare (ignore menu item))
		(kr-send (g-value menubar :parent)
			 :set-mode (g-value menubar :parent) :MOVE)))
      ("Link" ,#'(lambda (menubar menu item) 
		(declare (ignore menu item))
		(kr-send (g-value menubar :parent)
			 :set-mode (g-value menubar :parent) :LINK)))
      ("Unlink" ,#'(lambda (menubar menu item) 
		  (declare (ignore menu item))
		  (kr-send (g-value menubar :parent)
			   :set-mode (g-value menubar :parent) :UNLINK)))
      ("Delete node" ,#'(lambda (menubar menu item) 
		       (declare (ignore menu item))
		       (kr-send (g-value menubar :parent)
				:set-mode (g-value menubar :parent) :DELETE)))
      ))
    ("View" nil
     (("Arrange" ,#'(lambda (menubar menu item)
		   (declare (ignore menu item))
		   (kr-send (g-value menubar :parent :graph)
			    :layout-graph (g-value menubar :parent :graph))
		   (opal:update (g-value menubar :parent :window))))
      ))
    ))

(create-instance 'graph-editor-menubar garnet-gadgets:menubar
;  (:item-prototype menubar-menu-without-inverse)
  (:items *graph-editor-menubar-items*))


;; message pane

(create-instance 'message-pane opal:aggregadget
  (:parts
   `((:text ,opal:multi-text
	    (:top ,#f (gvl :parent :top))
	    (:string "")))))

(define-method :set-text message-pane (self format-string &rest format-args)
  (s-value (g-value self :text) :string
	   (apply #'format nil format-string format-args))
  ;; message will not change until an update happens
  )

(define-method :clear-text message-pane (self)
  (kr-send self :set-text self ""))

; graph-edit-agg specialized for use in graph-editor

(create-instance 'graph-editor-graph-agg graph-edit-agg
  (:set-message #'(lambda (agg string)
		    (kr-send (g-value agg :parent)
			     :set-message (g-value agg :parent) string)))
  (:source-roots #f (gvl :parent :roots))
  (:top #f (gvl :parent :graph-top))
  (:height #f(gvl :parent :graph-height))
  (:width #f (gvl :parent :width))
  (:bottom #f(+ (gvl :top) (gvl :height))))




; Editor for DAGS, with menubar, a pane for the dag, and a message pane
(create-instance 'graph-editor opal:aggregadget
  (:roots nil)
  (:width 600)
  (:menu-extra-space 50)
  (:graph-top #f (+ (gvl :menubar :bottom) (gvl :menu-extra-space)))
  (:graph-height 200)
  (:message-top #f (gvl :graph :bottom))
  (:message-height 50)
  (:parts
   `((:menubar ,graph-editor-menubar
	       (:top 0)
	       (:bottom ,#f(+ (gvl :top) (gvl :height))))
     (:divider1 ,opal:line		;separate menubar from graph
	       (:x1 0)
	       (:y1 ,#f (gvl :parent :menubar :bottom))
	       (:x2 ,#f (gvl :parent :width))
	       (:y2 ,#f (gvl :y1)))
     (:graph ,graph-editor-graph-agg)
     (:divider2 ,opal:line		;separate graph from message
	       (:x1 0)
	       (:y1 ,#f (gvl :parent :message-top))
	       (:x2 ,#f (gvl :parent :width))
	       (:y2 ,#f (gvl :y1)))
     (:message ,message-pane		;to display messages.
	       (:top ,#f (gvl :parent :message-top))
	       (:bottom ,#f (+ (gvl :top) (gvl :height)))
	       (:height ,#f (gvl :parent :message-height))
	       (:width ,#f (gvl :parent :width)))

     ))
  )

(define-method :set-mode graph-editor (self mode)
  (kr-send (g-value self :graph) :set-mode (g-value self :graph) mode))

(define-method :set-message graph-editor (self string &rest args)
  (kr-send (g-value self :message) :set-text (g-value self :message)
	   (apply #'format nil string args))
  ;; Message is not visible until an update happens
  (opal:update (g-value self :window)))





; example of usage - an editor for file systems.
; It does not really delete your files, don't worry.

;;(export '(graph-edit-directory))

(defparameter *wild-pathname* (make-pathname :name :wild :type :wild))

(defun graph-edit-directory (&optional (root (truename (pathname "./"))))
  (create-instance 'www inter:interactor-window
    (:title (format nil "Files in ~A" root))
    (:visible nil)
    (:aggregate
     (let ((agg (create-instance nil graph-editor
		  (:roots (list (pathname root)))
		  (:parts 
		   `(:menubar
		     :divider1
		     (:graph :modify
			     (:info-function
			      ,#'(lambda (pathname)
				   (format nil "~A" pathname)
				   #+comment
				   (format nil "~:[~;~:*~A~]~:[~;~:*.~A~]"
					   (pathname-name pathname)
					   (pathname-type pathname))))
			     (:children-function
			      ,#'(lambda (pathname depth)
				   (when (< depth 1)
				     (mapcar #'(lambda (p)
						 (enough-namestring p pathname))
					     (directory (make-pathname
							 :directory (pathname-directory pathname)
							 :defaults *wild-pathname*))))))
			     )
		     :divider2
		     :message)))))
;;       (setf ge agg)
       agg)))

  (s-value www :width (g-value www :aggregate :width))
  (s-value www :height (g-value www :aggregate :height))

  (opal:update www)
  ;; have to do this AFTER the update
  (opal:notice-items-changed (g-value www :aggregate :menubar))

  (progn (s-value www :visible t) (opal:update www))
  )


#| ; basement.

; dont need this - might have used it as a feedback object for the graph interactors.
(create-instance 'feedback-rect opal:rectangle
  (:obj-over nil)
  (:visible #f(gvl :obj-over))
  (:left #f(gvl :obj-over :left))
  (:top #f(gvl :obj-over :top))
  (:width #f(gvl :obj-over :width))
  (:height #f(gvl :obj-over :height))
  (:fast-redraw-p t)
  (:draw-function :xor)
  (:filling-style opal:black-fill)
  (:line-style nil))

|#
