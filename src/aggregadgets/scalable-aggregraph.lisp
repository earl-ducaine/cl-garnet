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
;;; scalable-aggregraph.lisp
;;; Requires the files "rectangle-conflict-object.lisp" and "aggregraphs.lisp".
;;;
;;; Written by A. Bryan Loyall
;;; Winter 1990-1991
;;;
;;; $Id::                                                             $


(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(SCALABLE-AGGREGRAPH SCALABLE-AGGREGRAPH-NODE-PROTOTYPE
	    SCALABLE-AGGREGRAPH-LINK-PROTOTYPE)))

;; requires the file "rectangle-conflict-object.lisp" to be loaded.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default node prototype
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; displays the provided string (possibly multiple lines) in a white
;; filled roundtangle.
;;
;; requires:
;;  - the user provided :info-function (in aggregraph object) must
;;    return a string.
(kr:create-instance 'scalable-aggregraph-node-prototype opal:aggregadget
                    
   ;; necessary slots for an aggregraph node prototype.

   (:left 0)
   (:top 0)
   (:width 150)
   (:height 25)
   (:info "") ;; string to be displayed.  returned by :info-function
              ;; in aggregraph object
   (:source-node nil)
   (:links-to-me nil)
   (:links-from-me nil)

   ;; internal bookkeeping for default layout function

   (:layout-info-appears-in-top-sortp nil)
   (:layout-info-left-setp nil)
   (:layout-info-top-setp nil)

   ;; graphics

   (:parts
     `((:box ,opal:roundtangle
          (:filling-style ,opal:white-fill)
          (:top ,(kr:o-formula (kr:gvl :parent :top)))
          (:left ,(kr:o-formula (kr:gvl :parent :left)))
          (:width ,(kr:o-formula (kr:gvl :parent :width)))
          (:height ,(kr:o-formula (kr:gvl :parent :height)))
          (:radius 5))
       (:text-al ,opal:multi-text 
          (:left ,(kr:o-formula (+ (kr:gvl :parent :left) 4)))
          (:top ,(kr:o-formula (+ (kr:gvl :parent :top) 4)))
          (:string ,(kr:o-formula (kr:gvl :parent :info)))
          (:visible ,(kr:o-formula
                      (and (< (+ (kr:gvl :width) 8) (kr:gvl :parent :width))
                           (< (+ (kr:gvl :height) 8) (kr:gvl :parent :height)))))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default link prototype
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; displays a line from the center of the right edge of the :from
;; object to the center of the left edge of the :to object (so it is
;; well suited to a left to right layout of a tree or dag.)
(kr:create-instance 'scalable-aggregraph-link-prototype opal:line
                   
   ;; necessary slots for an aggregraph node prototype.

   (:from nil)
   (:to nil)
   
   ;; graphics

   (:x1 (kr:o-formula (+ (kr:gvl :from :left) (kr:gvl :from :width))))
   (:y1 (kr:o-formula (+ (kr:gvl :from :top)
                      (floor (kr:gvl :from :height) 2))))
   (:x2 (kr:o-formula (kr:gvl :to :left)))
   (:y2 (kr:o-formula (+ (kr:gvl :to :top)
                      (floor (kr:gvl :to :height) 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helping function for
;; scalable-layout-tree function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this function returns an ordered list of the graphics nodes.  It
;; orders them in an "pre-order traversal" manner from the roots:
;;   - for a tree it will return exactly the pre-order traversal of
;;     the tree.
;;   - for a dag or graph it will do the same thing with the following
;;     interpretation:
;;       - nodes are only included in the list the first time they are
;;         encoutered.
;;         nodes which can not be reached by a pre-order traversal of
;;         the graph from the root (i.e. nodes which can not be
;;         reached by traversing the child pointers from the roots)
;;         are added at the appropriate places.
(defun scalable-top-sort (graph roots)
  (labels
      ((top-sort-node (node)
         (cond
           ((kr:g-value node :layout-info-appears-in-top-sortp) nil)
           (t (kr:s-value node :layout-info-appears-in-top-sortp t)
              (let* ((children-links (kr:g-value node :links-from-me))
                     (children (mapcar #'(lambda (link) (kr:g-value link :to))
                                       children-links))
                     (top-sorted-children (scalable-map-reduce #'append
                                                      #'top-sort-node
                                                      children))
                     (parent-links (kr:g-value node :links-to-me))
                     (parents (mapcar #'(lambda (link) (kr:g-value link :from))
                                      parent-links))
                     (top-sorted-parents (scalable-map-reduce #'append
                                                     #'top-sort-node
                                                     parents)))
                (append top-sorted-parents
                        (cons node top-sorted-children)))))))
    (let* ((sort-list (scalable-map-reduce #'append #'top-sort-node roots))
           (all-nodes (g-value (kr:g-value graph :nodes) :components))
           (sort-list-for-nodes-not-reachable-from-roots
              (scalable-map-reduce #'append #'top-sort-node all-nodes))
           (total-sort-list
              (append sort-list sort-list-for-nodes-not-reachable-from-roots)))
      (values total-sort-list (reverse total-sort-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default layout function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function places the nodes of graph.  It works for arbitrary
;; graphs, but only creates pleasing layouts for trees.  (It works for
;; arbitrary graphs by effectively ignoring all but a subset of the links
;; which makes the graph a tree.)
;;
;; Requires:
;;  - the slots :layout-info-appears-in-top-sortp, :layout-info-left-setp,
;;    and :layout-info-right-setp in the node prototype must not be set by
;;    the user, (and so they have the default value of nil.)
;;  [The aggregraph object requires that no slot whose name begins with
;;  ":layout-info" be set by anything other than the layout function.]
(defun scalable-layout-tree (graph)
  ;; reset bookkeeping information
  (opal:do-components (kr:g-value graph :nodes)
    #'(lambda (n)
        (kr:s-value n :layout-info-appears-in-top-sortp nil)
        (kr:s-value n :layout-info-left-setp nil)
        (kr:s-value n :layout-info-top-setp nil)))
  (kr:s-value graph :layout-info-rect-conflict-object
              (make-rectangle-conflict-object))
  (multiple-value-bind (parent-child-top-sort child-parent-top-sort) 
                       (scalable-top-sort graph (kr:g-value graph :graph-roots))
    ;; compute :left value for each node
    (dolist (node parent-child-top-sort)
      (let* ((parent-links (kr:g-value node :links-to-me))
             (parents (mapcar #'(lambda (link) (kr:g-value link :from))
                              parent-links))
             (parents-with-left-set
              (remove-if #'(lambda (node)
                             (not (kr:g-value node :layout-info-left-setp)))
                         parents)))
        (if (not (null parents-with-left-set))
            (kr:s-value node :default-left
                        (+ (scalable-map-reduce #'max
                                       #'(lambda (parent)
                                           (+ (kr:g-value parent :default-left)
                                              (kr:g-value parent :default-width)))
                                       parents-with-left-set)
                           (kr:g-value graph :h-spacing)))
            (kr:s-value node :default-left (kr:g-value graph :left)))
        (kr:s-value node :layout-info-left-setp t)))
    ;; compute :default-top value for each node
    ;;   for each node from the children to the root give nodes with
    ;;   (effectively) no children the next available space.  If (some of)a
    ;;   node's children have already been given a y-value, then center the
    ;;   node over those children.
    (let ((current-y (kr:g-value graph :top))) ;; next place to put
                                               ;; node if no other info 
      (dolist (node child-parent-top-sort)
        (let* ((children-links (kr:g-value node :links-from-me))
               (children (mapcar #'(lambda (link) (kr:g-value link :to))
                                 children-links))
               (children-with-top-set
                (remove-if #'(lambda (node)
                               (not (kr:g-value node :layout-info-top-setp)))
                           children)))
          (cond
            ;; if there are (effectively) no children for this node then just
            ;; give it the next available y position and update the next
            ;; available y position.
            ((null children-with-top-set)
             (let ((top (scalable-find-good-y-near-desired-y graph node current-y)))
               ;; update the current-y to be the position under this
               ;; node.  (Will work well for trees because
               ;; find-good-y-near-desired-y will just place the node
               ;; at current-y.  This means that the leaves are just
               ;; placed one right after another.  It may or may not
               ;; work well for dags and graphs.)
               (setf current-y (+ current-y
                                  (kr:g-value node :default-height)
                                  (kr:g-value graph
                                              :v-spacing)))
               (kr:s-value node :default-top top)))
            ;; otherwise center the node over it's (effective) children
            (t 
             (let ((total-centers 0))
               (dolist (child children-with-top-set)
                 (setf total-centers
                       (+ total-centers
                          (kr:g-value child :default-top)
                          (floor (kr:g-value child :default-height) 2))))
               (let ((desired-y (- (floor total-centers
                                          (length children-with-top-set))
                                   (floor (kr:g-value node :default-height) 2))))
                 (kr:s-value node :default-top
                         (scalable-find-good-y-near-desired-y graph node desired-y))))))
          (kr:s-value node :layout-info-top-setp t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; aggregraph object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition of aggregraph object
(kr:create-instance 'scalable-aggregraph opal:aggregadget

   ;; must be specified in created instance to actually create graph

   (:source-roots nil)
   (:children-function nil)
   (:info-function nil)
   
   ;; customizable slots

   (:left 0)
   (:top 0)
   (:parent-function nil)
   (:add-back-pointer-to-nodes-function nil)
   (:node-prototype scalable-aggregraph-node-prototype)
   (:link-prototype scalable-aggregraph-link-prototype)
   (:node-prototype-selector-function nil)
   (:link-prototype-selector-function nil)
   ;(:interactors nil) ;; inherited from aggregadget
   (:h-spacing 20)
   (:v-spacing 5)
   (:test-to-distinguish-source-nodes #'eql)

   ;; methods (can be overridden)
   
   (:layout-graph #'scalable-layout-tree)
   (:delete-link #'(lambda (graph link)
                     (let ((parent (kr:g-value link :from))
                           (child (kr:g-value link :to)))
                       ;; remove me from :links-{to/from}-me lists
                       (kr:s-value parent :links-from-me
                            (remove link (kr:g-value parent :links-from-me)))
                       (kr:s-value child :links-to-me
                            (remove link (kr:g-value child :links-to-me)))
                       ;; delete any image links
                       (mapc #'(lambda (image-graph image-link)
                                 (kr:kr-send image-graph
                                             :primitive-delete-link
                                             image-graph image-link))
                             (kr:g-value graph :image-graphs)
                             (kr:g-value link :image-links))
                       ;; now delete it
                       (opal:remove-component (kr:g-value graph :links) link)
                       (opal:destroy link))))
   (:delete-node #'(lambda (graph node)
                     ;; destroy the links to and from the node
                     (dolist (link (kr:g-value node :links-to-me))
                       (kr:kr-send graph :delete-link graph link))
                     (dolist (link (kr:g-value node :links-from-me))
                       (kr:kr-send graph :delete-link graph link))
                     ;; now take it off of the root lists if is a root
                     (when (member node (kr:g-value graph :graph-roots))
                       (kr:s-value graph :source-roots
                                   (remove (kr:g-value node :source-node)
                                           (kr:g-value graph :source-roots)))
                       (kr:s-value graph :graph-roots
                                   (remove node
                                           (kr:g-value graph :graph-roots))))
                     ;; remove it from the rectangle-conflict-object
                     ;; to mark the space as free again.
                     (scalable-remove-node-from-conflict-object graph node)
                     ;; remove it from the source->graph-node-table
                     (remhash (kr:g-value node :source-node)
                              (kr:g-value graph :source->graph-node-table))
                     ;; delete any image nodes
                     (mapc #'(lambda (image-graph image-node)
                               (kr:kr-send image-graph
                                           :primitive-delete-node
                                           image-graph image-node))
                           (kr:g-value graph :image-graphs)
                           (kr:g-value node :image-nodes))
                     ;; now delete it
                     (opal:remove-component (kr:g-value graph :nodes) node)
                     (opal:destroy node)))
   (:add-node #'(lambda (graph source-node parent-g-nodes children-g-nodes)
                  (let ((node (make-scalable-graph-node source-node graph)))
                    (opal:add-component (kr:g-value graph :nodes) node)
                    ;; add links from parents to node
                    (dolist (parent parent-g-nodes)
                      (kr:kr-send graph :add-link graph parent node))
                    ;; add links from node to children
                    (dolist (child children-g-nodes)
                      (kr:kr-send graph :add-link graph node child))
		    ;; add node to hash table
		    (setf (gethash source-node
				   (g-value graph :source->graph-node-table))
			  node)
                    ;; position node
                    (let* (;; first compute left
                           ;;   based on children
                           (right+space
                              (and children-g-nodes
                                   (scalable-map-reduce #'min
                                               #'(lambda (c)
                                                   (kr:g-value c :default-left))
                                               children-g-nodes)))
                           (cleft (and right+space
                                       (- right+space (kr:g-value node :default-width)
                                          (kr:g-value graph :h-spacing))))
                           ;;   based on parents
                           (pleft
                              (and parent-g-nodes
                                   (+ (scalable-map-reduce #'max
                                                  #'(lambda (p)
                                                      (+ (kr:g-value p :default-left)
                                                         (kr:g-value p :default-width)))
                                                  parent-g-nodes)
                                      (kr:g-value graph :h-spacing))))
                           ;;   total =
                           ;;     0, if no parents or children
                           ;;     pleft or cleft if other is nil
                           ;;     average otherwise
                           (left (cond
                                   ((and (null pleft) (null cleft)) 0)
                                   ((null pleft) cleft)
                                   ((null cleft) pleft)
                                   (t (floor (+ pleft cleft) 2))))
                           ;; next compute top.  average over centers of
                           ;; both parents and children
                           (total-centers 0))
                      (dolist (p parent-g-nodes)
                        (setf total-centers
                              (+ total-centers
                                 (kr:g-value p :default-top)
                                 (floor (kr:g-value p :default-height) 2))))
                      (dolist (c children-g-nodes)
                        (setf total-centers
                              (+ total-centers
                                 (kr:g-value c :default-top)
                                 (floor (kr:g-value c :default-height) 2))))
                      (let ((top (if (and (null parent-g-nodes)
                                          (null children-g-nodes))
                                     0
                                     (- (floor total-centers
                                               (+ (length children-g-nodes)
                                                  (length parent-g-nodes)))
                                        (floor (kr:g-value node :default-height) 2)))))
                        ;; now set the values
                        (kr:s-value node :default-left left)
                        (kr:s-value node :default-top (scalable-find-good-y-near-desired-y
                                               graph node top))))
                    ;; add node to any image graphs
                    (mapc #'(lambda (image-graph)
                              (kr:kr-send image-graph
                                          :primitive-add-node
                                          image-graph node))
                          (kr:g-value graph :image-graphs))
                    )))
   (:add-link #'(lambda (graph from to)
                  (let ((link (make-scalable-graph-link from to graph)))
                    (opal:add-component (kr:g-value graph :links) link)
                    (push link (kr:g-value from :links-from-me))
                    (push link (kr:g-value to :links-to-me))
                    ;; add link to any image graphs
                    (mapc #'(lambda (image-graph)
                              (kr:kr-send image-graph
                                          :primitive-add-link
                                          image-graph link))
                          (kr:g-value graph :image-graphs))
                    )))
   (:make-root #'(lambda (graph node)
                   (when (not (member node (kr:g-value graph :graph-roots)))
                     (push node (kr:g-value graph :graph-roots))
                     (push (kr:g-value node :source-node)
                           (kr:g-value graph :source-roots)))))
   (:remove-root #'(lambda (graph node)
                     (setf (kr:g-value graph :graph-roots)
                           (remove node (kr:g-value graph :graph-roots)))
                     (setf (kr:g-value graph :source-roots)
                           (remove (kr:g-value node :source-node)
                                   (kr:g-value graph :source-roots)))))
   (:source-to-graph-node
         #'(lambda (graph source-node)
             (gethash source-node
                      (kr:g-value graph :source->graph-node-table))))
   (:find-link #'(lambda (graph from to)
                   (declare (ignore graph))
                   (intersection (kr:g-value from :links-from-me)
                                 (kr:g-value to :links-to-me)
                                 :test #'eq)))
   
   ;; read-only slots
   
   (:nodes nil) ; set by initialize
   (:links nil) ; set by initialize
   (:graph-roots nil)

   ;; internal slots (not guaranteed to maintain names or implementation)

   ;; make an initial rectangle conflict object that always says there
   ;; is no conflict.  This allows people to write new layout
   ;; functions without redefining the methods.  The methods will work
   ;; if they do this, but perhaps not as nicely as if they redefine
   ;; them to do the appropriate bookkeeping for their layout.
   (:layout-info-rect-conflict-object #'(lambda (&rest args)
					  (declare (ignore args))
					  NIL))
   (:source->graph-node-table nil)
   (:internal-parent-function (kr:o-formula
                               (or (kr:gvl :parent-function)
                                   #'(lambda (source-node depth)
                                       (declare (ignore source-node depth))
                                       nil))))
   (:internal-scale-factor (kr:o-formula (let ((sf (kr:gvl :scale-factor)))
                                           (cond
                                             ((numberp sf) sf)
                                             (t 1)))))
   (:internal-node-prototype nil) ; set by initialize
   )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to make
;; internal node prototype
;;;;;;;;;;;;;;;;;;;;;;;;;
;; this function takes the user provided node prototype and creates a new
;; prototype with appropriate formulas to make the nodes adapt to
;; changes in the :scale-factor
(defun create-internal-scalable-node-prototype (prototype)
  (kr:create-instance nil prototype
     (:default-top (kr:g-value prototype :top))
     (:default-left (kr:g-value prototype :left))
     (:default-width (kr:g-value prototype :width))
     (:default-height (kr:g-value prototype :height))
     (:scale-factor (kr:o-formula
                     (kr:gvl :parent :parent :internal-scale-factor)))
     (:left (kr:o-formula (floor (* (kr:gvl :default-left)
                                    (kr:gvl :scale-factor)))))
     (:top (kr:o-formula (floor (* (kr:gvl :default-top)
                                   (kr:gvl :scale-factor)))))
     (:width (kr:o-formula (floor (* (kr:gvl :default-width)
                                     (kr:gvl :scale-factor)))))
     (:height (kr:o-formula (floor (* (kr:gvl :default-height)
                                      (kr:gvl :scale-factor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :initialize method
;; for aggregraph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to set up all internal structure for the aggregraph object,
;; including making and placing the graphical nodes and links.
(kr:define-method :initialize scalable-aggregraph (self &rest args)
 (declare (ignore args))
 (kr:call-prototype-method self)
 (when (and (kr:g-value self :source-roots)
            (kr:g-value self :children-function)
            (kr:g-value self :info-function))
   (let ((node-agg (kr:create-instance nil opal:aggregate))
         (link-agg (kr:create-instance nil opal:aggregate)))
     ;; need to test that :source-roots, :children-function, and
     ;; :info-function contain reasonable values
     (kr:s-value self :nodes node-agg)
     (kr:s-value self :links link-agg)
     (opal:add-components self link-agg node-agg))
   (kr:s-value self :source->graph-node-table
               (make-hash-table
                :test (kr:g-value self :test-to-distinguish-source-nodes)
                :size 50
                #-cmu :rehash-size #-cmu 2.0))
   (let ((node-prototype (kr:g-value self :node-prototype))
         (link-prototype (kr:g-value self :link-prototype)))
     ;; create internal node prototype
     (kr:s-value self :internal-node-prototype
                 (if (listp node-prototype)
                     (mapcar #'create-internal-scalable-node-prototype node-prototype)
                     (create-internal-scalable-node-prototype node-prototype)))
     ;; test that if multiple prototypes are included that a selector
     ;; function has been provided
     (when (and (listp node-prototype)
                (not (functionp-and-not-null
                      (kr:g-value self :node-prototype-selector-function))))
       (format t "~&Error in aggregraph:  :node-prototype is a list but no selector function was")
       (format t "~&                      provided in slot :node-prototype-selector-function.")
       (format t "~&                      First one in the list will be used.")
       (kr:s-value self :node-prototype-selector-function
                   #'(lambda (node l)
                       (declare (ignore node))
                       (car l))))
     (when (and (listp link-prototype)
                (not (functionp-and-not-null
                      (kr:g-value self :link-prototype-selector-function))))
       (format t "~&Error in aggregraph:  :link-prototype is a list but no selector function was")
       (format t "~&                      provided in slot :link-prototype-selector-function.")
       (format t "~&                      First one in the list will be used.")
       (kr:s-value self :link-prototype-selector-function
                   #'(lambda (from to l)
                       (declare (ignore from to))
                       (car l)))))
   ;; create nodes and links
   (create-scalable-aggregraph-nodes-and-links self)
   ;; layout graph (place nodes)
   (kr:kr-send self :layout-graph self)))

(defun make-scalable-graph-node (source-node graph)
  (let* ((prototype-arg (kr:g-value graph :internal-node-prototype))
         (prototype (cond
                      ;; if the user gave a list of prototypes then choose one
                      ((listp prototype-arg)
                       (let ((selector-f
                              (kr:g-value graph
                                          :node-prototype-selector-function)))
                         (funcall selector-f source-node prototype-arg)))
                      (t prototype-arg)))
         (graph-node (kr:create-instance nil prototype
                        (:info (funcall (kr:g-value graph :info-function)
                                        source-node))
                        (:source-node source-node)
                        (:links-to-me '())
                        (:links-from-me '())))
         (back-pointer-fun
            (kr:g-value graph :add-back-pointer-to-nodes-function)))
    (when back-pointer-fun
      (funcall back-pointer-fun source-node graph-node))
    graph-node))


(defun make-scalable-graph-link (from to graph)
  (let* ((prototype-arg (kr:g-value graph :link-prototype))
         (prototype (cond
                      ;; if the user gave a list of prototypes then choose one
                      ((listp prototype-arg)
                       (let ((selector-f
                              (kr:g-value graph
                                          :link-prototype-selector-function)))
                         (funcall selector-f from to prototype-arg)))
                      (t prototype-arg))))
    (kr:create-instance nil prototype
                        (:from from)
                        (:to to))))

;; walks the source graph with :source-roots and :children-function,
;; creating graph nodes from :node-prototype and :info-function.  It places
;; these graph nodes in the hash table :source->graph-node-table indexed by
;; the source nodes.   
;; values used in aggregraph:
;;   :source-roots
;;   :children-function
;;   :info-function
;;   :node-prototype
;; values set in aggregraph:
;;   :graph-roots
;; values changed in aggregraph:
;;   :source->graph-node-table
;; values set in nodes:
;;   :info
;;   :source-node
;;   :links-to-me
;;   :links-from-me
(defun create-scalable-nodes-and-links-from-root (root graph depth hash-table)
  (labels ((link-together (from-node to-node)
             (if (member-if #'(lambda (link)
                                (eq (kr:g-value link :to) to-node))
                            (kr:g-value from-node :links-from-me))
                 nil
                 (let ((link (make-scalable-graph-link
                              from-node to-node graph)))
                   (opal:add-component (kr:g-value graph :links) link)
                   (push link (kr:g-value from-node :links-from-me))
                   (push link (kr:g-value to-node :links-to-me))))))
    (let ((graph-node (gethash root hash-table)))
      (if graph-node
          graph-node
        (let ((graph-node (make-scalable-graph-node root graph)))
          (setf (gethash root hash-table) graph-node)
          (opal:add-component (kr:g-value graph :nodes) graph-node)
          (let* ((source-children (funcall
                                   (kr:g-value graph :children-function)
                                   root
                                   depth))
                 (source-parents (funcall
                                  (kr:g-value graph :internal-parent-function)
                                  root
                                  depth))
                 (graph-children
                  (mapcar #'(lambda (n)
                              (create-scalable-nodes-and-links-from-root
                                   n graph (+ depth 1) hash-table))
                          source-children))
                 (graph-parents
                  (mapcar #'(lambda (n)
                              (create-scalable-nodes-and-links-from-root
                                   n graph (+ depth 1) hash-table))
                          source-parents)))
            (dolist (child graph-children)
                    (link-together graph-node child))
            (dolist (parent graph-parents)
                    (link-together parent graph-node)))
          graph-node)))))

(defun create-scalable-aggregraph-nodes-and-links (graph)
  (let ((hash-table (kr:g-value graph :source->graph-node-table)))
    (kr:s-value graph
                :graph-roots
                (mapcar #'(lambda (n)
                            (create-scalable-nodes-and-links-from-root
                                 n graph 0 hash-table))
                        (kr:g-value graph :source-roots)))))

;;; Support functions for layout and general placement of nodes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scalable-find-free-space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function takes a rectangle-conflict-object and a rectangle
;; (x1, x2, y1, y2).  It returns a position where the given rectangle
;; will not overlap with any previous rectangles placed in
;; rectangle-conflict-object.  (The position is given by specifying a
;; new (x1,y1).)  The returned position will be the closest possible
;; to the initial initial position (given by (x1,y1)) by varying only
;; along the y axis and not going above 'top'.
(defun scalable-find-free-space (rect-conf-obj x1 x2 y1 y2 top)
  (let ((conflicting-rect (funcall rect-conf-obj x1 x2 y1 y2 nil)))
    (if conflicting-rect
        (let ((lowy (caddr conflicting-rect))
              (highy (cadddr conflicting-rect))
              (ysize (- y2 y1)))
          (scalable-find-space-by-varying-y
           rect-conf-obj x1 x2 lowy highy ysize top))
        (values x1 y1))))

;; one way to implement scalable-find-free-space
(defun scalable-find-space-by-varying-y (rect-conf-obj x1 x2 lowy
                                                       highy ysize top)
  (let ((below-highy (+ highy 1))
        (above-lowy (and lowy (- lowy 1))))
    ;; try below highy
    (let ((conf (funcall rect-conf-obj
                         x1 x2 below-highy (+ below-highy ysize) nil)))
      (cond
        ;; if no conflict then put it there
        ((not conf) 
         (values x1 below-highy))
        ;; if a conflict and not at the top then try above lowy
        ((and above-lowy (>= (- above-lowy ysize) top))
         (let ((conf2 (funcall rect-conf-obj x1 x2
                               (- above-lowy ysize) above-lowy nil)))
           (if conf2
               ;; try further out from conflicting rectangles
               (scalable-find-space-by-varying-y
                rect-conf-obj x1 x2 (caddr conf2) (cadddr conf) ysize top)
               ;; use above lowy
               (values x1 (- above-lowy ysize)))))
        ;; otherwise try further below conflicting rectangle
        (t
         (scalable-find-space-by-varying-y
          rect-conf-obj x1 x2 nil (cadddr conf) ysize top))
        ))))
          
(defun scalable-find-good-y-near-desired-y (graph node desired-y)
  ;; finds a free space for the node near the desired-y position and
  ;; updates the :layout-info-rect-conflict-object with the knowledge
  ;; that a node was placed there.  Returns the y position of the good
  ;; place. 
  (let* ((rect-conf-obj
          (kr:g-value graph :layout-info-rect-conflict-object))
         ;; get node's rectangle
         (node-x1 (kr:g-value node :default-left))
         (node-x2 (+ node-x1 (kr:g-value node :default-width)))
         (node-y1 desired-y)
         (node-y2 (+ desired-y (kr:g-value node :default-height)))
         ;; 2.01 is used to avoid conflicts when nodes are exactly the
         ;; correct distance apart.  May cause nodes to be one pixel
         ;; closer together than desired.
         (x-between (floor (kr:g-value graph :h-spacing) 2.01))
         (y-between (floor (kr:g-value graph :v-spacing) 2.01))
         ;; rectangle of influence
         (x1 (- node-x1 x-between))
         (x2 (+ node-x2 x-between))
         (y1 (- node-y1 y-between))
         (y2 (+ node-y2 y-between)))
    (multiple-value-bind (left top)
                         (scalable-find-free-space
                          rect-conf-obj x1 x2 y1 y2 (kr:g-value graph :top))
      (declare (ignore left))
      (+ top y-between))))

(defun scalable-remove-node-from-conflict-object (graph node)
  ;; removes a node from the rectangle-conflict-object so that the
  ;; space it occupied can be used again.
  (let* ((rect-conf-obj
          (kr:g-value graph :layout-info-rect-conflict-object))
         ;; get node's rectangle
         (node-x1 (kr:g-value node :default-left))
         (node-x2 (+ node-x1 (kr:g-value node :default-width)))
         (node-y1 (kr:g-value node :default-top))
         (node-y2 (+ node-y1 (kr:g-value node :default-height)))
         ;; find node's rectangle of influence in exactly the same
         ;; manner as was stored by scalable-find-good-y-near-desired-y.
         (x-between (floor (kr:g-value graph :h-spacing) 2.01))
         (y-between (floor (kr:g-value graph :v-spacing) 2.01))
         ;; rectangle of influence
         (x1 (- node-x1 x-between))
         (x2 (+ node-x2 x-between))
         (y1 (- node-y1 y-between))
         (y2 (+ node-y2 y-between)))
    ;; now delete rectangle of influence
    (funcall rect-conf-obj x1 x2 y1 y2 'delete)))

;;;;;;;;;;;;;;;;
;; scalable-map-reduce
;;;;;;;;;;;;;;;;
;; this function has exactly the same functionality as
;; (reduce reduce-f (mapcar map-f list)) without doing the
;; intermediate consing of that form.
(defun scalable-map-reduce (reduce-f map-f list)
  (labels ((scalable-map-reduce (first rest)
             (cond
               ((null rest) (funcall map-f first))
               (t (funcall reduce-f
                           (funcall map-f first)
                           (scalable-map-reduce (car rest) (cdr rest)))))))
    (cond
      ((null list) '())
      (t (scalable-map-reduce (car list) (cdr list))))))  
