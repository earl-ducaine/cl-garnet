;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-

;;;
;; AggreTrees from Pedro Szekely.  These will probably be eventually
;; combined with aggregraphs.
;;                     
;;  Author:  Pedro Szekely, USC/Information Sciences Institute
;;  Date: 3/14/91
;;
;;  Report bugs and comments to szekely@isi.edu


;;;----------------------------------------------------------------------------
;;
;; This file contains aggregates to display trees.
;; Currently the following kinds of tree display are supported:
;; 
;; AggreTree-1
;; 
;; Slots:
;; 
;; :tree - the tree object to be displayed
;; :node-prototype - The widget to display each of the nodes.  The sub-tree headed
;;                   by that node is stored in the :tree slot of the node.
;; :h-spacing - the horizontal spacing between nodes/sub-trees.
;; :v-spacing - the vertical spacing between levels of the tree.
;; :line-style - an opal:line-style to draw the lines.
;; :children-function - a function of to extract the next level of sub-nodes of a tree.
;; :expansion-level - the number of levels that each node should be expanded.  Can
;;                    be set in any node to arbitrarily control the expansion of the
;;                    sub-tree rooted at that node.  See aggretree-demo example.
;; :cache-nodes-p - whether to cache the nodes when sub-trees are closed.  The cache
;;                  is stored in the aggretree, so it is local to a specific aggretree.  
;;
;; AggreTree-2
;; 
;; Slots:
;; 
;; :lines-p - whether to show lines connecting the nodes.  Can't be changed after
;;            the aggretree is created.
;; :node-indent - length of the horizontal lines.
;; :sub-tree-indent - indentation of the vertical line.
;; 
;; The following slots have the same meaning as in AggreTree-1:
;; :line-style :v-spacing :children-function :tree :node-prototype
;; :expansion-level :cache-nodes-p
;; 
;; Functions:
;; 
;; revert-aggretree - redisplays the tree from scratch.
;; revert-aggretree-node - redisplays the tree starting at a specific node.
;; 
;; ----------------------------------------------------------------------------
;; Known bugs and planned enhancements:
;; 
;; Planned enhancements:
;; - Define formulas for :width and :height that don't depend on :left and :top.
;;   This is needed to be able to center trees inside other aggregates.
;; ----------------------------------------------------------------------------


(in-package :garnet-gadgets)

(export '(AggreTree-1
	  AggreTree-2
	  revert-aggretree
	  revert-aggretree-node
	  ))


(create-instance 'Tree-Node-Prototype opal:text
  (:string (o-formula (format nil "~S" (gvl :tree)))))


(create-instance 'Tree-Sub-Aggregate opal:aggregate
  (:left (o-formula (gvl :parent :left)))
  (:top (o-formula (gvl :parent :top))))


(defun create-aggretree-lines (agg node)
  (kr-send agg :create-aggretree-lines agg node))

(defun instantiate-prototype (agg parent-node previous tree node-prototype)
  (kr-send agg :instantiate-prototype agg parent-node previous tree node-prototype))

(defun delete-lines (agg node delete-line-to-parent-p)
  (kr-send agg :delete-lines agg node delete-line-to-parent-p))

(defun return-node-to-free-list (agg node)
  (s-value node :previous nil)
  (s-value node :parent-node nil)
  (s-value node :tree nil)
  (s-value node :sub-nodes nil)
  (s-value node :parent-node nil)
  (push node (g-value agg :node-free-list)))
       
(defun delete-node (agg node)
  (delete-lines agg node T)
  (dolist (n (g-value node :sub-nodes))
    (delete-node agg n))
  (opal:remove-component (g-value node :parent) node)
  (if (g-value agg :cache-nodes-p)
      (return-node-to-free-list agg node)
      (opal:destroy node)))

(defun create-aggretree (tree parent-agg)
  (let ((node-prototype (g-value parent-agg :node-prototype))
	(nodes-agg (create-instance nil Tree-Sub-Aggregate))
	(lines-agg (create-instance nil Tree-Sub-Aggregate)))
    ;; add the nodes and lines aggregate
    (s-value parent-agg :nodes-aggregate nodes-agg)
    (s-value parent-agg :lines-aggregate lines-agg)
    (opal:add-components parent-agg nodes-agg lines-agg)
    ;; Add the root and the tree
    (let ((root-widget (instantiate-prototype parent-agg nil nil tree node-prototype)))
      (s-value root-widget :expansion-level (g-value parent-agg :expansion-level))
      (opal:add-component nodes-agg root-widget)
      (s-value nodes-agg :root root-widget)
      (s-value parent-agg :root root-widget)
      (create-aggretree-children parent-agg root-widget node-prototype)
      (create-aggretree-lines parent-agg root-widget)
      (values root-widget))))

(defun create-aggretree-children (agg node-widget node-prototype)
  (let ((level (g-value node-widget :expansion-level)))
    (when (> level 0)
      (let* ((tree (g-value node-widget :tree))
	     (previous nil)
	     (new-node-list nil)
	     new-node)
	(dolist (sub-tree (funcall (g-value agg :children-function) tree))
	  (setf new-node
		(instantiate-prototype agg node-widget previous sub-tree node-prototype))
	  (opal:add-component (g-value node-widget :parent) new-node)
	  (push new-node new-node-list)
	  (setf previous new-node)
	  (s-value new-node :expansion-level (1- level))
	  (create-aggretree-children agg new-node node-prototype))
	(s-value node-widget :sub-nodes (nreverse new-node-list))
	(values node-widget)))))

(defun revert-aggretree-node (agg node)
  "Redisplay the tree headed by a node."
    (delete-lines agg node nil)
    (dolist (n (g-value node :sub-nodes))
      (delete-node agg n))
    (s-value node :sub-nodes nil)
    (create-aggretree-children agg node (g-value node :parent :parent :node-prototype))
    (create-aggretree-lines agg node))

(defun revert-aggretree (agg)
  "Recompute the whole aggretree."
  (opal:destroy (g-value agg :nodes-aggregate))
  (opal:destroy (g-value agg :lines-aggregate))
  (create-aggretree (g-value agg :tree) agg))


;;; Layout computation functions.
;;


(defun compute-aggretree-1-width ()
  (let ((sub-nodes (gvl :sub-nodes)))
    (if sub-nodes
	(max (gvl :width)
	     (let* ((h-spacing (gvl :parent :parent :h-spacing))
		    (result (- h-spacing)))
	       (dolist (node sub-nodes)
		 (incf result (+ (gv node :tree-width) h-spacing)))
	       (values result)))
	(gvl :width))))

(defun compute-aggretree-1-left ()
  (let ((previous (gvl :previous)))
    (if previous
	(+ (gv previous :tree-left) (gv previous :tree-width)
	   (gvl :parent :parent :h-spacing))
	(let ((parent-node (gvl :parent-node)))
	  (if parent-node (gv parent-node :tree-left) (gvl :parent :left))))))

(defun compute-aggretree-1-node-left ()
  (+ (gvl :tree-left) (truncate (- (gvl :tree-width) (gvl :width)) 2)))

(defun compute-aggretree-1-node-top ()
  (let ((parent-node (gvl :parent-node)))
    (if parent-node
	(+ (opal:gv-bottom parent-node) (gvl :parent :parent :v-spacing))
	(gvl :parent :top))))



;;; Prototypes for Lines.
;;
    
(create-instance 'AggreTree-1-Horizontal-Line opal:line
  (:line-style (o-formula (gvl :parent :parent :line-style)))
  (:first-node (o-formula (car (gvl :node :sub-nodes))))
  (:last-node (o-formula (car (last (gvl :node :sub-nodes)))))
  (:x1 (o-formula (+ (gvl :first-node :left)
		     (truncate (gvl :first-node :width) 2))))
  (:x2 (o-formula (- (opal:gv-right (gvl :last-node))
		     (truncate (gvl :last-node :width) 2))))
  (:y1 (o-formula (+ (opal:gv-bottom (gvl :node))
		     (truncate (gvl :parent :parent :v-spacing) 2))))
  (:y2 (o-formula (gvl :y1))))

(create-instance 'AggreTree-1-Bottom-Vertical-Line opal:line
  (:line-style (o-formula (gvl :parent :parent :line-style)))
  (:x1 (o-formula (opal:gv-center-x (gvl :node))))
  (:x2 (o-formula (gvl :x1)))
  (:y1 (o-formula (opal:gv-bottom (gvl :node))))
  (:y2 (o-formula (gvl :horizontal-line :y1))))

(create-instance 'AggreTree-1-Top-Vertical-Line opal:line
  (:line-style (o-formula (gvl :parent :parent :line-style)))
  (:x1 (o-formula (opal:gv-center-x (gvl :node))))
  (:x2 (o-formula (gvl :x1)))
  (:y1 (o-formula (gvl :node :top)))
  (:y2 (o-formula (gvl :horizontal-line :y1))))



;;; Functions to create and delete lines
;;


(defun create-aggretree-1-lines (agg node-widget)
  (let ((sub-nodes (g-value node-widget :sub-nodes)))
    (when sub-nodes
      (let* ((horiz-line (create-instance nil AggreTree-1-Horizontal-Line
			   (:node node-widget)))
	     (bot-vert-line (create-instance nil AggreTree-1-Bottom-Vertical-Line
			      (:horizontal-line horiz-line)
			      (:node node-widget))))
	(opal:add-components (g-value node-widget :parent :parent :lines-aggregate)
			     horiz-line bot-vert-line)
	(s-value node-widget :horizontal-line horiz-line)
	(s-value node-widget :bottom-vertical-line bot-vert-line)
	(dolist (node sub-nodes)
	  (create-aggretree-1-lines agg node)
	  (let ((top-vert-line (create-instance nil AggreTree-1-Top-Vertical-Line
				 (:horizontal-line horiz-line)
				 (:node node))))
	    (s-value node :top-vertical-line top-vert-line)
	    (opal:add-components (g-value node :parent :parent :lines-aggregate)
				 top-vert-line)))))
    (values node-widget)))


(defun delete-aggretree-1-lines (agg node delete-line-to-parent-p)
  (declare (ignore agg))
  (let ((h-line (g-value node :horizontal-line))
	(bot-line (g-value node :bottom-vertical-line)))
    (when h-line
      (s-value node :horizontal-line nil)
      (opal:remove-component (g-value h-line :parent) h-line)
      (opal:destroy h-line))
    (when bot-line
      (s-value node :bottom-vertical-line nil)
      (opal:remove-component (g-value bot-line :parent) bot-line)
      (opal:destroy bot-line)))
  (when delete-line-to-parent-p
    (let ((top-line (g-value node :top-vertical-line)))
      (when top-line
	(s-value node :top-vertical-line nil)
	(opal:remove-component (g-value top-line :parent) top-line)
	(opal:destroy top-line)))))


;;; The aggregadget
;;

(create-instance 'AggreTree-1 opal:aggregadget
  (:expansion-level 999)
  (:cache-nodes-p T)
  (:h-spacing 20)
  (:v-spacing 20)
  (:line-style opal:default-line-style)
  (:width (o-formula (gvl :root :tree-width)))
  (:left 0)
  (:top 0)
  (:children-function "Your children function here")
  (:tree "Your tree object here")
  (:node-prototype Tree-Node-Prototype))

(define-method :initialize AggreTree-1 (agg)
  (call-prototype-method agg)
  (create-aggretree (g-value agg :tree) agg))

(define-method :create-aggretree-lines AggreTree-1 (agg node)
  (create-aggretree-1-lines agg node))

(define-method :delete-lines AggreTree-1 (agg node delete-line-to-parent-p)
  (delete-aggretree-1-lines agg node delete-line-to-parent-p))

(define-method :instantiate-prototype AggreTree-1
    (agg parent-node previous tree node-prototype)
  (declare (ignore agg))
  (create-instance nil node-prototype
    (:left (o-formula (compute-aggretree-1-node-left)))
    (:top (o-formula (compute-aggretree-1-node-top)))
    (:tree-width (o-formula (compute-aggretree-1-width)))
    (:tree-left (o-formula (compute-aggretree-1-left)))
    (:previous previous)
    (:parent-node parent-node)
    (:tree tree)))


(define-method :destroy-me AggreTree-1 (agg &optional top-level-p)
  (dolist (node (g-value agg :node-free-list)) (opal:destroy node))
  (call-prototype-method agg top-level-p))



;;; Aggretree-2
;;

(defun compute-aggretree-2-height ()
  (let ((sub-nodes (gvl :sub-nodes)))
    (+ (gvl :height)
       (let ((v-spacing (gvl :parent :parent :v-spacing))
	     (result 0))
	 (dolist (node sub-nodes)
	   (incf result (+ v-spacing (gv node :tree-height))))
	 (values result)))))

(defun compute-aggretree-2-node-left ()
  (let ((parent-node (gvl :parent-node)))
    (if parent-node
	(+ (gv parent-node :left)
	   (gvl :parent :parent :node-indent)
	   (gvl :parent :parent :sub-tree-indent))
	(gvl :parent :left))))

(defun compute-aggretree-2-node-top ()
  (let ((parent-node (gvl :parent-node))
	(previous (gvl :previous)))
    (cond (previous
	   (+ (gv previous :top)
	      (gv previous :tree-height)
	      (gvl :parent :parent :v-spacing)))
	  (parent-node
	   (+ (opal:gv-bottom parent-node)
	      (gvl :parent :parent :v-spacing)))
	  (t
	   (gvl :parent :top)))))



;;; Prototypes for Lines.
;;


(create-instance 'Aggretree-2-Vertical-Line opal:line
  (:line-style (o-formula (gvl :parent :parent :line-style)))
  (:x1 (o-formula (+ (gvl :node :left) (gvl :parent :parent :sub-tree-indent))))
  (:x2 (o-formula (gvl :x1)))
  (:y1 (o-formula (opal:gv-bottom (gvl :node))))
  (:y2 (o-formula (opal:gv-center-y (car (last (gvl :node :sub-nodes)))))))

(create-instance 'Aggretree-2-Horizontal-Line opal:line
  (:line-style (o-formula (gvl :parent :parent :line-style)))
  (:x1 (o-formula (gvl :node :parent-node :vertical-line :left)))
  (:x2 (o-formula (gvl :node :left)))
  (:y1 (o-formula (opal:gv-center-y (gvl :node))))
  (:y2 (o-formula (gvl :y1))))


;;; Functions to create and delete lines
;;


(defparameter *at2-vertical-line-free* nil)
(defparameter *at2-horizontal-line-free* nil)

(defun create-aggretree-2-lines (agg node-widget)
  (when (g-value agg :lines-p)
    (let ((sub-nodes (g-value node-widget :sub-nodes)))
      (when sub-nodes
	(let ((vert-line
	       (or (pop *at2-vertical-line-free*)
		   (create-instance nil Aggretree-2-Vertical-Line))))
	  (s-value vert-line :node node-widget)
	  (opal:add-component (g-value node-widget :parent :parent :lines-aggregate)
			      vert-line)
	  (s-value node-widget :vertical-line vert-line))
	(dolist (node sub-nodes)
	  (create-aggretree-2-lines agg node)
	  (let ((horiz-line (or (pop *at2-horizontal-line-free*)
				(create-instance nil Aggretree-2-Horizontal-Line))))
	    (s-value horiz-line :node node)
	    (opal:add-component (g-value node-widget :parent :parent :lines-aggregate)
				horiz-line)
	    (s-value node :horizontal-line horiz-line)))))))

(defun delete-aggretree-2-lines (agg node delete-line-to-parent-p)
  (declare (ignore agg))
  (let ((v-line (g-value node :vertical-line)))
    (when v-line
      (s-value node :vertical-line nil)
      (opal:remove-component (g-value v-line :parent) v-line)
      #+out(opal:destroy v-line)
      (s-value v-line :node nil)
      (push v-line *at2-vertical-line-free*)
      ))
  (when delete-line-to-parent-p
    (let ((p-line (g-value node :horizontal-line)))
      (when p-line
	(s-value node :horizontal-line nil)
	(opal:remove-component (g-value p-line :parent) p-line)
	#+out(opal:destroy p-line)
	(s-value p-line :node nil)
	(push p-line *at2-horizontal-line-free*)
	))))


;;; The aggregadget
;;
;

(create-instance 'Aggretree-2 opal:aggregadget
  (:expansion-level 999)
  (:cache-nodes-p Nil)
  (:lines-p T)
  (:v-spacing 5)
  (:node-indent 20)
  (:sub-tree-indent 20)
  (:line-style opal:default-line-style)
  (:left 0)
  (:top 0)
  (:children-function "Your children function here")
  (:tree "Your tree object here")
  (:node-prototype Tree-Node-Prototype))

(define-method :initialize Aggretree-2 (agg)
  (call-prototype-method agg)
  (create-aggretree (g-value agg :tree) agg))


(define-method :create-aggretree-lines AggreTree-2 (agg node)
  (create-aggretree-2-lines agg node))

(define-method :delete-lines AggreTree-2 (agg node delete-line-to-parent-p)
  (delete-aggretree-2-lines agg node delete-line-to-parent-p))


(define-method :instantiate-prototype AggreTree-2
    (agg parent-node previous tree node-prototype)
    ;; (declare (ignore agg))
    (let ((node (or (pop (g-value agg :node-free-list))
		    (create-instance nil node-prototype
		      (:left (o-formula (compute-aggretree-2-node-left)))
		      (:top (o-formula (compute-aggretree-2-node-top)))
		      (:tree-height (o-formula (compute-aggretree-2-height)))))))
      (s-value node :previous previous)
      (s-value node :parent-node parent-node)
      (s-value node :tree tree)
      (values node)))


(define-method :destroy-me AggreTree-2 (agg &optional top-level-p)
  (dolist (node (g-value agg :node-free-list)) (opal:destroy node))
  (call-prototype-method agg top-level-p))



;;; Demos.
;;


(defun aggretree-go (&optional (kind AggreTree-1))
  (defun tree-children (tree)
    (if (consp tree) (cdr tree)))
  (defun tree-root (tree)
    (if (consp tree) (car tree) tree))
  (let* ((tree '(root (node-1 (node-2) (node-3 (node-8)))
		 (node-4 (node-6) (node-7)) (node-5)))
	 (agg (create-instance nil kind
		(:expansion-level 1)
		(:tree tree)
		(:line-style opal:dotted-line)
		(:children-function 'tree-children)
		(:node-prototype
		 (create-instance 'My-Tree-Node opal:text
		   (:string (o-formula (format nil "~S" (tree-root (gvl :tree)))))))
		)))
    (create-instance 'aggretree-vp inter:interactor-window
      (:left 10) (:top 10) (:width 500) (:height 200)
      (:aggregate agg))
    (opal:update aggretree-vp)))

(defun aggretree-stop ()
  (opal:destroy aggretree-vp))


(format t "
*** Execute (garnet-gadgets::aggretree-go) to see a demo.
*** (garnet-gadgets::aggretree-go garnet-gadgets:AggreTree-2) shows a different demo.
*** (garnet-gadgets::aggretree-demo) shows an interactive demo.
*** (garnet-gadgets::aggretree-stop) stops any of the demos.
")


(defun aggretree-demo ()
  (let ((agg
	 (create-instance nil AggreTree-2
	   (:children-function #'(lambda (ob) (kr::get-local-values ob :is-a-inv)))
	   (:expansion-level 1)
	   (:tree opal:line)
	   (:line-style opal:dotted-line)
	   (:node-prototype
	    (create-instance 'My-Tree-Node opal:text
	      (:string (o-formula
			(let* ((info (gvl :tree))
			       (children (kr::get-local-values info :is-a-inv)))
			  (if (and (zerop (gvl :expansion-level))
				   children)
			      (format nil "~S ...(~S)" info (length children))
			      (format nil "~S" info)))))))
	   (:interactors
	    `((:selection-inter ,inter:button-interactor
	       (:window ,(o-formula (gv-local :self :operates-on :window)))
	       (:start-where ,(o-formula (list :element-of
					       (gvl :operates-on :nodes-aggregate))))
	       (:start-event :leftdown)
	       (:final-function
		,#'(lambda (inter ob)
		     (let ((level (g-value ob :expansion-level)))
		       (s-value ob :expansion-level (if (> level 0) 0 1))
		       (revert-aggretree-node (g-value ob :parent :parent) ob))))))))))
    (create-instance 'aggretree-vp inter:interactor-window
      (:left 10) (:top 10) (:width 500) (:height 600)
      (:aggregate agg))
    (opal:update aggretree-vp))
  (format t "
*** Start (inter:main-event-loop) if you need to.
*** Click left on nodes to expand/shrink them."))

#||----------------------------------------------------------------------------

Test

(aggretree-go)
(aggretree-go AggreTree-2)
(aggretree-demo)
(aggretree-stop)


(setf tree '(root (node-1 (node-2) (node-3)) (node-4) (node-5)))
(setf tree1 '(root (node-1 (node-2) (node-3 (node-8))) (node-4 (node-6) (node-7)) (node-5)))

(defun tree-children (tree)
  (if (consp tree) (cdr tree)))

(defun tree-root (tree)
  (if (consp tree) (car tree) tree))

(create-instance 'My-Tree-Node opal:text
  (:string (o-formula (format nil "~S" (tree-root (gvl :tree)))))
  )

(setf agg (create-instance nil AggreTree-1
	    (:tree tree)
	    (:line-style opal:dotted-line)
	    (:children-function 'tree-children)
	    (:node-prototype My-Tree-Node)
	    ))

(setf agg1 (create-instance nil AggreTree-2
	    (:lines-p nil)
	    (:tree tree)
	    (:line-style opal:dotted-line)
	    (:children-function 'tree-children)
	    (:node-prototype My-Tree-Node)
	    ))

(defun select-node-ff (inter ob)
  (let ((level (g-value ob :expansion-level)))
    (s-value ob :expansion-level (if (> level 0) 0 1))
    (revert-aggretree-node (g-value ob :parent :parent) ob)
    (opal:update (g-value ob :window) T)))


(create-instance 'vp inter:interactor-window
  (:left 10) (:top 10) (:width 300) (:height 200)
  (:aggregate agg))

(s-value agg :v-spacing 5)
(s-value agg :line-style opal:line-4)
(s-value agg :sub-tree-indent 4)
(s-value agg :tree tree1)
(revert-aggretree agg)

(opal:update vp)
(opal:destroy vp) 
(s-value KR-DEBUG:MY-TREE-NODE-12915 :dont-expand-children nil)
(s-value KR-DEBUG:MY-TREE-NODE-11581 :tree tree1)
(revert-aggretree-node agg KR-DEBUG:MY-TREE-NODE-12915)
----------------------------------------------------------------------------||#
