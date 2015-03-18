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
;;; scalable-aggregraph-image.lisp
;;; Requires the files "rectangle-conflict-object.lisp", "aggregraphs.lisp",
;;; and "scalable-aggregraph".
;;;
;;; Written by A. Bryan Loyall
;;; Winter 1990-1991
;;;
;;; $Id::                                                             $



(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(SCALABLE-AGGREGRAPH-IMAGE SCALABLE-AGGREGRAPH-IMAGE-NODE-PROTOTYPE
	    SCALABLE-AGGREGRAPH-IMAGE-LINK-PROTOTYPE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; par-assoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a function which takes two lists rather than an association list.
;; It uses the first list as a list of keys and the second as a list
;; of values.  It then finds the value in the value listwhich is at
;; the same index as the first matching key in keylist.
(defun par-assoc (key keylist vallist)
  (cond
    ((null keylist) nil)
    ((eql (car keylist) key) (car vallist))
    (t (par-assoc key (cdr keylist) (cdr vallist)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default node prototype
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Is a roundtangle.  Many slots are set with formulas/values when the
;; instances are created in the graph.
(kr:create-instance 'scalable-aggregraph-image-node-prototype opal:roundtangle
   ;; set by initialize function in instances
   (:corresponding-node nil)
   (:left 0)
   (:top 0)
   (:width 0)
   (:height 0)
   ;; graphics stuff
   (:filling-style opal:white-fill)
   (:radius 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default link prototype
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Is a line.  Many slots are set with formulas/values when the
;; instances are created in the graph.
(kr:create-instance 'scalable-aggregraph-image-link-prototype opal:line
   ;; set by initialize function in instances
   (:corresponding-link nil)
;  (:x1 nil)
;  (:y1 nil)
;  (:x2 nil)
;  (:y2 nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scalable-aggregraph-image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This object is a dynamic image of an existing aggregraph (standard,
;; scalable, or image).  Changes to the original aggregraph are
;; reflected in the image.  Changes to the image such as adding or
;; deleting nodes or links, or laying out the graph, are actually
;; performed on the original aggregraph and reflected in the image.
;; When creating a scalable-aggregraph-image, the :source-aggregraph
;; must be given an existing aggregraph, scalable-aggregraph, or
;; scalable-aggregraph-image.
(kr:create-instance 'scalable-aggregraph-image opal:aggregadget
                       
   ;; must be specified in created instance to actually create graph

   (:source-aggregraph nil)

   ;; may be specified in created instance if desired

   (:left 0)
   (:top 0)
   (:desired-height nil)
   (:desired-width nil)
   (:scale-factor nil)

   (:node-prototype scalable-aggregraph-image-node-prototype)
   (:link-prototype scalable-aggregraph-image-link-prototype)
   (:node-prototype-selector-function nil)
   (:link-prototype-selector-function nil)
   ;(:interactors nil) ;; inherited from aggregadget

   ;; methods (probably shouldn't be overridden)

   (:layout-graph #'(lambda (graph)
                      (let ((sgraph (kr:g-value graph :source-aggregraph)))
                        (kr:kr-send sgraph :layout-graph sgraph))))
   (:delete-link #'(lambda (graph link)
                     (let ((sgraph (kr:g-value graph :source-aggregraph))
                           (slink (kr:g-value link :corresponding-link)))
                       (kr:kr-send sgraph :delete-link sgraph slink))))
   (:delete-node #'(lambda (graph node)
                     (let ((sgraph (kr:g-value graph :source-aggregraph))
                           (snode (kr:g-value node :corresponding-node)))
                       (kr:kr-send sgraph :delete-node sgraph snode))))
   (:add-node #'(lambda (graph source-node parent-g-nodes children-g-nodes)
                  (let ((sgraph (kr:g-value graph :source-aggregraph))
                        (sparent-g-nodes
                         (mapcar #'(lambda (n)
                                     (kr:g-value n :corresponding-node))
                                 parent-g-nodes))
                        (schildren-g-nodes
                         (mapcar #'(lambda (n)
                                     (kr:g-value n :corresponding-node))
                                 children-g-nodes)))
                    (kr:kr-send sgraph :add-node sgraph source-node
                                sparent-g-nodes schildren-g-nodes))))
   (:add-link #'(lambda (graph from to)
                  (let ((sgraph (kr:g-value graph :source-aggregraph))
                        (sfrom (kr:g-value from :corresponding-node))
                        (sto (kr:g-value to :corresponding-node)))
                    (kr:kr-send sgraph :add-link sgraph sfrom sto))))
   (:make-root #'(lambda (graph node)
                   (let ((sgraph (kr:g-value graph :source-aggregraph))
                         (snode (kr:g-value node :corresponding-node)))
                     (kr:kr-send sgraph :make-root sgraph snode))))
   (:remove-root #'(lambda (graph node)
                     (let ((sgraph (kr:g-value graph :source-aggregraph))
                           (snode (kr:g-value node :corresponding-node)))
                       (kr:kr-send sgraph :remove-root sgraph snode))))
   (:source-to-graph-node
         #'(lambda (graph source-node)
             (let* ((sgraph (kr:g-value graph :source-aggregraph))
                    ;; get graph node from source-graph
                    (snode  (kr:kr-send sgraph :source-to-graph-node
                                        sgraph source-node))
                    ;; get list of image nodes of this graph node (our
                    ;; node is one of them)
                    (node-list (kr:g-value snode :image-nodes))
                    ;; get the list of image graphs (this graph is one
                    ;; of them)
                    (graph-list (kr:g-value sgraph :image-graphs)))
               ;; look down graph-list and node-list.  when we find
               ;; the current graph, the corresponding node is the
               ;; node in this graph that corresponds to the source-node.
               (par-assoc graph graph-list node-list))))
   (:find-link #'(lambda (graph from to)
                   (let* ((sgraph (kr:g-value graph :source-aggregraph))
                          (sfrom (kr:g-value from :corresponding-node))
                          (sto (kr:g-value to :corresponding-node))
                          (slink (kr:kr-send sgraph :find-link sfrom sto))
                          ;; get list of image links of this graph link (our
                          ;; link is one of them)
                          (link-list (kr:g-value slink :image-links))
                          ;; get the list of image graphs (this graph is one
                          ;; of them)
                          ;; look down graph-list and link-list.  when we find
                          ;; the current graph, the corresponding link is the
                          ;; link in this graph that corresponds to
                          ;; the source-link. 
                          (graph-list (kr:g-value sgraph :image-graphs)))
                     (par-assoc graph graph-list link-list))))

   ;; primitive methods (only called by aggregraph family objects to
   ;; correctly manage any scalable-aggregraph-image objects.)
   ;; Actually executes the delete, add, etc. operations on this
   ;; object.  All of the arguments to these functions must be nodes,
   ;; links, etc. of this graph.
   (:primitive-delete-link #'(lambda (graph link)
                               ;; first delete any image links of this one
                               (mapc #'(lambda (image-graph image-link)
                                         (kr:kr-send image-graph
                                                     :primitive-delete-link
                                                     image-graph image-link))
                                     (kr:g-value graph :image-graphs)
                                     (kr:g-value link :image-links))
                               ;; now destory this one.
                               (let ((link-agg (kr:g-value graph :links)))
                                 (opal:remove-component link-agg link)
                                 (opal:destroy link))))
   (:primitive-delete-node #'(lambda (graph node) 
                               ;; first delete any image nodes of this one
                               (mapc #'(lambda (image-graph image-node)
                                         (kr:kr-send image-graph
                                                     :primitive-delete-node
                                                     image-graph image-node))
                                     (kr:g-value graph :image-graphs)
                                     (kr:g-value node :image-nodes))
                               ;; now delete this one.
                               (let ((node-agg (kr:g-value graph :nodes)))
                                 (opal:remove-component node-agg node)
                                 (opal:destroy node))))
   (:primitive-add-node
       #'(lambda (graph full-size-node)
           ;; create the node in this graph
           (let ((node (make-scalable-aggregraph-node full-size-node graph)))
             ;; add image-node to any image-graphs of this graph
             (mapc #'(lambda (image-graph)
                       (kr:kr-send image-graph
                                   :primitive-add-node
                                   image-graph
                                   node))
                   (kr:g-value graph :image-graphs)))))
   (:primitive-add-link
       #'(lambda (graph full-size-link)
           ;; create the link in this graph
           (let ((link (make-scalable-aggregraph-link full-size-link graph)))
             ;; add link to any image graphs
             (mapc #'(lambda (image-graph)
                       (kr:kr-send image-graph
                                   :primitive-add-link
                                   image-graph
                                   link))
                   (kr:g-value graph :image-graphs)))))

   ;; read-only slots

   (:nodes nil) ; set by :initialize
   (:links nil) ; set by :initialize
   (:image-graphs nil)
   
   ;; internal, implementation-dependent slots
   
   (:internal-scale-factor
    (kr:o-formula
     (let ((sf (kr:gvl :scale-factor))
           (dh (kr:gvl :desired-height))
           (dw (kr:gvl :desired-width)))
       (cond
         ((numberp sf) sf)
         ((and (numberp dh) (numberp dw))
          (min (/ dh (kr:gvl :source-aggregraph :height))
               (/ dw (kr:gvl :source-aggregraph :width))))
         (t 1)))))
   (:internal-node-prototype nil) ; set by :initialize
   (:internal-link-prototype nil) ; set by :initialize
   )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions to make
;; internal prototypes
;;;;;;;;;;;;;;;;;;;;;;;;;
;; these function takes the user provided prototypes and create new
;; prototypes with appropriate formulas to make them adapt to the form
;; and shape of the original graph.
(defun create-internal-image-node-prototype (prototype)
  (kr:create-instance nil prototype
     (:graph (kr:o-formula (kr:gvl :parent :parent)))
     (:scale-factor (kr:o-formula (kr:gvl :graph :internal-scale-factor)))
     (:left (kr:o-formula
             (+ (floor (* (- (kr:gvl :corresponding-node :left)
                             (kr:gvl :graph :source-aggregraph :left))
                          (kr:gvl :scale-factor)))
                (kr:gvl :graph :left))))
     (:top (kr:o-formula
            (+ (floor (* (- (kr:gvl :corresponding-node :top)
                            (kr:gvl :graph :source-aggregraph :top))
                         (kr:gvl :scale-factor)))
               (kr:gvl :graph :top))))
     (:width (kr:o-formula (floor (* (kr:gvl :corresponding-node :width)
                                     (kr:gvl :scale-factor)))))
     (:height (kr:o-formula (floor (* (kr:gvl :corresponding-node :height)
                                      (kr:gvl :scale-factor)))))))

(defun create-internal-image-link-prototype (prototype)
  (kr:create-instance nil prototype
     (:graph (kr:o-formula (kr:gvl :parent :parent)))
     (:scale-factor (kr:o-formula (kr:gvl :graph :internal-scale-factor)))
     (:x1 (kr:o-formula
           (+ (floor (* (- (kr:gvl :corresponding-link :x1)
                           (kr:gvl :graph :source-aggregraph :left))
                        (kr:gvl :scale-factor)))
              (kr:gvl :graph :left))))
     (:y1 (kr:o-formula
           (+ (floor (* (- (kr:gvl :corresponding-link :y1)
                           (kr:gvl :graph :source-aggregraph :top))
                        (kr:gvl :scale-factor)))
              (kr:gvl :graph :top))))
     (:x2 (kr:o-formula
           (+ (floor (* (- (kr:gvl :corresponding-link :x2)
                           (kr:gvl :graph :source-aggregraph :left))
                        (kr:gvl :scale-factor)))
              (kr:gvl :graph :left))))
     (:y2 (kr:o-formula
           (+ (floor (* (- (kr:gvl :corresponding-link :y2)
                           (kr:gvl :graph :source-aggregraph :top))
                        (kr:gvl :scale-factor)))
              (kr:gvl :graph :top))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize function for
;; scalable-aggregraph-image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(kr:define-method :initialize scalable-aggregraph-image (self &rest args)
 (declare (ignore args))
 (kr:call-prototype-method self)
 (when (kr:g-value self :source-aggregraph)
   (let ((source-graph (kr:g-value self :source-aggregraph))
         (node-agg (kr:create-instance nil opal:aggregate))
         (link-agg (kr:create-instance nil opal:aggregate)))
     ;; need test that :source-aggregraph contains an aggregraph,
     ;; scalable-aggregraph, or scalable-aggregraph-image object.
     (kr:s-value self :nodes node-agg)
     (kr:s-value self :links link-agg)
     (opal:add-components self node-agg link-agg)
     ;; create internal prototypes
     (let ((node-prototype (kr:g-value self :node-prototype))
           (link-prototype (kr:g-value self :link-prototype)))
       (kr:s-value self :internal-node-prototype
                   (if (listp node-prototype)
                       (mapcar #'create-internal-image-node-prototype node-prototype)
                       (create-internal-image-node-prototype node-prototype)))
       (kr:s-value self :internal-link-prototype
                   (if (listp link-prototype)
                       (mapcar #'create-internal-image-link-prototype link-prototype)
                       (create-internal-image-link-prototype link-prototype)))
       ;; test that if multiple prototypes are included that a selector
       ;; function has been provided.
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
                     #'(lambda (corresponding-link l)
                         (declare (ignore corresponding-link))
                         (car l)))))
     ;; create nodes and links
     (opal:do-components (kr:g-value source-graph :nodes)
       #'(lambda (snode)
           (make-scalable-aggregraph-node snode self)))
     (opal:do-components (kr:g-value source-graph :links)
       #'(lambda (slink)
           (make-scalable-aggregraph-link slink self)))
     ;; add image to source graph's image list.
     (push self (kr:g-value source-graph :image-graphs)))))

(defun make-scalable-aggregraph-node (full-size-node image-graph)
  (let* ((node-agg (kr:g-value image-graph :nodes))
         (prototype-arg (kr:g-value image-graph :internal-node-prototype))
         (prototype ;; if the user gave a list of prototypes then
                    ;; choose one otherwise just use the prototype
                    (if (listp prototype-arg)
                        (funcall (kr:g-value image-graph
                                             :node-prototype-selector-function)
                                 full-size-node
                                 prototype-arg)
                        prototype-arg))
         (node (kr:create-instance nil prototype
                                   (:corresponding-node full-size-node))))
    ;; add image to full-sized node's image list
    (push node (kr:g-value full-size-node :image-nodes))
    ;; add node to graph
    (opal:add-component node-agg node)
    node))

(defun make-scalable-aggregraph-link (full-size-link image-graph)
  (let* ((link-agg (kr:g-value image-graph :links))
         (prototype-arg (kr:g-value image-graph :internal-link-prototype))
         (prototype ;; if the user gave a list of prototypes then
                    ;; choose one otherwise just use the prototype
                    (if (listp prototype-arg)
                        (funcall (kr:g-value image-graph
                                             :link-prototype-selector-function)
                                 full-size-link
                                 prototype-arg)
                        prototype-arg))
         (link (kr:create-instance nil prototype
                                   (:corresponding-link full-size-link))))
    ;; add image to full-sized link's image list
    (push link (kr:g-value full-size-link :image-links))
    ;; add link to graph
    (opal:add-component link-agg link)))
