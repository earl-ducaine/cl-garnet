;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;

;;; Helper file for aggregraphs.lisp
;;  Written by A. Bryan Loyall
;;  Winter 1990-1991
;; 


;;; Change log:
;;   07/15/93  Andrew Mickish   - #+lcl3.0 ---> #+lucid
;;   12/06/91  Nikos Drakos     - Remove duplicates after storing the position
;;                                of a new node in aggregraph.
;;   07/08/91  Steven Berson    - Changed #+lcl4.0 switch to #+lcl3.0
;;   03/21/91  Pedro Szekely    - Added optimize declaration in
;;			          make-rectangle-conflict-object.
;;   03/11/91  Andrew Mickish   - Massaged into Opal/Aggregadgets format
;;   03/09/91  Bryan Loyall     - Created


(in-package "OPAL")

(defun between (v low high)
  "true if v is between low and high inclusive."
  (and (>= v low) (<= v high)))

(defun rectangle-conflictp (lowx1 highx1 lowy1 highy1 lowx2 highx2 lowy2 highy2)
  "determines if two rectangles overlap (not counting borders).  A
given rectangle is represented by four numbers: x1, x2, y1, y2,
which is interpretted to be the rectangle: {(x,y) | x1 <= x <= x2
and y1 <= y <= y2}"
  (labels (			; true if the two ranges [low1 high1] and [low2 high2] overlap.
           (range-conflict (low1 high1 low2 high2)
             (or (between low2 low1 high1)
                 (between high2 low1 high1)
                 (between low1 low2 high2))))
    (and (range-conflict lowx1 highx1 lowx2 highx2)
         (range-conflict lowy1 highy1 lowy2 highy2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-binary-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this function returns a binary-tree object.  This object stores
;; keys and other arbitrary information (except nil) according to the
;; order of the keys.  Nil is not a valid value for the associated
;; arbitrary information.
;; key= and key< are functions (lambda (key1 key2) ...)
;; which return t if key1 is equal or less than respectively to key2.
;; info-item= is a similar function for info-items (used when deleting
;; items from info-lists.)
;; The object has the following functions (notice that three arguments
;; must always be passed in): 
;;  - (obj 'store key info-list nil) -- store key with info-list in obj.
;;         If there are other values already stored at key in the tree
;;         info-list is appended to the list.
;;  - (obj 'get key nil nil) -- get matching key(s).  if key matches
;;         any key* with a non-empty info-list in obj, it returns
;;         (key* .  info-list) as both values.  Otherwise it returns
;;         (key1 .  info-list1) and (key2 . info-list2) such that key1
;;         is the closest key < key and key2 is the closest key > key
;;         with non-empty info-lists.  Either or both (key1 .
;;         info-list1) or (key2 .  info-list2) could be be nil if
;;         there is no key in obj < or > key respectively.
;;  - (obj 'get-info key nil nil) -- same as 'get above except the 
;;         info-list(s) ar returned instead of (key . info-list).
;;  - (obj 'delete-key key nil nil) -- deletes key from the object
;;  - (obj 'delete-info-item info-item lowkey highkey) or
;;    (obj 'delete-info-item info-item nil nil) --
;;         deletes info-item (tested by eql) from all info-lists.  If
;;         lowkey and highkey are given, info-item is only deleted in
;;         nodes with keys between lowkey and highkey inclusive.
;;  - (obj 'add-info-item info-item lowkey highkey) or
;;    (obj 'add-info-item info-item nil nil) --
;;         adds info-item to all info-lists.  If lowkey and highkey
;;         are given, info-item is only added to nodes with keys
;;         between lowkey and highkey inclusive.

;; the internal data structures for this object are nodes which are
;; connected both as binary search trees and as linked lists.  This
;; should properly be inside the definition of make-binary-tree since
;; it is never used external to that object.
(defstruct node
  ;; info
  key (info-list nil)
  ;; link-list links
  prev next
  ;; binary sort tree links
  left-child right-child parent)

;; object itself
(defun make-binary-tree (key= key< info-item=)
  (let* ((empty (gensym))
         ;; hack for speed.  if store is called right after get, the
         ;; place to store is cached here.
         (last-key-got empty)
         (last-closest-node empty))
    (labels (
             ;; do-tree calls the function on all nodes of the tree
             ;; rooted at tree.
             (do-tree (fun tree)
               (if (null tree) '()
                   (progn
                     (funcall fun tree)
                     (do-tree fun (node-left-child tree))
                     (do-tree fun (node-right-child tree)))))
             ;; do-linked-list calls the function on all nodes between
             ;; low-node and high-node inclusive.  (nil can be passed
             ;; in for high-node to do it till the end of the list.)
             (do-linked-list (fun low-node high-node) 
                (cond
                 ((null low-node) '())
                 ((eq low-node high-node) (funcall fun low-node))
                 (t (funcall fun low-node)
                    (do-linked-list fun (node-next low-node) high-node))))
             ;; does a normal binary tree find.
             (binary-tree-find (key tree)
               (cond
                ((eql (node-key tree) empty) tree)
                ((funcall key= (node-key tree) key) tree)
                ((funcall key< (node-key tree) key)
                 (let ((right-child (node-right-child tree)))
                   (if right-child
                       (binary-tree-find key right-child)
                       tree)))
                ((funcall key< key (node-key tree))
                 (let ((left-child (node-left-child tree)))
                   (if left-child
                       (binary-tree-find key left-child)
                       tree)))
                (t (format t "~&Error ~A and ~A do not partition the space"
                           key= key<)
                   (format t " of keys passed to bin-tree-object.")
                   tree)))

             ;; get-prev takes a tree and follows prev pointers until
             ;; it finds a node with a non nil info-list and returns
             ;; it.  If it doesn't find one it returns nil.
             (get-prev (node)
               (let ((prev (node-prev node)))
                 (cond
                  ((null prev) nil)
                  ((not (null (node-info-list prev))) prev)
                  (t (get-prev prev)))))

             ;; get-next does the same as get-prev only following next
             ;; pointers.
             (get-next (node)
               (let ((next (node-next node)))
                 (cond
                  ((null next) nil)
                  ((not (null (node-info-list next))) next)
                  (t (get-next next)))))

             ;; gets closest node with non-nil info list on both sides
             ;; of key using closest node (given by binary-tree-find).
             (closest-nodes (key closest-node)
               (cond
                ((eql (node-key closest-node) empty)
                 (values nil nil))
                ((funcall key= (node-key closest-node) key)
                 (if (node-info-list closest-node)
                     (values closest-node closest-node)
                     (values (get-prev closest-node) (get-next closest-node))))
                ((funcall key< (node-key closest-node) key)
                 (if (node-info-list closest-node)
                     (values closest-node (get-next closest-node))
                     (values (get-prev closest-node) (get-next closest-node))))
                ((funcall key< key (node-key closest-node))
                 (if (node-info-list closest-node)
                     (values (get-prev closest-node) closest-node)
                     (values (get-prev closest-node) (get-next closest-node))))
                (t (format t "~&Error in key= and key< functions in")
                   (format t "binary-tree object.  They do not partition the")
                   (format t "space of key values.~&")
                   (values nil nil))))
             )
    (let ((tree (make-node :key empty)))
      #'(lambda (op arg1 arg2 arg3)
          (case op
            (store (let ((node-to-insert-under
                          (if (or (eq last-key-got empty)
                                  (not (funcall key= arg1 last-key-got)))
                              (binary-tree-find arg1 tree)
                              last-closest-node)))
                     (cond
                      ((eql (node-key node-to-insert-under) empty)
                       (setf (node-key node-to-insert-under) arg1)
                       (setf (node-info-list node-to-insert-under) arg2))
                      ((funcall key= (node-key node-to-insert-under) arg1)
                       (setf (node-info-list node-to-insert-under)
			     (remove-duplicates
                               (append arg2
                                     (node-info-list node-to-insert-under)))))
                      ((funcall key< (node-key node-to-insert-under) arg1)
                       ;; insert-node
                       (let ((prev node-to-insert-under)
                             (next (node-next node-to-insert-under)))
                         (let ((new-node
                                (make-node :key arg1
                                           :info-list arg2 
                                           :prev prev
                                           :next next
                                           :left-child nil
                                           :right-child nil)))
                           ;; insert in tree
                           (setf (node-right-child node-to-insert-under)
                                 new-node)
                           ;; insert in linked-list
                           (setf (node-next prev) new-node)
                           (and next (setf (node-prev next) new-node)))))
                      ((funcall key< arg1 (node-key node-to-insert-under))
                       (let ((prev (node-prev node-to-insert-under))
                             (next node-to-insert-under))
                         (let ((new-node
                                (make-node :key arg1
                                           :info-list arg2
                                           :prev prev
                                           :next next
                                           :left-child nil
                                           :right-child nil)))
                           ;; insert in tree
                           (setf (node-left-child node-to-insert-under)
                                 new-node)
                           ;; insert in linked-list
                           (and prev (setf (node-next prev) new-node))
                           (setf (node-prev next) new-node))))
                      (t (format t "~&test partition error in store of")
                         (format t "bin tree object."))
                      )
                     (setf last-key-got empty) ;; flush cache
                     ))
            (get
             (let ((closest-node (binary-tree-find arg1 tree)))
               ;; hack for speed (see above).
               (setf last-key-got arg1)
               (setf last-closest-node closest-node)
               (multiple-value-bind (node1 node2)
                                    (closest-nodes arg1 closest-node)
                 (values
                  (and node1 (cons (node-key node1) (node-info-list node1)))
                  (and node2 (cons (node-key node2) (node-info-list node2))))
                 )))
            (get-info
             (let ((closest-node (binary-tree-find arg1 tree)))
               ;; hack for speed (see above).
               (setf last-key-got arg1)
               (setf last-closest-node closest-node)
               (multiple-value-bind (node1 node2)
                                    (closest-nodes arg1 closest-node)
                 (values (and node1 (node-info-list node1))
                         (and node2 (node-info-list node2)))
                 )))
            (delete-key
             (let ((node (if (or (eq last-key-got empty)
                                 (funcall key= arg1 last-key-got))
                             (binary-tree-find arg1 tree)
                             last-closest-node)))
               (cond ((funcall key= (node-key node) arg1)
                      (setf (node-info-list node) nil))
                     (t (format t "~&Tried to delete non-existent")
                        (format t " key in object")))
               ;; since we don't physically delete the node, the cache is
               ;; still correct for the purposes of store and
               ;; delete-key (the only two methods which use it.)
               ;(setf last-key-got empty) ;; flush cache
               ))
            (delete-info-item
             (labels ((delete-info (node)
                        (setf (node-info-list node)
                              (remove arg1 (node-info-list node)
                                      :test info-item=))))
               (if arg2
                   ;; delete between keys inclusive
                   (let ((lownode (binary-tree-find arg2 tree))
                         (highnode (and arg3 (binary-tree-find arg3 tree))))
                     (if (and (funcall key= (node-key lownode) arg2)
                              (or (null highnode)
                                  (funcall key= (node-key highnode) arg3)))
                         (do-linked-list #'delete-info lownode highnode)
                         (progn
                           (format t "~&Error:  in binary-tree-object.  Tried")
                           (format t " to delete info-item ~A between " arg1)
                           (format t "keys ~A and ~A.  " arg2 arg3)
                           (format t "There is no node for at least one of ")
                           (format t "these keys."))))
                   ;; otherwise delete from whole tree
                   (do-tree #'delete-info tree)))
             ;; since we don't physically remove the node (if the info
             ;; list is empty), the cache is still current for the
             ;; purposes of store and delete-key (the only two
             ;; methods which use it.)
             ;(setf last-key-got empty) ;; flush cache
             )
            (add-info-item
             (labels ((add-info (node)
                        (push arg1 (node-info-list node))))
               (if arg2
                   ;; add between the keys inclusive
                   (let ((lownode (binary-tree-find arg2 tree))
                         (highnode (and arg3 (binary-tree-find arg3 tree))))
                     (if (and (funcall key= (node-key lownode) arg2)
                              (or (null highnode)
                                  (funcall key= (node-key highnode) arg3)))
                         (do-linked-list #'add-info lownode highnode)
                         (progn
                           (format t "~&Error:  in binary-tree-object.  Tried")
                           (format t " to add info-item ~A between " arg1)
                           (format t "keys ~A and ~A.  " arg2 arg3)
                           (format t "There is no node for at least one of ")
                           (format t "these keys."))))
                   ;; otherwise add for the whole tree.
                   (do-tree #'add-info tree))))
            (else (format t "Error:  invalid method call to binary tree ")
                  (format t "object"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-rectangle-conflict-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this function returns a rectangle-conflict-object.  A
;; rectangle-conflict-object is a self modifying information object.
;; When called as:
;;     (funcall <rectangle-conflict-object> x1 x2 y1 y2 nil),
;; it takes a rectangle and returns whether the rectangle overlaps
;; with any previously stored rectangles (see below for method of
;; storing.)
;;   - If the given rectangle overlaps with any previously stored
;;     rectangle, then the previously stored rectangle is returned as
;;     the list (x1 x2 y1 y2).
;;   - If the given rectangle doesn't overlap with any previously
;;     stored rectangle, then nil is returned and the given rectangle
;;     is stored.  (This is the only way to store a rectangle in the object.)
;; Only rectangles orthogonal to the x and y axes are handled.  A
;; given rectangle is represented by four numbers:  x1, x2, y1, y2,
;; which is interpretted to be the rectangle:
;;     {(x,y) | x1 <= x <= x2 and y1 <= y <= y2}
;; If a non-nil fifth argument is passed in then the rectangle is
;; removed from the object instead (so it can not be returned for any
;; future calls.)  The result is then unspecified.
(defun make-rectangle-conflict-object ()
  "naive implementation
	(let ((rectangle-list '()))
	  (labels ((add-rectangle (x1 x2 y1 y2)
		     (push (list x1 x2 y1 y2) rectangle-list))
		   (remove-rectangle (x1 x2 y1 y2)
		     (setf rectangle-list
			   (remove (list x1 x2 y1 y2) rectangle-list :test #'equal)))
		   (get-potential-conflicts (x1 x2 y1 y2)
		     (declare (ignore x1 x2 y1 y2))
		     rectangle-list))
	    ))

Rectangles are denoted by four-tuples (x1 x2 y1 y2).  A given
four-tuple denotes the rectangle:
                 {(x,y) | (x1 <= x <= x2) & (y1 <= y <= y2)}.

This implementation uses two binary trees (one for the x axis and
one for the y axis).  Each binary tree node contains a coordinate
of the axis as its key associated with all of the rectangles that
overlaps that coordinate.   A seperate entry in the tree is made
for the lower and upper bounds on each axis that define the
rectangle.

With this structure maintained, the potential conflicts for a
rectangle (x1 x2 y1 y2) are only the rectangles that overlap
with coordinates closest to x1 or x2, intersected with the
rectangles that overlap with coordinates closest to y1 or y2.
These rectangles can then be checked in a linear fashion for
actual conflicts with the given rectangle."
  (let ((xtree (make-binary-tree #'= #'< #'equal))
        (ytree (make-binary-tree #'= #'< #'equal)))
    (labels ((rects-that-cross-coord (coord tree low-f high-f)
               (multiple-value-bind (poss-rect1 poss-rect2)
                                    (funcall tree 'get-info coord nil nil)
                 (let ((possible-overlapping-rectangles
                        (union poss-rect1 poss-rect2)))
                   (let ((overlapping-rectangles '()))
                     (dolist (rect possible-overlapping-rectangles)
                       (let ((low (funcall low-f rect))
                             (high (funcall high-f rect)))
                         (when (between coord low high)
                           (push rect overlapping-rectangles))))
                     overlapping-rectangles))))
             (rects-that-cross-x-coord (x)
               (rects-that-cross-coord x xtree #'car #'cadr))
             (rects-that-cross-y-coord (y)
               (rects-that-cross-coord y ytree #'caddr #'cadddr))
             (add-rectangle (x1 x2 y1 y2)
               ;; add the coordinates of the endpoints with all of the
               ;; rectangles that cross them.
               (funcall xtree 'store x1 (rects-that-cross-x-coord x1) nil)
               (funcall xtree 'store x2 (rects-that-cross-x-coord x2) nil)
               (funcall ytree 'store y1 (rects-that-cross-y-coord y1) nil)
               (funcall ytree 'store y2 (rects-that-cross-y-coord y2) nil)
               ;; add this rectangle to all of the applicable coordinates
               (let ((rectangle (list x1 x2 y1 y2)))
                 (funcall xtree 'add-info-item rectangle x1 x2)
                 (funcall ytree 'add-info-item rectangle y1 y2)))
             (remove-rectangle (x1 x2 y1 y2)
               (let ((rectangle (list x1 x2 y1 y2)))
                 ;; cannot delete coordinate keys from trees because
                 ;; other nodes may have those same endpoints.
                 (funcall xtree 'delete-info-item rectangle x1 x2)
                 (funcall ytree 'delete-info-item rectangle y1 y2)))
             (get-potential-conflicts (x1 x2 y1 y2)
               (multiple-value-bind (lowx1 highx1)
                                    (funcall xtree 'get-info x1 nil nil)
                 (multiple-value-bind (lowx2 highx2)
                                      (funcall xtree 'get-info x2 nil nil)
                   (multiple-value-bind (lowy1 highy1)
                                        (funcall ytree 'get-info y1 nil nil)
                     (multiple-value-bind (lowy2 highy2)
                                          (funcall ytree 'get-info y2 nil nil)
;                       (format t "~&lowx1, highx1, ...:  ~A"
;                               (list lowx1 highx1 lowx2 highx2 lowy1
;                                     highy1 lowy2 highy2))
                       (intersection (union (union lowx1 highx1)
                                            (union lowx2 highx2))
                                     (union (union lowy1 highy1)
                                            (union lowy2 highy2))))))))
             )

      #'(lambda (x1 x2 y1 y2 deletep)
;;;        (format t "~%In rect-conf-obj ~A." (list x1 x2 y1 y2 deletep))
;;;        (finish-output)
          (cond
            (deletep (remove-rectangle x1 x2 y1 y2))
            (t (labels ((conflictp (rect)
                          (apply #'rectangle-conflictp x1 x2 y1 y2 rect)))
                 (let* ((potential-conflicting-rectangles
                         (get-potential-conflicts x1 x2 y1 y2))
;;;                      (dummy (progn (format t "~&Potential conflicting-rectangles:  ~A"
;;;                                            potential-conflicting-rectangles)
;;;                                    (finish-output)))
                        (conflicting-rectangles
                         (member-if #'conflictp potential-conflicting-rectangles)))
                   (cond
                     (conflicting-rectangles (car conflicting-rectangles))
                     (t (add-rectangle x1 x2 y1 y2)
                        nil))))))))))
