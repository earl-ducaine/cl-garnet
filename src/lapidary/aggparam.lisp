;;; CHANGE LOG
;;;
;;; 08/24/92 amickish - Removed declaration of comp from let* in AggListParam

(in-package "LAPIDARY")

#|

This function assumes that the secondary  selection is an agglist
and that the primary selection is a child of that agglist.  This function
takes a list of slots in the primary selection.  The slots  should
get their values from the items slot in the agglist.  It is also assumed
that the primary selection is not a member of any other agglist either 
directly or indirectly. 

The items and items-info slots in the agglist will be updated
also formulas will be installed in the item-prototype-object
to access the :items slot

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun select2 (components comp2select sel2a)
 (let* (
  (comp          (first components))
  (obj           (eval `(g-value ,comp ,@comp2select)))
  )
 
 (loop (when (member obj components) (return sel2a))
   (push :PARENT sel2a)
   (setf obj (g-value obj :PARENT)))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun AggListParam (slots)
"Update primary selection prototype to use formulas on the named slots
 first their current values are saved in :ITEMS and then
 formulas are created to get the slot value based on from :ITEMS "

 (let* (
  (selected   (first (g-value lapidary::*selection-info* :p-selected)) )
  (obj        selected)
  (agglist    (first (g-value lapidary::*selection-info* :s-selected)) )
  (components (reverse (g-value agglist :COMPONENTS)) )
  (items-info (g-value agglist :items-info))
  (items      (g-value agglist :items))
  (i-proto-o  (g-value agglist :item-prototype-object))
  proto      
  index 
  form
  clist
  slot
  rec
  oldslots
  select2items ; path from selected to agglist items
  select2rank  ; path from selected to instance rank
  (comp2select  nil)
  )

   ;; deselect the primary selection--it will be destroyed when the :items
   ;; list is changed
   (primary-deselect selected)

  ; comp2select  path from agglist component to primary selected
  (loop (when  (member obj components) (return))
   (push (g-value obj :KNOWN-AS) comp2select)
   (setf obj (g-value obj :PARENT)))

  ; find  slots that had formulas last time, but do not now.
  ; destory the slots so that values will be inherited
  (setf proto    (eval `(g-value ,i-proto-o ,@comp2select)))
  (setf rec      (assoc comp2select items-info :test #'equal)) 
  (setf oldslots (second rec))
  (dolist (slot oldslots)
   (unless (member slot slots)
    (destroy-slot proto slot)))

  ;install new slot list in :items-info
  (cond
   ((null slots)
    ; no slots set in selected object
    (setf items-info 
	  (delete comp2select items-info :test #'equal :key #'FIRST) )
    (s-value agglist :items-info items-info)) 
   ((null rec)
    ; first time selected object has formulas  
    (setf items-info (append items-info `((,comp2select ,slots))))
    (s-value agglist :items-info items-info)) 
   (t
    ; selected objects already in assoc list, just replace slots list
    (setf (second rec) slots)
    (s-value agglist :items-info items-info)))

  ; build items lists and formual for three cases
  (cond 
    ((null items-info) 
     ; there are no formulas, items should be a number
     (unless (numberp items)
      (s-value agglist :items (length items))))
    ((and (= (length items-info) 1) 
	  (= (length (second (first items-info))) 1))
     ; there is only one value comming from items for each component 
     (setf items nil)
     (setf comp2select (FIRST  (FIRST items-info)))
     (setf slot    (FIRST (second (first items-info))))
     (dolist (comp components)
      (push (eval `(g-value ,comp ,@comp2select ,slot)) items) )
     (s-value agglist :items items)
     (setf select2rank  (select2 components comp2select '(:rank)))
     (setf select2items (select2 components comp2select '(:PARENT :ITEMS)))
     (setf form (formula `(nth (gvl ,@select2rank) (gvl ,@select2items))))
     ;; reset proto to point to the one remaining object on items-info
     (setf proto (eval `(g-value ,i-proto-o ,@(first (first items-info)))))
     (s-value proto slot form)); install formula in prototype
    (t
     ; each component has multiple values comming from items
     ; create items list
     (setf items nil)
     (dolist (comp components)
      (setf clist nil)
      (dolist (alist items-info)
       (setf comp2select (FIRST alist))
       (setf slots (SECOND alist))
       (dolist (slot slots)
        (push (eval `(g-value ,comp ,@comp2select ,slot)) clist)))
      (push (reverse clist) items))
     (s-value agglist :items items)
     ; create formulas
     (setf index 0)
     (dolist (alist items-info)
      (setf comp2select (FIRST alist))
      (setf slots (SECOND alist))
      (setf select2rank  (select2 components comp2select '(:rank)))
      (setf select2items (select2 components comp2select '(:PARENT :ITEMS)))
      (setf proto (eval `(g-value ,i-proto-o ,@comp2select)))
      (dolist (slot slots)
       (setf form (formula 
           `(nth ,index (nth (gvl ,@select2rank) (gvl ,@select2items))) ))
       (s-value proto slot form) ; install formula in prototype
       (setf index (1+ index))))))))

