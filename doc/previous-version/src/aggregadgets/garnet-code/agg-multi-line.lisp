(in-package "USER" :use '("LISP" "KR" "GARNET-DEBUG"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
   (:left 700)(:top 10)(:width 230)(:height 110))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate
			  (:left 10) (:top 10)
			  (:width 200) (:height 200)))
(opal:update WIN)

(defun Make-Lines (lines-agg)
  (let ((lines NIL))
    (dolist (line-ends (g-value lines-agg :lines-end-points))
      (setf lines (cons (create-instance NIL opal:line
			   (:x1 (first line-ends))
			   (:y1 (second line-ends))
		           (:x2 (third line-ends))
			   (:y2 (fourth line-ends)))
			lines)))
    (values (reverse lines) (g-value lines-agg :lines-names))))

(create-instance 'MY-MULTI-LINE1 opal:aggregadget
   (:parts `(,#'Make-Lines))
   (:lines-end-points '((10 10 100 100)
                        (10 100 100 10)
                        (55 10 55 100)
                        (10 55 100 55)))
   (:lines-names
    '(:down-diagonal :up-diagonal :vertical :horizontal)))

(create-instance 'MY-MULTI-LINE2 opal:aggregadget
   (:parts `(,#'Make-Lines))
   (:lines-end-points '((120 100 170 10)
                        (170 10 220 100)
                        (220 100 150 100))))
   
(opal:add-components TOP-AGG MY-MULTI-LINE1 MY-MULTI-LINE2)

(opal:update WIN)