;;;
;;; PostScript DAG Grapher  -- Joseph Bates, CMU CSD, March 1988
;;;

(defstruct psnode
  name
  info
  children
  parents
  appears-in-top-sort
  children-appear-in-top-sort
  yvalue
  height
)

(defvar max-psnodes 5000)
(defvar extra-x-spacing 20)
(defvar extra-y-spacing 6)
(defvar fontname "Helvetica")
(defvar fontsize 10)
(defvar second-fontname "Helvetica-Oblique")
(defvar second-fontsize 8)
(defvar boxgray ".2")      ;; 0 is black, 1 is white
(defvar boxkind "stroke")  ;; stroke for outline, fill for solid
(defvar edgewidth ".5")    ;; .5 is thin lines, 1 is fairly thick
(defvar edgegray ".2")     ;; dark, but not solid black, lines
(defvar edgecap "1")       ;; round the line caps
(defvar textgray "0")      ;; solid black text
(defparameter pageheight 300) ;;; 720)
(defparameter pagewidth 300) ;;; 540)
(defvar boxradius (floor fontsize 2))
(defvar boxedge (floor fontsize 4))
(defvar chunksize 400)

(defvar num-psnodes)
(defvar narray)
(defvar psnode-index)
(defvar child-function)
(defvar info-function)
(defvar top-sort)
(defvar maximum-y)
(defvar minimum-y)
(defvar rows)
(defvar y-offset)
(defvar psnodes-at-y)
(defvar ancestor-cache)

(defun psgraph (root childf infof &optional (shrink nil) (scribe-p nil))
  (setq narray (make-array max-psnodes))
  (setq num-psnodes 0)
  (setq psnode-index (make-hash-table :test #'equal
				    :size 500
				    :rehash-size 2.0))
  (setq child-function childf)
  (setq info-function infof)
  (setq ancestor-cache (make-hash-table :test #'equal
				    :size 1000
				    :rehash-size 2.0))

  ;;; walk the graph computing node info
  (walk-graph root)
  (dotimes (index num-psnodes)
    (setf (psnode-parents (aref narray index))
	  (nreverse (psnode-parents (aref narray index))))
    (setf (psnode-children (aref narray index))
	  (nreverse (psnode-children (aref narray index)))))

  ;;; topological sort the graph
  (setq top-sort nil)
  (top-sort-node 0)
  (setq top-sort (nreverse top-sort))

  ;;; declare this as a PostScript file
  (format t "%!PS-Adobe-1.0~%")

  ;;; this is required for the Apple LaserWriter (it likes to know the page
  ;;; ordering so that it can print pages upside down).  It is best not to
  ;;; confuse the LaserWriter when inserting things into a scribe document.
  (unless scribe-p
    (format t "%%Page: ? ?~%"))

  ;;; define global functions
  (dolist (line '(

     "/max {2 copy lt {exch} if pop} def"

     "/min {2 copy gt {exch} if pop} def"

     "/inch {72 mul} def"

     "/drawbox"
     " {/height exch def"
     "  /width exch def"
     "  /y exch def"
     "  /x exch def"
     "  gsave newpath"
     "  x y boxradius add moveto"
     "  x y height add x width add y height add boxradius arcto pop pop pop pop"
     "  x width add y height add x width add y boxradius arcto pop pop pop pop"
     "  x width add y x y boxradius arcto pop pop pop pop"
     "  x y x y height add boxradius arcto pop pop pop pop"
     "  boxgray setgray boxkind grestore"
     " } def"

     "/printposter"
     " {/rows exch def"
     "  /columns exch def"
     "  /bigpictureproc exch def"
     "  newpath"
     "    leftmargin botmargin moveto"
     "    0 pageheight rlineto"
     "    pagewidth 0 rlineto"
     "    0 pageheight neg rlineto"
     "  closepath clip"
     "  leftmargin botmargin translate"
     "  0 1 rows 1 sub"
     "   {/rowcount exch def"
     "    0 1 columns 1 sub"
     "     {/colcount exch def"
     "      gsave"
     "       pagewidth colcount mul neg"
     "       pageheight rowcount mul neg"
     "       translate"
     "       bigpictureproc"
     "       gsave showpage grestore"
     "      grestore"
     "     } for"
     "   } for"
     " } def"

   )) (format t "~A~%" line))

  ;;; declare arrays
  (format t "/xarray ~D array def~%" num-psnodes)
  (format t "/widtharray ~D array def~%" num-psnodes)

  ;;; define global settings
  (format t "/leftmargin 36 def~%")
  (format t "/botmargin 36 def~%")
  (format t "/pagewidth ~D def~%" pagewidth)
  (format t "/pageheight ~D def~%" pageheight)
  (format t "/boxradius ~D def ~%" boxradius)
  (format t "/boxedge ~D def ~%" boxedge)
  (format t "/boxgray ~A def ~%" boxgray)
  (format t "/boxkind {~A} def~%" boxkind)

  ;;; compute width and height of each node
  (format t "/~A findfont ~D scalefont setfont~%" fontname fontsize)
  (dotimes (index num-psnodes)
    (format t "widtharray ~D (~A) stringwidth pop put~%"
	    index
	    (car (psnode-info (aref narray index)))))
  (format t "/~A findfont ~D scalefont setfont~%"
	  second-fontname second-fontsize)
  (dotimes (index num-psnodes)
    (format t "widtharray ~D get~%" index)
    (dolist (info (cdr (psnode-info (aref narray index))))
       (format t "(~A) stringwidth pop max~%" info))
    (format t "~D add widtharray exch ~D exch put~%"
 	    (* 2 boxedge)
	    index))
  (dotimes (index num-psnodes)
    (setf (psnode-height (aref narray index))
	  (+ (* 2 boxedge)
	     extra-y-spacing
	     fontsize
	     (* (- (length (psnode-info (aref narray index))) 1)
		second-fontsize))))

  ;;; compute x location of each node
  (format t "xarray 0 0 put~%")
  (dolist (index (cdr top-sort))
    (let ((parents (psnode-parents (aref narray index))))
      (format t "xarray ~D get widtharray ~D get add~%"
	      (car parents) (car parents))
      (dolist (parent (cdr parents))
	  (format t "xarray ~D get widtharray ~D get add max~%"
		  parent parent))
      (format t "~D add xarray exch ~D exch put~%" extra-x-spacing index)))

  ;;; compute maximum x used
  (format t "/maximum-x 0~%")
  (dotimes (index num-psnodes)
     (format t "xarray ~D get widtharray ~D get add max~%"
	     index index))
  (format t "def~%")

  ;;; compute y location of each node and maximum and minimum y used
  (setq maximum-y 0)
  (setq minimum-y 0)
  (setq psnodes-at-y (make-hash-table :size (* num-psnodes fontsize)
				    :rehash-size 2.0))
  (let ((currenty 0))
    (dolist (index (reverse top-sort))
       (let (desired-y)
	 (cond ((null (psnode-children (aref narray index)))
	          (setf desired-y currenty)
		  (setf currenty (+ currenty (psnode-height
					      (aref narray index)))))
	       (t
		 (let ((children (psnode-children (aref narray index)))
		       (ysum 0))
		   (dolist (child children)
		      (setf ysum (+ ysum (psnode-yvalue (aref narray child)))))
		   (setq desired-y (floor ysum (length children))))))

	 ;;; We may not be able to put the node at the desired y.
	 ;;; If there is another node that overlaps in the y direction
	 ;;; and that node is neither a parent* nor child* (hence its x
	 ;;; location may overlap this one) then we have to choose
         ;;; another y value -- we choose the nearest (up or down)
	 ;;; location so that there is no possible overlap in y or x.
	 (let ((height (psnode-height (aref narray index)))
	       (related (make-hash-table :test #'equal
					 :size (+ 50 num-psnodes)
					 :rehash-size 2.0))
	       collision
	       upward-bot
	       upward-top
	       downward-bot
	       downward-top)
	   (setq upward-bot desired-y)
	   (setq upward-top upward-bot)
	   (setq downward-top (- (+ desired-y height) 1))
	   (setq downward-bot downward-top)
	   (loop
	      ;;; check upward-top for collision
	     (setq collision nil)
	     (dolist (n (gethash upward-top psnodes-at-y))
		(let ((r (gethash n related)))
		  (when (null r)
			(setf r (related-classes n index))
			(setf (gethash n related) r))
		  (when (eql r 'no)
			(setq collision t)
			(return))))
				 
	     ;;; if no collision and big enough space then we win
	     (incf upward-top)
	     (when (and (not collision) (= (- upward-top upward-bot) height))
		   (setf desired-y upward-bot)
		   (return))

	     (when collision
		   (setf upward-bot upward-top))

	     ;;; check downward-bot for collision
	     (setq collision nil)
	     (dolist (n (gethash downward-bot psnodes-at-y))
		(let ((r (gethash n related)))
		  (when (null r)
			(setf r (related-classes n index))
			(setf (gethash n related) r))
		  (when (eql r 'no)
			(setq collision t)
			(return))))

	      ;;; if no collision and big enough space then we win
	     (decf downward-bot)
	     (when (and (not collision) (= (- downward-top downward-bot) height))
		   (setf desired-y (+ 1 downward-bot))
		   (return))
	     
	     (when collision
		   (setf downward-top downward-bot))
	   )
	   ;;; add our name to psnodes-at-y table
	   (dotimes (i height)
	      (push index (gethash (+ i desired-y) psnodes-at-y)))

	   (setf (psnode-yvalue (aref narray index)) desired-y)
	 )

	 (setf minimum-y (min minimum-y (psnode-yvalue (aref narray index))))
	 (setf maximum-y
	       (max maximum-y (+ (psnode-yvalue (aref narray index))
				 (psnode-height (aref narray index))))))))
		       
  ;;; compute y-offset to center graph vertically
  (setq rows (ceiling (- maximum-y minimum-y) pageheight))
  (setq y-offset (- (floor (- (* rows pageheight) (- maximum-y minimum-y)) 2)
		    minimum-y))
  (when shrink (setq y-offset (- 0 minimum-y)))

  ;;; create dictionary big enough to hold all the
  ;;; procedures defined below and make it a current dictionary
  (format t "~D dict begin~%"
	  (+ (* 4 (+ num-psnodes (ceiling num-psnodes chunksize))) 50))

  ;;; define procedures to display the background box for each node
  (dotimes (index num-psnodes)
     (format t "/box~D {~%" index)
     (format t "xarray ~D get ~D widtharray ~D get ~D drawbox~%"
	     index
	     (+ y-offset 
		(psnode-yvalue (aref narray index))
                (floor extra-y-spacing 2))
	     index
	     (- (psnode-height (aref narray index)) extra-y-spacing))
     (format t "} def~%"))

  ;;; define procedures to display the text info for each node
  (dotimes (index num-psnodes)
     (let ((yvalue (+ y-offset
		      (floor extra-y-spacing 2)
		      (floor fontsize 5)
                      boxedge
		      (psnode-yvalue (aref narray index))
		      (* second-fontsize
			 (- (length (psnode-info (aref narray index))) 1)))))
       (format t "/text~D {xarray ~D get boxedge add ~D moveto (~A) show} def~%"
	       index
	       index
	       yvalue
	       (car (psnode-info (aref narray index))))
       (when (not (null (cdr (psnode-info (aref narray index)))))
	   (format t "/secondtext~D {~%" index)
	   (dolist (info (cdr (psnode-info (aref narray index))))
	      (setq yvalue (- yvalue second-fontsize))
	      (format t "xarray ~D get boxedge add ~D moveto (~A) show~%"
		      index
		      yvalue
		      info))
	   (format t "} def~%"))))

  ;;; define procedures to display the edges leading into each node
  (dotimes (index num-psnodes)
     (format t "/edge~D {newpath~%" index)
     (dolist (parent (psnode-parents (aref narray index)))
        (format t "xarray ~D get widtharray ~D get add ~D moveto~%"
		parent
		parent
		(+ (psnode-yvalue (aref narray parent))
		   (floor (psnode-height (aref narray parent)) 2)
		   y-offset))
	(format t "xarray ~D get ~D lineto~%"
		index
		(+ (psnode-yvalue (aref narray index))
		   (floor (psnode-height (aref narray index)) 2)
		   y-offset)))
     (format t "stroke } def~%"))

  ;;; Define procedures to display chunks of boxes, text, and edges.
  ;;; We limit each chunk to at most chunksize calls, to avoid overflowing
  ;;; the Postscript operand stack.
  (dotimes (index num-psnodes)
     (when (eql (mod index chunksize) 0)
	   (format t "/boxchunk~D {~%" (floor index chunksize)))
     (format t "box~D~%" index)
     (when (or (eql (mod index chunksize) (- chunksize 1))
	       (eql index (- num-psnodes 1)))
	   (format t "} def~%")))
  (dotimes (index num-psnodes)
     (when (eql (mod index chunksize) 0)
	   (format t "/textchunk~D {~%" (floor index chunksize)))
     (format t "text~D~%" index)
     (when (or (eql (mod index chunksize) (- chunksize 1))
	       (eql index (- num-psnodes 1)))
	   (format t "} def~%")))
  (dotimes (index num-psnodes)
     (when (eql (mod index chunksize) 0)
	   (format t "/secondtextchunk~D {~%" (floor index chunksize)))
     (when (not (null (cdr (psnode-info (aref narray index)))))
	   (format t "secondtext~D~%" index))
     (when (or (eql (mod index chunksize) (- chunksize 1))
	       (eql index (- num-psnodes 1)))
	   (format t "} def~%")))
  (dotimes (index num-psnodes)
     (when (eql (mod index chunksize) 0)
	   (format t "/edgechunk~D {~%" (floor index chunksize)))
     (format t "edge~D~%" index)
     (when (or (eql (mod index chunksize) (- chunksize 1))
	       (eql index (- num-psnodes 1)))
	   (format t "} def~%")))

  ;;; Define procedure to display entire graph.
  ;;; First do the boxes, then the edges, then the text.
  (format t "/drawgraph { gsave~%")
  (dotimes (i (ceiling num-psnodes chunksize))
     (format t "boxchunk~D~%" i))
  (format t "~A setlinewidth~%" edgewidth)
  (format t "~A setlinecap~%" edgecap)
  (format t "~A setgray~%" edgegray)
  (dotimes (i (ceiling num-psnodes chunksize))
     (format t "edgechunk~D~%" i))
  (format t "~A setgray~%" textgray)
  (format t "/~A findfont ~D scalefont setfont~%" fontname fontsize)
  (dotimes (i (ceiling num-psnodes chunksize))
     (format t "textchunk~D~%" i))
  (format t "/~A findfont ~D scalefont setfont~%" second-fontname second-fontsize)
  (dotimes (i (ceiling num-psnodes chunksize))
     (format t "secondtextchunk~D~%" i))
  (format t "grestore } def~%")

  ;;; show the virtual page in as many actual pages as needed
  (cond (shrink
          ;;; shrink the output to fit on one page
	  (format t "leftmargin botmargin translate~%")
	  (format t "pagewidth dup maximum-x max div pageheight dup ~
	             ~D max div scale~%"
		  (- maximum-y minimum-y))
	  (if scribe-p
	      (format t "drawgraph end~%")
	      (format t "drawgraph showpage end~%")))
	(t
	  (format t "{drawgraph} maximum-x pagewidth div ceiling ~
	             ~D printposter end~%"
		  rows)))
  
)


(defun walk-graph (root)
  (when (eql num-psnodes max-psnodes)
	(error "More than ~D nodes in graph.  Graphing aborted."))
  (let ((root-index num-psnodes)
	(child-names (apply child-function (list root))))
    (incf num-psnodes)
    (setf (gethash root psnode-index) root-index)
    (setf (aref narray root-index) (make-psnode))
    (setf (psnode-name (aref narray root-index)) root)
    (setf (psnode-info (aref narray root-index))
	  (apply info-function (list root)))
    (setf (psnode-children (aref narray root-index)) nil)
    (setf (psnode-parents (aref narray root-index)) nil)
    (setf (psnode-appears-in-top-sort (aref narray root-index)) nil)
    (setf (psnode-children-appear-in-top-sort (aref narray root-index)) nil)
    (dolist (child child-names)
	    (let ((child-index (gethash child psnode-index)))
	      (cond (child-index
		      (push child-index
			    (psnode-children (aref narray root-index)))
		      (push root-index
			    (psnode-parents (aref narray child-index))))
		    (t
		      (let ((child-index (walk-graph child)))
			(push child-index
			      (psnode-children (aref narray root-index)))
			(push root-index
			      (psnode-parents (aref narray child-index))))))))
    root-index)
)
	  
(defun top-sort-node (index)
  (when (not (psnode-appears-in-top-sort (aref narray index)))

	;;; make sure the parents are processed
	(dolist (parent (psnode-parents (aref narray index)))
		(top-sort-parent parent))

	;;; add this node to top-sort
	(push index top-sort)
	(setf (psnode-appears-in-top-sort (aref narray index)) t))

  (when (not (psnode-children-appear-in-top-sort (aref narray index)))
	(dolist (child (psnode-children (aref narray index)))
		(top-sort-node child))
	(setf (psnode-children-appear-in-top-sort (aref narray index)) t))
)

(defun top-sort-parent (index)
  (when (not (psnode-appears-in-top-sort (aref narray index)))

	;;; make sure the parents are processed
	(dolist (parent (psnode-parents (aref narray index)))
		(top-sort-parent parent))

	;;; add this node to top-sort
	(push index top-sort)
	(setf (psnode-appears-in-top-sort (aref narray index)) t))
)

(defun related-classes (x y)
  (cond ((ancestor x y) 'yes)
	((ancestor y x) 'yes)
	(t 'no))
)

(defun ancestor (x y)
  (let ((cached-value (gethash (list x y) ancestor-cache)))
    (cond (cached-value (car cached-value))
	  (t
	    (setq cached-value
		  (cond ((equal x y) t)
			(t
			  (some #'(lambda (child) (ancestor child y))
				(psnode-children (aref narray x))))))
	    (setf (gethash (list x y) ancestor-cache) (list cached-value))
	    cached-value)))
)
