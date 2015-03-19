;;;; CarGo
;;
;;  CarGo is copyright (c) 1993 by Peter Dudey
;;
;;  Thanks to:
;;  All those involved with Garnet, Lucid Common LISP, GNU Emacs, 
;;  and the OSU department of computer science
;;
;;  Jim Levenick, for all of his help in Go
;;  Bob French, for introducing me to LISP and GNU Emacs
;;  David Kosbie, for extensive help with Garnet
;;
;;  To run CarGo, load this file into Lucid Common LISP, and type:
;;  (cargo:do-go)



;;; Package and initialization stuff

(defpackage "CARGO"
  (:use :kr :common-lisp)
  (:export do-go do-stop)		; Garnet standard is to export these two functions
  )
(in-package "CARGO")			; This is the CarGo package

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "KR")
    (error "You need to load Garnet first."))
  (defvar initialization		; So this will only be done once
    (progn
      (common-lisp-user::garnet-load "gadgets:menubar-loader") ; The menubar gadget
      (use-package 'kr))))				       ; Use Garnet's KR object package


;;; Constants

(defvar *board-lines* 19
  "Number of lines on the board.")
(defparameter *board-points* (* *board-lines* *board-lines*)
  "Number of points on the board.")
(defparameter *stone-width* 35
  "Width of graphical stones, in pixels.")
(defparameter *stone-offset* (round *stone-width* 2)
  "Number of pixels from center to edge of a graphical stone.")
(defparameter *handicap-point-width* (round *stone-width* 4)
  "Width of handicap dots, in pixels.")
(defparameter *handicap-point-offset* (round *stone-width* 8)
  "Number of pixels from center to edge of a handicap point.")
(defparameter *board-width* (* *board-lines* *stone-width*)
  "Width of the board, in pixels.")
(defparameter *window-width* (+ *board-width* *stone-width*)
  "Width of window, in pixels.")
(defvar *factorial-array* (make-array 181 :initial-element NIL)
  "Factorials of 0 through 181.")


;;; Global variables (that aren't objects)

(defparameter *row-aggregates* (make-array *board-lines*)
  "Array of aggrelists containing graphical stones.")
(defparameter *chains* NIL
  "List of chains of adjacent stones.")
(defparameter *objects* NIL
  "Master list of instances, to be destroyed by do-stop.")


;;; Objects that may be referred to before they are created

(proclaim '(special board-data		; Extensive data on points and chains
		    go-window		; The window containing all of the graphics
		    top-aggregate	; The aggregate for GO-WINDOW
		    board-background	; The picture of the board
		    stone-setter	; The button for playing stones
		    life-checker))	; The button for checking life


;;; Utility functions used in several places

(defun pixel-to-row (pixel)
  "Returns the board row closest to a particular pixel position (in one dimension)."
  (let ((row (round (- pixel *stone-width*) *stone-width*)))
    (cond ((>= row *board-lines*) (1- *board-lines*))
	  ((<= row 0) 0)
	  (T row))))

(defun message-box (message)
  "Creates a box with a message on it.  Clicking on the box destroys it."
  (create-instance			; Create a window for the message
   'message-window inter:interactor-window
   (:left 100)
   (:top 100)
   (:height
    (max (opal:string-height opal:default-font message) 28))
   (:width
    (max (opal:string-width opal:default-font message) 217))
   (:modal-p T)
   (:title "(Click in message to continue)"))
  (s-value message-window :aggregate	; Create an aggregate for the window
	   (create-instance
	    'message-aggregate opal:aggregate))
  (create-instance			; Create the text
   'message-text opal:multi-text
   (:justification :center)
   (:string message))
  (opal:add-component message-aggregate message-text) ; Install the text
  (opal:update message-window)			      ; Update the window
  (create-instance			              ; Create a button to destroy all this stuff
   'message-button inter:button-interactor
   (:window message-window)
   (:start-where (list :in message-text))
   (:continuous NIL)
   (:final-function #'(lambda (button object-over)
			(declare (ignore button object-over))
			(opal:destroy message-window)))))

(defun score-mode ()
  "Destroys STONE-SETTER and sets up new ones to allow players to remove dead chains.  As a
side effect, LIFE-CHECKER is changed so that when the right button is clicked, the score is
counted and reported."
  (opal:destroy stone-setter)
  (create-instance		    ; A similar "button", using the right button, checking life
      'dead-chain-remover inter:button-interactor
    (:continuous NIL)
    (:start-where `(:in ,board-background))
    (:start-event :leftdown)
    (:window go-window)
    (:final-function
     #'(lambda (button object-over)
	 (declare (ignore button object-over))
	 (let* ((row (pixel-to-row (inter::event-y inter::*current-event*)))
		(column (pixel-to-row (inter::event-x inter::*current-event*)))
		(chain (g-value (aref (g-value board-data :points) row column) :chain)))
	   (if chain			; If there's a stone there
	       (progn
		 (dolist (dead-stone (g-value chain :stones)) ; For each stone in the captured group
		   (s-value dead-stone :color NIL)	      ;  set its :COLOR to NIL
		   (opal:destroy (g-value dead-stone :graphical-stone)) ;  destroy the graphical stone
		   (s-value dead-stone :graphical-stone NIL)) ;  set the point's :GRAPHICAL-STONE slot to NIL
		 (opal:update go-window))
	       (inter:beep))))))	;  otherwise beep
  (s-value life-checker :final-function
	   #'(lambda (button object-over)
	       (declare (ignore button object-over))
	       (kr-send board-data :count-score))))


;;; Prototypes

(create-instance			; Line drawn on board
 'proto-line opal:line
 (:line-style opal:line-2))

(create-instance			; Handicap point drawn on board
 'proto-handicap-point opal:circle
 (:width *handicap-point-width*)
 (:height *handicap-point-width*)
 (:filling-style opal:black-fill))

(create-instance			; Graphical stone object
 'proto-stone opal:circle
 (:width *stone-width*)
 (:height *stone-width*))

(create-schema		      ; Data for board point
 'proto-point
 (:color NIL)		      ; :BLACK, :WHITE, or NIL
 (:graphical-stone NIL)	      ; The picture of the stone (if any)
 (:chain NIL)		      ; The chain of adjacent stones to which the point belongs, or NIL
 (:scored NIL)		      ; This will be set to :black, :white, or :dame in scoring
 (:tag NIL))		      ; Used by various functions to prevent redundant processing

(create-schema				; Chain of adjacent stones
 'proto-chain
 (:liberties NIL)			; Adjacent empty points
 (:enemies NIL)				; Adjacent enemy chains
 (:tag NIL))				; Flag for routines such as life checking

(create-schema
 'proto-move
 (:captured NIL))			; The chains that were captured by this move

(create-schema				; Data about the board
 'proto-board-data							    
 (:points (make-array (list *board-lines* *board-lines*) ; An array of points
		      :initial-element NIL))
 (:ko NIL)				; The point, if any, where ko prohibits play
 (:player :black)			; Whose move it is
 (:turn 1)				; The turn number
 (:passes 0)				; How many consecutivepasses there have been
 (:history NIL))			; Stack of moves which have been made (for undoing)


;;; Points

;; Functions used by methods

(defun note-neighbors (point row column point-array)
  "Sets the :adjacent slot of POINT to a list of its north, south, east, and west neighbors."
  (s-value point :adjacent
	   (mapcar #'(lambda (location)
		       (let ((r (car location))
			     (c (cdr location)))
			 (if (and (>= r 0) (< r *board-lines*) (>= c 0) (< c *board-lines*))
			     (aref point-array r c)
			   NIL)))
		   `((,(1- row) . ,column)
		     (,(1+ row) . ,column)
		     (,row . ,(1+ column))
		     (,row . ,(1- column))))))

(defun check-neighboring-chains (point my-chain)
  "Looks at points adjacent to the newly played stone at point POINT.
Off-board points are ignored.  Empty points are added to the list in the :LIBERTIES slot of
MY-CHAIN.  Enemy chains are added to the list in the :ENEMIES slot of MY-CHAIN, and MY-CHAIN is
added to their :ENEMIES lists.  Friendly chains are RETURNED in a list.  The point POINT is
removed from the liberty lists of all adjacent enemy chains."
  (let ((friendly-chains NIL))
    (dolist (neighbor (g-value point :adjacent))
      (if neighbor
	  (let ((neighboring-chain (g-value neighbor :chain)))
	    (cond
	      ((not neighboring-chain)	; The point is empty
	       (push neighbor (g-value my-chain :liberties)))
	      ((eq (g-value neighbor :color)
		   (g-value point :color)) ; The point is in a friendly chain
	       (pushnew neighboring-chain friendly-chains))
	      (t			; The point is in an enemy chain
	       (pushnew neighboring-chain (g-value my-chain :enemies))
	       (pushnew my-chain (g-value neighboring-chain :enemies))
	       (s-value neighboring-chain :liberties
			(delete point (g-value neighboring-chain :liberties))))))))
    friendly-chains))

(defun do-any-captures-around (point)
  "Checks to see if any of the points adjacent to POINT are enemy points with no liberties.  
If so, they are removed from the enemy lists of their enemies and from *CHAINS*.  All of
the points in the captured chain are set to :COLOR NIL, and added to the liberty lists of chains
to which they are adjacent.
Returns a list of chains which were captured."
  (let ((captured-chains NIL))
    (dolist (neighbor (g-value point :adjacent))
      ;; If the point exists, and is in a libertyless enemy chain
      (when (and neighbor
		 (g-value neighbor :chain)
		 (not (eq (g-value point :color) (g-value neighbor :color)))
		 (not (g-value neighbor :chain :liberties)))
	  (let ((victim (g-value neighbor :chain)))
	    (push victim captured-chains)
	    (setq *chains* (delete victim *chains*))
	    ;; For each enemy of VICTIM
	    ;; remove VICTIM from the enemy's enemies
	    (dolist (enemy (g-value victim :enemies))
	      (s-value enemy :enemies
		       (delete victim (g-value enemy :enemies))))
	    ;; For each stone in the captured group
	    ;; set its :COLOR to NIL
	    (dolist (dead-stone (g-value victim :stones)) 
	      (s-value dead-stone :color NIL)
	      ;; destroy the graphical stone
	      ;; set the point's :GRAPHICAL-STONE slot to NIL
	      (opal:destroy (g-value dead-stone :graphical-stone))
	      (s-value dead-stone :graphical-stone NIL)

	      ;; set the point's :CHAIN slot to NIL
	      ;; and for each enemy of the captured chain
	      ;; if this point is adjacent to it
	      ;; add it to the enemy's liberty list		    
	      (s-value dead-stone :chain NIL)
	      (dolist (enemy (g-value victim :enemies))
		(if (intersection (g-value dead-stone :adjacent)
				  (g-value enemy :stones))
		    (push dead-stone (g-value enemy :liberties))))))))
    ;; Return the list of captured chains
    captured-chains))

(defun list-neighbor-scores (point)
  "Returns a list of the :scored slots of points adjacent to POINT."
  (let ((result NIL))
    (dolist (neighbor (g-value point :adjacent))
	    (if neighbor
		(push (g-value neighbor :scored) result)))
    result))


;;; Methods
;;

(define-method :get-played-at proto-point (self row column player move)
  "Sets the point's color and creates a chain and a graphical stone.  
The appropriate changes are made to MOVE."
  (s-value self :color player)		; Set the color of the point
  (s-value self :graphical-stone	; Tell STONE-SETTER to create a stone, and note it
	   (kr-send stone-setter :play-at row column player))
  (let ((new-chain (s-value self :chain (create-instance ; Create the new chain
					 NIL proto-chain
					 (:stones (list self))
					 (:color player)))))
    (push new-chain *chains*)		; Add it to the *CHAINS*
    (push new-chain *objects*)		; Add it to *OBJECTS* so do-stop can destroy it
    (s-value move :chain new-chain)
    (s-value move :merged		; Look at adjacent points
	     (check-neighboring-chains self new-chain))
    (s-value move :merged
	     ;; Merge with friendly neighboring chains
	     ;; after checking the adjacent points
	     (cons (merge-with
		    new-chain
		    (g-value move :merged)) 
		   (cons new-chain
			 (g-value move :merged)))))
  (s-value move :captured
	   ;; Handle any captures, and give MOVE the list of captured chains
	   (do-any-captures-around self))) 

(define-method :count-self proto-point (self)
  "Looks at own color and neighboring points to score self as :blackish, :whitish,
or :unknown (still undetermined), :black, :white, or :dame."
  (case (g-value self :scored)
    ;; If already settled, don't do anything
   ((:black :white :dame)		
    NIL)
   ((:blackish)				
    (s-value self :scored
	     (cond
	      ((intersection (list-neighbor-scores self) '(:dame :white :whitish))
	       :dame)
	      (T
	       :blackish))))
   ((:whitish)
    (s-value self :scored
	     (cond
	      ((intersection (list-neighbor-scores self) '(:dame :black :blackish))
	       :dame)
	      (T
	       :whitish))))
   ;; The point is unoccupied, and hasn't looked at neighbors
   ((:unknown)
    (let ((result (list-neighbor-scores self)))
      (s-value self :scored
	       (cond
		 ;; If there are :dame adjacent,
		 ;; or point leaning to both colors,
		((or (member :dame result)
		     (and (intersection result '(:white :whitish))
			  (intersection result '(:black :blackish))))
		 :dame)					       ;   this is :dame
		;; If there white-leaning but no black-leaning adjacent,
		((and (intersection result '(:white :whitish))
		      (not (intersection result '(:black :blackish))))
		 :whitish)						    ;  this is :whitish
		;; If there black-leaning but no white-leaning adjacent,
		((and (intersection result '(:black :blackish))
		      (not (intersection result '(:white :whitish))))
		 :blackish)						    ;  this is :blackish
		;; If there are only NIL and :unknown adjacent		
		(T
		 :unknown)))))		;  this is still unknown
   ;; If this is the first guess, look at own color
   (T
    (if (g-value self :color)
	(s-value self :scored (g-value self :color))
      (s-value self :scored :unknown)))))

(define-method :destroy proto-point (self)
  "Destroys the object."
  (destroy-schema self))



;;; Chains
;; NOTE:  The :unconditionally-alive method, and its subordinate functions, 
;; are located below in a special section

;; Functions used by methods

(defun merge-with (self other-chains)
  "Merges SELF and OTHER-CHAINS into a new chain, which is returned."
  (when other-chains
    (let ((new-chain (create-instance	; Create a new chain
			 NIL proto-chain
		       (:stones NIL)
		       (:color (g-value self :color)))))
      (push new-chain *OBJECTS*)	; Add the new chain to *OBJECTS*
      (dolist (chain (cons self other-chains))
	(dolist (stone (g-value chain :stones))
	  ;; tell the stone it belongs to NEW-CHAIN
	  ;; and vice-versa
	  (s-value stone :chain new-chain)
	  (pushnew stone (g-value new-chain :stones)))
	;; Add each liberty to NEW-CHAIN's liberties
	(dolist (liberty (g-value chain :liberties))       
	  (pushnew liberty (g-value new-chain :liberties)))
	;; Add each enemy chain to NEW-CHAIN's enemies
	(dolist (enemy (g-value chain :enemies))       
	  (pushnew enemy (g-value new-chain :enemies)) 
	  (s-value enemy :enemies
		   (delete chain (g-value enemy :enemies))) ; delete CHAIN
	  (s-value enemy :enemies	; and add NEW-CHAIN to ENEMY's list of enemies
		   (pushnew new-chain (g-value enemy :enemies))))
	(setq *chains* (delete chain *chains*))) ; Remove each old chain from *CHAINS*
      ;; Since the old chains need to be recoverable for undos, the new
      ;; stone isn't removed from their liberty lists.
      (s-value new-chain :liberties 
	       (delete (car (g-value self :stones)) (g-value new-chain :liberties))) 
      ;; This makes sure SELF's stone isn't a liberty for NEW-CHAIN
      (push new-chain *chains*)		; Add NEW-CHAIN to *CHAINS*
      new-chain)))

(defun uncapture (self)
  "Return the undead chain (which I cleverly didn't destroy) to *CHAINS*, 
re-create its stones, tell them that they now belong to this chain, adding
SELF to the enemy lists of its enemies, and removing from their lists of
liberties all adjacent members of SELF."
  (push self *chains*)
  (dolist (stone (g-value self :stones))  ; For each stone in SELF
    (s-value stone :graphical-stone	  ;  Create a graphical stone for it
	     (kr-send stone-setter :play-at
		      (g-value stone :row)
		      (g-value stone :column)
		      (g-value self :color)))
    (s-value stone :color (g-value self :color)) ;  Set the point to the proper color
    (s-value stone :chain self))		 ;  Tell it SELF is its chain
  (dolist (enemy (g-value self :enemies))	 ; For each adjacent enemy chain
    (push self (g-value enemy :enemies))	 ;  Add SELF to its list of enemies
    (dolist (stone (g-value self :stones))	 ;  For each stone in self
      (if (member stone (g-value enemy :liberties)) ;   If it's a liberty of the enemy chain
	  (s-value enemy :liberties	;    remove it from that chain's liberty list
		   (delete stone (g-value enemy :liberties)))))))


;; Methods

(defun unmerge (merge-list)
  "Unmerges MERGE-LIST, the first element of which is the object that receives this message,
and the rest of which are chains that were merged to create this object.  Each chain in the
cdr, therefore, has to have its members told that they now belong to SELF (the car),and SELF
must be added to the enemy lists of each of the enemies of any members of the cdr.  The splitee
must also be removed from each of these lists, and from *CHAINS*, and be destroyed."
  (let ((self (car merge-list)))
    (dolist (chain (cdr merge-list))		    ; For each chain that was merged into SELF
      (dolist (stone (g-value chain :stones))	    ;  update the chain of each stone
	(s-value stone :chain chain))
      (dolist (enemy (g-value chain :enemies)) ;  update enemy lists
	(s-value enemy :enemies (delete self (g-value enemy :enemies)))
	(push chain (g-value enemy :enemies)))
      (push chain *chains*))		     ;  add the resurrected chains to *CHAINS*
    (setf *chains* (delete self *chains*))   ; Remove self from *CHAINS*
    (setq *objects* (delete self *objects*)) ; Ditto *OBJECTS*
    (opal:destroy self)))
    
(define-method :undo-destroy proto-chain (self)
  "A destroy method called by proto-board's :UNDO method.  SELF will always consist of exactly
one stone.  Each point adjacent to SELF needs to have SELF removed from its list of enemies,
and have the point added to its list of liberties.  The point in SELF needs to have its color
changed and its stone set to NIL, and SELF needs to be destroyed."
  (let ((stone (car (g-value self :stones))))
    (dolist (neighbor (g-value stone :adjacent)) ; Deal with the neighboring chains
      (if (and neighbor (g-value neighbor :chain))
	  (let ((chain (g-value neighbor :chain)))
	    (unless (eq (g-value chain :color) (g-value self :color))
	      (s-value chain :enemies (delete self (g-value chain :enemies)))
	      (pushnew stone (g-value chain :liberties))))))
    (s-value stone :color NIL)		; Deal with the stones in SELF
    (s-value stone :chain NIL)
    (opal:destroy (g-value stone :graphical-stone)) ; Destroy objects which are no longer needed
    (setf *chains* (delete self *chains*))
    (destroy-schema self)))

(define-method :flash proto-chain (self)
  "Flashes each stone belong to a point in proto-chain -- a handy debugging feature."
   (dolist (stone
	     (mapcar 
	      #'(lambda (point)
		  (g-value point :graphical-stone))
	      (g-value self :stones)))
     (garnet-debug:flash stone)))

(define-method :destroy proto-chain (self)
  "Destroys the object."
  (destroy-schema self))



;;; Board-data

;; Methods
 
(define-method :play-at proto-board-data (row column player)
 "Attempts to make a move for PLAYER (:black or :white) at ROW, COLUMN.  Returns the next
player if successful, otherwise NIL."
  (when (kr-send board-data :legal-move row column player)		 
    (let ((this-move (create-instance
			 NIL proto-move
		       (:ko (g-value board-data :ko))))
	  (this-point (aref (g-value board-data :points) row column)))
      (push this-move *objects*)	; Add the object for this move to the list of objects
      (kr-send (aref (g-value board-data :points) row column) ; Tell the point it's been played at
	       :get-played-at
	       (aref (g-value board-data :points) row column)
	       row
	       column
	       player
	       this-move)
      (push this-move (g-value board-data :history))
      (s-value board-data :ko		; Set the ko value
	       (if (and (g-value this-move :captured)
			(= 1					      ; If
			   (length (g-value this-move :captured))     ;  one chain was captured
			   (length (g-value (car (g-value this-move :captured)) :stones)) ;  containing one stone
			   (length (g-value this-point :chain :stones)) ;  and the chain of THIS-POINT has one stone
			   (length (g-value this-point :chain :liberties)))) ;  and one liberty
		   (car (g-value this-point :chain :liberties))	;   make that liberty the ko point
		   NIL)))					;  otherwise, set it to NIL
    (incf (g-value board-data :turn))				; Increment the turn
    (s-value board-data :passes 0)	; Zero the tally of consecutive passes
    (s-value board-data :player		; Toggle :PLAYER
	     (if (eq (g-value board-data :player) :black) :white :black))))

(define-method :pass proto-board-data ()
  "Makes a pass move, and ends the game if appropriate."
  (push (create-instance
	 NIL proto-move
	 (:ko (g-value board-data :ko))
	 (:chain NIL)
	 (:merged NIL)
	 (:captured NIL))
	(g-value board-data :history))
  (incf (g-value board-data :turn))
  (incf (g-value board-data :passes))
  (s-value board-data :player
	   (if (eq (g-value board-data :player) :black)
	       :white
	     :black))
  (when (>= (g-value board-data :passes) 3)
	(message-box (format NIL "GAME OVER~2%~
After clicking on this window,~%~
click on dead chains with left button,~%~
then hit right button when done.."))
	(score-mode)))

(define-method :undo proto-board-data ()
  "Undoes the last move, if there is one.
  There is a slight flaw with this:  if there are two passes and then a move, undoing the move will leave the :PASSES slot of BOARD-DATA
  at 0, not 2."
  (cond
   ((g-value board-data :history)
    (let* ((last-move (pop (g-value board-data :history)))
	   (merged (g-value last-move :merged)))
      (cond
       (merged								    ; This will be NIL iff the move was a pass
	(s-value board-data :ko (g-value last-move :ko))		    ; Restore the old Ko value
	(dolist (chain (g-value last-move :captured))			    ; Tell each captured chain to uncapture itself
		(uncapture chain))
	(if (car (g-value last-move :merged))				    ; If anything was merged by the move
	    (unmerge merged))						    ;  unmerge it
	(kr-send (g-value last-move :chain)				    ; Destroy the one-stone chain created by the move
		 :undo-destroy (g-value last-move :chain)))
       (t								    ; If the move was a pass
	(s-value board-data :passes					    ;  decrement the number of passes
		 (min 0 (1- (g-value board-data :passes))))))
      (decf (g-value board-data :turn))					    ; Decrement the turn number
      (s-value board-data :player					    ; Toggle :PLAYER
	       (if (eq (g-value board-data :player) :black) :white :black))
      (opal:destroy last-move)))
   (t									    ; If there is no previous move, beep
    (inter:beep))))

(define-method :legal-move proto-board-data (row column player)
  "Returns T if the move in question is legal, otherwise NIL."
  (let ((this-point (aref (g-value board-data :points) row column)))
    (not (or (eq this-point (g-value board-data :ko))			    ; The move is legal if it is not ko
	     (g-value this-point :color)				    ;  and the point is not occupied
	     (let ((safe NIL))
	       (dolist (neighbor (g-value this-point :adjacent))
		       (if neighbor
			   (setq safe
				 (or safe
				     (not (g-value neighbor :color))	    ;  and one neighboring point is unoccupied
				     (and (eq player			    ;  or a member of a friendly chain with more than one liberty
					      (g-value neighbor :color))
					  (> (length (g-value neighbor :chain :liberties)) 1))
				     (and (eq (car (remove player '(:black :white))) ;  or a member of an enemy chain with only one liberty
					      (g-value neighbor :color))
					  (= (length (g-value neighbor :chain :liberties)) 1))))))
	       (not safe))))))

(define-method :count-score proto-board-data ()
  "Counts the score (Chinese) with all the graphic majesty available to Garnet.  Black's score minus white's score is returned."
  (dotimes (i (* 2 (1- *board-lines*)))					    ; Do it enough times to propagate across the board
	   (dotimes (j *board-points*)
		    (let ((point (row-major-aref (g-value board-data :points) j)))
		      (kr-send point :count-self point))))
  (let ((black-score 0)							    ; Count points and draw gray stones for territory
	(white-score 0))
    (dotimes (i *board-lines*)
	     (dotimes (j *board-lines*)
		      (let ((point (aref (g-value board-data :points) i j)))
			(case (g-value point :scored)
			      (:black
			       (incf black-score))
			      (:white
			       (incf white-score))
			      (:blackish
			       (incf black-score)
			       (opal:add-component
				(svref *row-aggregates* i)
				(create-instance
				 NIL proto-stone
				 (:left (+ *stone-width* (- (* j *stone-width*) *stone-offset*)))
				 (:top (+ *stone-width* (- (* i *stone-width*) *stone-offset*)))
				 (:filling-style opal:dark-gray-fill))))
			      (:whitish
			       (incf white-score)
			       (opal:add-component
				(svref *row-aggregates* i)
				(create-instance
				 NIL proto-stone
				 (:left (+ *stone-width* (- (* j *stone-width*) *stone-offset*)))
				 (:top (+ *stone-width* (- (* i *stone-width*) *stone-offset*)))
				 (:filling-style opal:light-gray-fill)))))
			(s-value point :scored NIL))))			    ; Remove the scoring information
    (opal:update go-window)
    (message-box (format NIL "Black: ~a~%White: ~a~2%~a~2%Click in here to see board."
			 black-score
			 white-score
			 (cond
			  ((> black-score white-score)
			   "Black Wins")
			  ((> white-score black-score)
			   "White Wins")
			  (T
			   "Tie"))))
  (- black-score white-score)))						    ; Returns board value for black (useful in minimax)

(define-method :initialize proto-board-data (self)
  "Installs initial values in arrays for PROTO-BOARD-DATA"
  (dotimes (i *board-lines*)						    ; Install proto-points in PROTO-BOARD-DATA's :POINTS array
	   (dotimes (j *board-lines*)
		    (push (setf (aref (g-value self :points) i j)	    ; As each point is created, push it onto *OBJECTS*
				(create-instance
				 NIL proto-point
				 (:row i)
				 (:column j)))
			  *objects*)))
  (let ((points (g-value self :points)))				    ; Have each point note its neighbors
    (dotimes (i *board-lines*)
	     (dotimes (j *board-lines*)
		      (note-neighbors
		       (aref points i j)
		       i
		       j
		       points))))
  (setq *chains* NIL))							    ; Clear the lists of chains

(define-method :destroy proto-board-data (self)
  "Destroys the object."
  (destroy-schema self))



;;; Life detection

;; Functions called by method

(defun copy-move-array (move-array)
  "An awkward kludge to act like copy-seq but handle fill-pointers."
  (let ((result (make-array 181 :fill-pointer (fill-pointer move-array))))
    (dotimes (i (fill-pointer result))
	     (setf (aref result i) (aref move-array i)))
    result))

(defun undo-attacking-moves (move-array minimum)
  "Undoes moves from the fill-pointer of MOVE-ARRAY back to MINIMUM, inclusive."
  (dotimes (i (count-if #'(lambda (x) (g-value x :color)) (subseq move-array minimum (fill-pointer move-array))))
	   (kr-send board-data :undo)))
  
(defun point-tag (x)
  "Gets the :tag value of the proto-point X.  Useful as a :key."
  (g-value x :tag))

(defun easy-first (point-1 point-2)
  "A sorting function for points, based on their :tags.  This will return T iff POINT-1 is 'easier' than POINT-2.  Points tagged NIL are
easiest, the points labeled :tricky, then points labeled :eye, then non-points (NIL)."
  (let ((a (g-value point-1 :tag))
	(b (g-value point-2 :tag)))
    (or (and b (not a))
	(and (eq a :tricky) (eq b :eye)))))

(defun factorial (x)
  (or (svref *factorial-array* x)
      (setf (svref *factorial-array* x)
	    (if (< x 2)
		1
	      (* x (factorial (1- x)))))))

(defun permutation (n a)
  "Swaps items N and N - 1 in the array A, and returns the permuted array.  This is destructive."
  (let ((temp))
    (setf temp (aref a n))
    (setf (aref a n) (aref a (1- n)))
    (setf (aref a (1- n)) temp)
    a))

(defun make-first-attacking-pass (move-array chain)
  "Tries the moves in MOVE-ARRAY in order, tagging points as :tricky and :eye as appropriate.  If CHAIN is captured or has two obvious
eyes, the fill-pointer of MOVE-ARRAY is set to just after the last move made, and :captured or :two-eyes is returned.  Otherwise, NIL is
returned."
  (do* ((i 0 (1+ i))
	(move (aref move-array i) (aref move-array i))
	(attacker (if (eq :black (g-value chain :color)) :white :black))
	(test-stone (car (g-value chain :stones)))
	(eyes 0)
	(row)
	(column)
	(result NIL))
       ((or result (= i (fill-pointer move-array)))
	result)
       (setf row (g-value move :row))
       (setf column (g-value move :column))
       (if (kr-send board-data :legal-move row column attacker)		    ; If the move is legal, play there
	   (progn
	     (kr-send board-data :play-at row column attacker)
	     (opal:update go-window)					    ; Take out these two lines
	     (sleep 0.25)						    ;  if you want speed rather than visual clarity
	     (unless (g-value test-stone :color)
		     (setf (fill-pointer move-array) (1+ i))
		     (setf result :captured)))
	 (let ((defending-chains NIL)					    ; If the move is illegal, it's either tricky or in an
	       (attacking-stones (list move)))				    ;  obvious eye, and things must be tagged
	   (dolist (neighbor (g-value move :adjacent))
		   (if neighbor
		       (if (eq (g-value neighbor :color) attacker)
			   (progn
			     (dolist (defending-chain (g-value neighbor :chain :enemies)) 
				     (pushnew defending-chain defending-chains))
			     (dolist (stone (g-value neighbor :chain :stones))
				     (pushnew stone attacking-stones)))
			 (if (g-value neighbor :color)
			     (pushnew (g-value neighbor :chain) defending-chains)))))
	   (if (equal defending-chains (list chain))			    ;  If only CHAIN is responsible, it's an obvious eye
	       (progn
		 (dolist (stone attacking-stones)
			 (s-value stone :tag :eye))
		 (incf eyes)
		 (when (= eyes 2)
		       (setf (fill-pointer move-array) (1+ i))
		       (setf result :two-eyes)))
	     (dolist (stone attacking-stones)
		     (s-value stone :tag :tricky)))))))

(defun make-attacking-pass (move-array chain first)
  "Tries the moves in MOVE-ARRAY in order, starting with FIRST.  If CHAIN is captured, the fill-pointer of MOVE-ARRAY is set
to just after the last move made, and T is returned.  Otherwise, NIL is returned."
  (do ((i first (1+ i))
       (move)
       (row)
       (column)
       (attacker (if (eq :black (g-value chain :color)) :white :black))
       (test-stone (car (g-value chain :stones)))
       (result NIL))
      ((or result (= i (fill-pointer move-array)))
       result)
      (setf move (aref move-array i))
      (setf row (g-value move :row))
     (setf column (g-value move :column))
      (if (kr-send board-data :legal-move row column attacker)		    ; If the move is legal, play there
          (progn
            (kr-send board-data :play-at row column attacker)
            (opal:update go-window)					    ; Take out these two lines
            (sleep 0.25)						    ;  if you want speed rather than visual clarity
            (if (not (g-value test-stone :color))
		(setf result T))))))

(defun find-moves-to-attack (chain)
  "Finds all liberties of chain, and of other chains adjacent to its liberties, and so on.  These are returned in an array."
  (let ((result (make-array 181 :fill-pointer 0)))
    (dolist (liberty (g-value chain :liberties))			    ; Start with the liberties of the chain
	    (vector-push liberty result))
    (do ((done NIL))							    ; Repeat the following until nothing is added
	(done)
	(setq done T)
	(dotimes (j (fill-pointer result))
		 (let ((liberty (aref result j)))			    ; Find the friendly chains adjacent to each liberty
		   (dolist (neighbor (g-value liberty :adjacent))
			   (when (and neighbor
				      (eq (g-value neighbor :color) (g-value chain :color))
				      (not (g-value neighbor :chain :tag)))
				 (s-value (g-value neighbor :chain) :tag T) ; Tag the friendly chain so it won't be processed again
				 (dolist (additional-liberty		    ; Add the friendly chain's liberties to RESULT
					  (g-value neighbor :chain :liberties))
					 (unless (position additional-liberty result)
						 (setq done NIL)
						 (vector-push additional-liberty result))))))))
    (dolist (chain *chains*)						    ; Remove all tags
	    (s-value chain :tag NIL))
    result))


;; Method

(define-method :unconditionally-alive proto-chain (self)
  "A maximax procedure.  Returns T if the chain is unconditionally alive, otherwise NIL."
  ;; Since I wrote this, someone told me about Benson's algorithm, described in Information Sciences 10, pp. 17-29.  It's much
  ;; better than this, but would require some changes to data structures.  Installing that might be a good piece of practice in
  ;; Go programming.
  (let* ((next-player (g-value board-data :player))
	 (master-move-array (find-moves-to-attack self))
	 (result-of-first-pass (make-first-attacking-pass master-move-array self))
	 (result))
    (setf
     result
     (cond
      ((eq result-of-first-pass :captured)				    ; If the chain was captured, undo the moves
       (undo-attacking-moves master-move-array 0)
       NIL)								    ;  and return NIL
      ((or (eq result-of-first-pass :two-eyes)				    ; If the chain has two eyes,
	   (not (position :tricky master-move-array :key #'point-tag)))	    ;  or if there are no tricky moves
       (undo-attacking-moves master-move-array 0)			    ;  undo the moves
       T)								    ;  and return T
      (T								    ; Otherwise, there are tricky moves.
       (do* ((previous-move-array
	      (copy-move-array master-move-array))
	     (permutation-number 0 (1+ permutation-number))
	     (move-array
	      (setf master-move-array (stable-sort master-move-array #'easy-first)))
	     (branch-point
	      (if (not (equal move-array previous-move-array))
		  (mismatch move-array previous-move-array)
		(fill-pointer move-array)))
	     (first-tricky (if (position :tricky move-array :key #'point-tag)
			       (position :tricky move-array :key #'point-tag)
			     (1- (fill-pointer move-array))))
	     (last-tricky (if (position :eye move-array :key #'point-tag)
			      (1- (position :eye move-array :key #'point-tag))
			    (1- (fill-pointer move-array))))
	     (permutation-point last-tricky)
	     (max-permutations (factorial (- last-tricky first-tricky -1)))
	     (captured NIL))
	    ((or captured						    ; Repeat until the chain is captured or
		 (= permutation-number max-permutations))		    ;  there are no more permutations to try
	     (progn
	       (undo-attacking-moves previous-move-array 0)
	       (not captured)))						    ; Return T if alive, otherwise NIL
	    (unless (= permutation-number 0)				    ; These needn't be evaluated on the first or last pass
		    (setf previous-move-array				    ;  (when the loop is exited) through the loop variables.
			  (copy-move-array move-array))
		    (setf move-array (permutation permutation-point move-array))
		    (setf branch-point (mismatch move-array previous-move-array))
		    (setf permutation-point
			  (if (= (decf permutation-point) first-tricky)
			      last-tricky
			    permutation-point)))
	    (undo-attacking-moves previous-move-array branch-point)
	    (do ((previous-last-move NIL))
		((or captured (eq previous-last-move (car (g-value (car (g-value board-data :history)) :chain :stones)))))
		(setf previous-last-move (car (g-value (car (g-value board-data :history)) :chain :stones)))
		(setf captured (make-attacking-pass move-array self branch-point)))))))
    (s-value board-data :player next-player)
    (dotimes (i (fill-pointer master-move-array))
	     (s-value (aref master-move-array i) :tag NIL))
    result))


;;; Exported functions

;; Functions used by exported functions

(defun create-board-picture ()
  "Creates BOARD-AGGREGATE and the pieces of the board picture:  BOARD-BACKGROUND"
  (create-instance							    ; Create an aggregate for the picture of the board
   'board-aggregate opal:aggregate)
  (create-instance							    ; The orange (or gray) background for the board
   'board-background opal:rectangle
   (:line-style NIL)							    ;  No outline
   (:filling-style
    (if (g-value opal:color :color-p)
	opal:orange-fill
      opal:light-gray-fill))
   (:top *stone-offset*)
   (:left *stone-offset*)
   (:width *board-width*)
   (:height *board-width*))
  (opal:add-component board-aggregate board-background)			    ; Add the orange background to BOARD-AGGREGATE
  (let ((here *stone-width*)						    ; Add the lines to BOARD-AGGREGATE
	(there *board-width*)
	(i 0))
    (dotimes (row *board-lines*)
	     (incf i *stone-width*)
	     (opal:add-components board-aggregate
				  (create-instance
				   NIL proto-line
				   (:x1 i) (:y1 here) (:x2 i) (:y2 there))
				  (create-instance
				   NIL proto-line
				   (:x1 here) (:y1 i) (:x2 there) (:y2 i)))))
  (dolist (point (case *board-lines*					    ; Add the handicap points to BOARD-AGGREGATE
		       (5 '((2 . 2)))
		       (9 '((2 . 2) (2 . 6) (4 . 4) (6 . 2) (6 . 6)))
		       (13 '((3 . 3) (3 . 6) (3 . 9) (6 . 3) (6 . 6) (6 . 9) (9 . 3) (9 . 6) (9 . 9)))
		       (19 '((3 . 3) (3 . 9) (3 . 15) (9 . 3) (9 . 9) (9 . 15) (15 . 3) (15 . 9) (15 . 15)))
		       (t NIL)))
		 (opal:add-component board-aggregate
				     (create-instance
				      NIL proto-handicap-point
				      (:left (+ *stone-width*
						(- (* (car point) *stone-width*) *handicap-point-offset*)))
				      (:top (+ *stone-width*
					       (- (* (cdr point) *stone-width*) *handicap-point-offset*)))))))

(defun create-aggregates ()
  "Create TOP-AGGREGATE and the aggregates which will contain the graphical stones.  *ROW-AGGREGATES* will be a list of these."
  ;; David Kosbie pointed out that, since aggregates are linked lists, it's best not to make one big aggregate containing all of these
  ;; graphical stones -- this would require going through all of them to get to the lower right.  With a list of rows, only the row in
  ;; question need be traversed.
  (create-instance 'top-aggregate opal:aggregate)			    ; Create a master aggregate for all graphic objects and add it
									    ;  to *OBJECTS*
  (opal:add-component top-aggregate board-aggregate)			    ; Add the picture of the board to TOP-AGGREGATE
  (dotimes (row *board-lines*)						    ; For each element of *ROW-AGGREGATES*
	   (opal:add-component top-aggregate				    ;  create an aggregate and attach it to TOP-AGGREGATE
			       (setf (svref *row-aggregates* row)
				     (create-instance
				      NIL opal:aggregate)))))

(defun create-keyboard-accelerators ()
  "Create hot keys to undo with 'u' and pass with 'p'."
  (create-instance
   'undo-key inter:button-interactor
   (:continuous NIL)
   (:start-where `(:in ,board-background))
   (:start-event #\u)
   (:final-function
    #'(lambda (button object-over)
	(declare (ignore button object-over))
	(kr-send board-data :undo))))
  (create-instance
   'pass-key inter:button-interactor
   (:continuous NIL)
   (:start-where `(:in ,board-background))
   (:start-event #\p)
   (:final-function
    #'(lambda (button object-over)
	(declare (ignore button object-over))
	(kr-send board-data :pass)))))

(defun create-buttons ()
  "Create the buttons which deal with mouse clicks: STONE-SETTER and LIFE-CHECKER."
  (create-instance							    ; The big "button" that turns mouse-clicks into moves
   'stone-setter inter:button-interactor
   (:continuous NIL)
   (:start-where `(:in ,board-background))
   (:final-function							    ; When a point is clicked on
    #'(lambda (button object-over)
	(declare (ignore button object-over))
	(let ((player (g-value board-data :player))
	      (row (pixel-to-row (inter::event-y inter::*current-event*)))
	      (column (pixel-to-row (inter::event-x inter::*current-event*))))
	  (if (kr-send board-data :legal-move row column player)	    ;  If the move is legal
	      (kr-send board-data :play-at row column player)		    ;   play it
	    (inter:beep))))))						    ;   otherwise beep
  (define-method :play-at stone-setter (row column player)
    ;; Creates a graphical stone at ROW and COLUMN, for PLAYER (either :black or :white).  Returns the stone object.  This is called by
    ;; the :PLAY-AT method of BOARD-DATA, so the move is assumed to be legal.
    (opal:add-component
     (svref *row-aggregates* row)					    ;  add to the row aggregates
     (create-instance							    ;   a new stone
      NIL proto-stone
      (:left (+ *stone-width* (- (* column *stone-width*) *stone-offset*)))
      (:top (+ *stone-width* (- (* row *stone-width*) *stone-offset*)))
      (:filling-style (if (eq player :black) opal:black-fill opal:white-fill)))))
  (create-instance							    ; A similar "button", using the right button, checking life
   'life-checker inter:button-interactor
   (:continuous NIL)
   (:start-where `(:in ,board-background))
   (:start-event :rightdown)
   (:final-function
    #'(lambda (button object-over)
	(declare (ignore button object-over))
	(let* ((row (pixel-to-row (inter::event-y inter::*current-event*)))
	       (column (pixel-to-row (inter::event-x inter::*current-event*)))
	       (chain (g-value (aref (g-value board-data :points) row column) :chain)))
	  (if chain							    ; If there's a stone there
	      (if (kr-send chain :unconditionally-alive chain)		    ;  tell if it's alive
		  (message-box (format NIL "Yup, it's alive"))
		(message-box (format NIL "It's not UNCONDITIONALLY alive...")))
	    (inter:beep)))))))						    ;  otherwise beep

(defun create-menubar ()
  "Creates the menubar for GO-WINDOW."
  (create-instance			; Create a menubar
      'go-menu garnet-gadgets:menubar
    (:items
     `(("Game " NIL
		(("About CarGo"
		  ,#'(lambda (g m s)
		       (declare (ignore g m s))
		       (message-box
			(format NIL "CarGo is copyright (c) 1993 by Peter Dudey~2%~
I'm distributing this mainly as a demonstration for those interested~%~
writing Go programs and using Garnet.  There are better free Go~%~
programs to be had;  I recommend Igo, a crippled version of David~%~
Fotland's \"Many Faces of Go\".  See rec.games.go for info on such~%~
programs.~2%~
Permission is granted to distribute this program without changes.~%~
Altered versions may be distributed, so long as:~%~
-They are be explained in a \"Changes\" submenu of \"Game\".~%~
-The name is changed, e.g., to \"ConsGo, incorporating CarGo\".~%~
-CarGo is mentioned in any \"About\" screens/windows.~2%~
CdrGo, a program which will play against the user, and hopefully~%~
learn, should be available by sometime in 1995, unless I switch to~%~
another language...  (It's my Master's project).  I reserve exclusive~%~
rights to the names CarGo and CdrGo.~2%~
Have fun!~2%~
I can be reached at dudeyp@research.cs.orst.edu."))))
		 ("Mouse Buttons"
		  ,#'(lambda (g m s)
		       (declare (ignore g m s))
		       (message-box
			(format NIL "Clicking on a point with the LEFT button plays there if it's legal,~%~
or beeps if it isn't.~2%~
Clicking on a stone with the RIGHT button tells if the stone in question~%~
is UNCONDITIONALLY alive, i.e., can never be captured."))))
		 ("What is Go?"
		  ,#'(lambda (g m s)
		       (declare (ignore g m s))
		       (message-box
			(format NIL "Go is an ancient oriental boardgame -- by some accounts, the oldest~%~
boardgame still played.~2%~
The rules are fairly simple, but better explained in person than in~%~
text.  Computer scientists tend to play Go, so you can probably find~%~
a player in your local CS department.  (Show them the Game/Mouse menu).~2%~
CarGo uses Chinese counting (score = stones + territory), and three passes~%~
end the game.~2%~
The game is also discussed on the newsgroup rec.games.go."))))
		 ("New Game" ,#'(lambda (g m s)
				  (declare (ignore g m s))
				  (do-go)))
		 ("Quit" ,#'(lambda (g m s)
			      (declare (ignore g m s))
			      (do-stop)))))
       ("Board Size " ,#'(lambda (g m s)
			   (declare (ignore g m))
			   (when (cond
				   ((and (equal s "5x5") (not (= *board-lines* 5)))
				    (setq *board-lines* 5))
				   ((and (equal s "9x9") (not (= *board-lines* 9)))
				    (setq *board-lines* 9))
				   ((and (equal s "13x13") (not (= *board-lines* 13)))
				    (setq *board-lines* 13))
				   ((and (equal s "19x19") (not (= *board-lines* 19)))
				    (setq *board-lines* 19))
				   (T NIL))
			     (do-stop)
			     (load "contrib:cargo") ; Several objects need to be reconstructed, so it's
			     (do-go)))	            ; easiest to just reload and restart
		      (("5x5" NIL)
		       ("9x9" NIL)
		       ("13x13" NIL)
		       ("19x19" NIL)))
       ("Move " NIL
		(("Pass       [p]" ,#'(lambda (g m s)
					(declare (ignore g m s))
					(kr-send board-data :pass)))
		 ("Undo Move  [u]" ,#'(lambda (g m s)
					(declare (ignore g m s))
					(kr-send board-data :undo))))))))
  (opal:add-component top-aggregate go-menu)
  (opal:notice-items-changed go-menu)
  (opal:update go-window))


;; Exported functions

(defun do-go ()
  "The main function for CarGo.  It creates all of the initial objects, which should then handle themselves."
  (do-stop)				; Destroy everything in *OBJECTS*
  (create-board-picture)		; Create grid, etc.
  (create-aggregates)			; Create TOP-AGGREGATE and *ROW-AGGREGATES*
  (create-keyboard-accelerators)
  (push (create-instance 'board-data proto-board-data) *objects*) ; Create BOARD-DATA and add it to *OBJECTS*
  (create-buttons)		      ; Create full-board "buttons" that deal with mouse clicks
  (create-instance		      ; Create a window
      'go-window inter:interactor-window
    (:width *window-width*)
    (:height *window-width*)
    (:title "CarGo")
    (:aggregate top-aggregate))
  (s-value stone-setter :window go-window) ; Tell the STONE-SETTER button what window it's in
  (s-value life-checker :window go-window) ; Tell the LIFE-CHECKER button what window it's in
  (s-value pass-key :window go-window)
  (s-value undo-key :window go-window)
  (opal:update go-window)     ; Draw the window
  (create-menubar)	      ; Create the menubar
  (push go-window *objects*)  ; Add the window to the list of things to be destroyed by do-stop
  NIL)			      ; Returns NIL

(defun do-stop ()
  ;;
  ;; Destroy everything listed in *OBJECTS*.
  ;;
  (dolist (object *objects*)
    (if (schema-p object)
	(opal:destroy object)))
  (setq *objects* NIL)
  (setq *chains* NIL))

