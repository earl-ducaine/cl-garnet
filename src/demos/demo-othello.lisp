;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-OTHELLO; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$



#||
			DEMO-OTHELLO

           ** THIS VERSION WORKS WITH MGE EDITOR **

                   Written by David Kosbie
||#


(in-package :DEMO-OTHELLO)

(declaim (special W TOP-AGG TITLE TITLE-LINE PLAYER1-SCORE PLAYER2-SCORE
		  CONTROL-PANEL SCROLL-TEXT1 SCROLL-TEXT2 MESSAGE-BOX
		  MESSAGES-HEADER MESSAGES-UNDERLINE MESSAGE OUTER-SQUARE))

(dolist (gadget '("v-scroll-loader" "menu-loader"))
  (common-lisp-user::garnet-load (concatenate 'string "gadgets:"
                                  gadget)))

(unless (get :garnet-modules :mge)
  (format t "Loading MGE editor...~%")
  (common-lisp-user::garnet-load (concatenate 'string "demos:" 
                                  "mge"))
  (setf (get :garnet-modules :mge) t))


(defmacro half (n)
  `(round (/ ,n 2.0)))

(defvar *game-started* NIL)

(defparameter *max-size*	16)	;; no more than 16 x 16 board!
(defparameter *rect-count*	(* *max-size* *max-size*))
(defparameter *rectangles*	NIL)	;; array of the rectangles

(defvar board-array NIL)		;; 2-d array of the rectangles

(defparameter *window-left*	225)
(defparameter *window-top*	40)
(defparameter *window-width*	800)
(defparameter *window-height*	606)
(defparameter *window-center*	(half *window-width*))
(defparameter *title-width-offset*	10)
(defparameter *title-height-offset*	5)
(defparameter *title-line-x-offset*	19)
(defparameter *title-line-y-offset*	20)

(defparameter *rightmost-box-point*	190)
(defparameter *score-box-piece-width*    20)
(defparameter *score-box-x-indent*	  5)
(defparameter *score-box-y-indent*	  5)

(defvar *score-box-bottom*  10)		;; This will be set in Make-Score-Box

(defvar *inter-box-y-offset* 10)	;;; This will be set in Make-Scroll-Bar

(defvar *test-debug* NIL)
(defvar scroll-bar NIL)

(defparameter *scroll-bar-x-offset* 20)
(defparameter *scroll-bar-height*  190)
(defparameter *scroll-bar-width*    21)

(defparameter *message-box-height*   75)
(defparameter *messages-indentation* 10)

(defparameter *panel-x-indent*	 20)
(defparameter *panel-y-indent*	 20)

(defparameter *board-x-offset*	10)
(defparameter *board-offset*	10)

(defparameter *mat-left*	(+ *rightmost-box-point* *board-x-offset*))
(defparameter *mat-right*	(- *window-width* (* 2 *title-line-x-offset*)))

(defparameter *board-left*	(+ *mat-left* *board-offset*))
(defparameter *board-right*	(- *mat-right* *board-offset*))
(defparameter *board-width*	(- *board-right* *board-left*))

(defvar *topmost-box-point* NIL)
(defvar *bottommost-box-point* NIL)
(defvar *mat-top* NIL)
(defvar *mat-bottom* NIL)
(defvar *board-top* NIL)
(defvar *board-height* NIL)
(defvar *board-bottom* NIL)
(defvar *player1-text* NIL)
(defvar *player2-text* NIL)
(defvar *score-box* NIL)

;;
;; Definitions moved out of Preliminaries function
;;
(defparameter *title-font* (create-instance NIL opal:font
	                      (:family :serif)
			      (:face :bold-italic)
			      (:size :very-large)))
(defparameter *standard-font* (create-instance NIL opal:font
			         (:family :serif)
				 (:face :bold)
				 (:size :large)))
(defparameter *control-panel-font* (create-instance NIL opal:font
				      (:size :large)))
(defparameter *squares* (create-instance NIL opal:aggregate))
(defparameter *scroll-bar-font* (create-instance NIL opal:font
			           (:family :serif)
				   (:face :roman)
				   (:size :small)))



(defun Preliminaries ()
  (setq *game-started* NIL)
  (setq board-array (make-array (list *max-size* *max-size*)))
  (setq *rectangles* (make-array *rect-count*))
  (dotimes (index *rect-count*)
    (let ((rect (create-instance NIL opal:rectangle
				(:select-outline-only NIL))))
      (setf (aref *rectangles* index) rect)
      (opal:add-component *squares* rect))))


(defun Make-Window-And-Title ()
  (create-instance 'w inter:interactor-window
				  (:left   *window-left*)
				  (:top    *window-top*)
				  (:width  *window-width*)
				  (:height *window-height*)
				  (:title "Demo-Othello")
				  (:icon-title "Othello"))
  (s-value w :aggregate (create-instance 'top-agg opal:aggregate))

  (opal:update w)
  (create-instance 'title opal:text
	(:string "Garnet Othello")
	(:font *title-font*))
  (opal:add-component top-agg title)
  (let* ((title-height (g-value title :height))
	 (title-width  (g-value title :width))
	 (title-left   (- *window-center* (half title-width)))
	 (title-right  (+ title-left title-width))
	 (title-frame-left       (- title-left *title-width-offset*))
	 (title-frame-right      (+ title-right *title-width-offset*))
	 (title-frame-mid-height (+ *title-height-offset* (half title-height)))
	 (title-line-right	 (- *window-width* *title-line-x-offset*))
	 (title-line-bottom	 (- *window-height* *title-line-y-offset*)))
	(setq *topmost-box-point* (+ title-height (* 2 *title-height-offset*)))
	(setq *bottommost-box-point*	(- *window-height*
					   (* 2 *title-line-y-offset*)))
	(setq *mat-top*	*topmost-box-point*)
	(setq *board-top*	(+ *mat-top* *board-offset*))
	(setq *mat-bottom*	*bottommost-box-point*)
	(setq *board-bottom*	(- *mat-bottom* *board-offset*))
	(setq *board-height*	(- *board-bottom* *board-top*))
	(s-value title :left title-left)
	(s-value title :top *title-height-offset*)
	(create-instance 'title-line opal:polyline
	  (:constant T)
	  (:point-list
	    (list
		title-frame-left	title-frame-mid-height
		*title-line-x-offset*	title-frame-mid-height
		*title-line-x-offset*	title-line-bottom
		title-line-right	title-line-bottom
		title-line-right	title-frame-mid-height
		title-frame-right	title-frame-mid-height
	    )))
	(opal:add-component top-agg title-line))
)

(defun Make-Score-Box ()
  (let* ((score-box-top  *topmost-box-point*)
 	 (score-box-left (* 2 *title-line-x-offset*))
	 (player-text-left (+ score-box-left
			      *score-box-piece-width*
			      (* 2 *score-box-x-indent*)))
	 (player1-top    (+ score-box-top *score-box-y-indent*))
	 player2-top)
    (setq *player1-text* (create-instance NIL opal:text
			    (:constant T)
			    (:string "Player 1")
			    (:font *standard-font*)
			    (:left player-text-left)
			    (:top  player1-top)))
    (opal:add-component top-agg *player1-text*)
    (setq *player2-text* (create-instance NIL opal:text
			    (:constant T)
			    (:string "Player 2")
			    (:font *standard-font*)
			    (:left player-text-left)
			    (:top (setq player2-top
					(+ player1-top
					   (g-value *player1-text* :height)
					   *score-box-y-indent*)))))
    (opal:add-component top-agg *player2-text*)
    (setq *score-box* (create-instance NIL opal:rectangle
			 (:constant T)
			 (:top    score-box-top)
			 (:left   score-box-left)
			 (:width  (- *rightmost-box-point* score-box-left))
			 (:height (- (+ player2-top
					(g-value *player2-text* :height)
					*score-box-y-indent*)
				     score-box-top))))
    (opal:add-component top-agg *score-box*)
    (create-instance 'player1-score opal:text
       (:string "0")
       (:font *standard-font*)
       (:left (o-formula (- *rightmost-box-point*
			    *score-box-x-indent*
			    (gvl :width))))
       (:top player1-top))
    (create-instance 'player2-score opal:text
       (:string "0")
       (:font *standard-font*)
       (:left (o-formula (- *rightmost-box-point*
			    *score-box-x-indent*
			    (gvl :width))))
       (:top player2-top))
    (opal:add-components top-agg player1-score player2-score)
    (setq *score-box-bottom* (+ score-box-top (g-value *score-box* :height)))
    )
  )

(defun Make-Control-Panel ()
	(opal:add-component top-agg
		(create-instance 'Control-Panel garnet-gadgets:menu
				 (:constant T)
				 (:left (* 2 *title-line-x-offset*))
				 (:top 425)
				 (:item-font *control-panel-font*)
				 (:items '(("Start" start-game)
					   ("Pass" skip-turn)
					   ("Stop" stop-game)
					   ("Quit" do-quit))))))

;;;---------------------------------
;;; Do-Go
;;;---------------------------------

(defun Make-Scroll-Bar ()

 (setq *inter-box-y-offset*
   (round (/ (- (g-value Control-Panel :top)
	        *score-box-bottom*
		*scroll-bar-height*
		*message-box-height*)
	     3.0)))

 (let* ((scroll-bar-top  (- (g-value Control-Panel :top)
			    *inter-box-y-offset*
			    *scroll-bar-height*))
	(scroll-bar-left (+ (g-value Control-Panel :left)
			    *scroll-bar-x-offset*))
	(scroll-text-top  (+ scroll-bar-top
			     (round (/ *scroll-bar-height* 3.0))))
	(scroll-text-left (+ scroll-bar-left
			     *scroll-bar-width*
			     *scroll-bar-x-offset*)))
   (create-instance 'Scroll-Bar garnet-gadgets:v-scroll-bar
		    (:constant T)
		    (:left scroll-bar-left)
		    (:top scroll-bar-top)
		    (:width *scroll-bar-width*) 
		    (:height *scroll-bar-height*)
		    (:scr-incr 2)
		    (:page-trill-p nil)
		    (:val-1 2)
		    (:val-2 16)
		    (:indicator-font *scroll-bar-font*))
   (s-value scroll-bar :value 8)

  (create-instance 'scroll-text1 opal:text
	(:string "Next Game:")
	(:font  *scroll-bar-font*)
	(:left  scroll-text-left)
	(:top   scroll-text-top))
  (create-instance 'scroll-text2 opal:text
	(:string
	  (o-formula
	    (let ((nstring (princ-to-string (gv scroll-bar :value))))
		(concatenate 'string nstring " x " nstring))))
	(:font *scroll-bar-font*)
	(:left
	  (o-formula
	    (+ (gv scroll-text1 :left)
	       (half (- (gv scroll-text1 :width)
			(gvl :width))))))
	(:top
	  (o-formula
	    (+ (gv scroll-text1 :top)
	       (gv scroll-text1 :height)
	       8))))
  (opal:add-components top-agg scroll-bar scroll-text1 scroll-text2)))

(defun Make-Message-Box ()
  (let* ((message-box-top   (+ *score-box-bottom* *inter-box-y-offset*))
	 (message-box-left  (g-value *score-box* :left))
	 (message-box-width (- *rightmost-box-point* message-box-left))
	 (messages-left  (+ message-box-left *messages-indentation*))
	 messages-top
	 (messages-bottom (+ message-box-top *message-box-height*)))
    (create-instance 'Message-Box opal:rectangle
        (:constant T)
	(:top message-box-top)
	(:left message-box-left)
	(:width message-box-width)
	(:height *message-box-height*))
    (create-instance 'messages-header opal:text
	(:string "Comments:")
	(:left messages-left)
	(:top (+ message-box-top 10)))
    (opal:add-components top-agg message-box messages-header)
    (setq messages-top (+ (g-value messages-header :top)
			  (g-value messages-header :height)
			  3))
    (create-instance 'messages-underline opal:line
        (:constant T)
	(:x1 messages-left)
	(:x2 (+ messages-left (g-value messages-header :width)))
	(:y1 messages-top)
	(:y2 messages-top))
    (opal:add-component top-agg messages-underline)
    (create-instance 'message opal:text
	(:string "Player 1's move")
	(:top (o-formula
		(+ messages-top (half (- messages-bottom
					 messages-top
					 (gvl :height))))))
	(:left (o-formula
		(+ message-box-left (half (- message-box-width
					     (gvl :width)))))))
    (opal:add-component top-agg message)))

(defvar board NIL)
(defvar board-inter NIL)
(defvar *current-player* 1)	;;; This is either 1 or 2
(defvar *other-player* 2)	;;; This is either 1 or 2
(defvar *scores* NIL)		;;; array of the two folks' scores

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; THE ACTUAL GAME... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Set-Scores ()
  (set-score 1 (aref *scores* 1))
  (set-score 2 (aref *scores* 2))
)

(defun Flip (square)
 (let* ((old-player (g-value square :player))
	(new-player (- 3 old-player)))
  (incf (aref *scores* new-player) 1)
  (decf (aref *scores* old-player) 1)
  (s-value square :player new-player)
  (opal:update w)
))

(defun Make-Game-Piece-Pair (row column player)
  (let* ((square (aref board-array row column))
;;*BAM*	 (piece1 (create-instance NIL game-piece-1
;;*BAM*	 		(:left   (+ (g-value square :left) 2))
;;*BAM*	 		(:top    (+ (g-value square :top)  2))
;;*BAM*	 		(:width  (- (g-value square :width) 4))
;;*BAM*	 		(:height (- (g-value square :height) 4))
;;*BAM*			(:visible (o-formula (eq (gvl :square :player) 1)))
;;*BAM*	 		(:square square)))
;;*BAM*	 (piece2 (create-instance NIL game-piece-2
;;*BAM*	 		(:left   (+ (g-value square :left) 2))
;;*BAM*	 		(:top    (+ (g-value square :top)  2))
;;*BAM*	 		(:width  (- (g-value square :width) 4))
;;*BAM*	 		(:height (- (g-value square :height) 4))
;;*BAM*			(:visible (o-formula (eq (gvl :square :player) 2)))
;;*BAM*	 		(:square square))))
	 (piece1 (mge:create-piece (+ (g-value square :left) 2)  ;left
				   (+ (g-value square :top)  2)  ;top
				   (- (g-value square :width) 4) ; width
				   (- (g-value square :height) 4) ; height
				   1))
         (piece2 (mge:create-piece (+ (g-value square :left) 2)  ;left
				   (+ (g-value square :top)  2)  ;top
				   (- (g-value square :width) 4) ; width
				   (- (g-value square :height) 4) ; height
				   2)))
    (s-value piece1 :square square)
    (s-value piece2 :square square)
    (s-value piece1 :visible (o-formula (eq (gvl :square :player) 1)))
    (s-value piece2 :visible (o-formula (eq (gvl :square :player) 2)))

    (opal::add-components board piece1 piece2)
    (s-value square :player player)))
  
(defmacro Out-Of-Bounds (row col n)
  `(or (>= ,row ,n) (>= ,col ,n) (< ,row 0) (< ,col 0)))

(defun Legal-Move? (x-off y-off base-row base-col n flip-p)
  (let ((row (+ base-row x-off))
	(col (+ base-col y-off))
	square)
    (if (or (out-of-bounds row col n)
	    (not (eq (g-value (setq square (aref board-array row col)) :player)
		     *other-player*)))
	(return-from Legal-Move? NIL))
    (if flip-p (flip square))
    (loop
	(incf row x-off)
	(incf col y-off)
	(if (out-of-bounds row col n) (return-from Legal-Move? NIL))
        (if (eq (g-value (setq square (aref board-array row col)) :player)
		*current-player*)
	    (return-from Legal-Move? T)
	    (if (null (g-value square :player))  ;; empty square
		(return-from Legal-Move? NIL)
		(when flip-p (flip square)))))))

(defun Process-Button-Press (interactor square-selected)
  (declare (ignore interactor))
  (let ((illegal-move T)
	(base-row (g-value square-selected :row))
	(base-col (g-value square-selected :column))
	(n (g-value board :n))
	temp)
    (unless (g-value square-selected :player)
	(dolist (y-off '(-1 0 1))
	  (dolist (x-off '(-1 0 1))
	    (unless (and (zerop y-off) (zerop x-off))
	      (when (legal-move? x-off y-off base-row base-col n NIL)
		(legal-move? x-off y-off base-row base-col n T)
		(setq illegal-move NIL)))))
	)
    (if illegal-move
      (progn
	(s-value message :string
	  (if (eq *current-player* 1)
		"Try Again, #1"
		"Try Again, #2"))
	(inter:beep)
	(inter:beep)
	)
      (progn
	(s-value square-selected :player *current-player*)
	(Make-Game-Piece-Pair base-row base-col *current-player*)
	(opal:update w)
	(incf (aref *scores* *current-player*))
	(setq temp *current-player*)
	(setq *current-player* *other-player*)
	(setq *other-player* temp)
	(set-scores)
	(opal:update w)
	(s-value message :string
		(if (eq *current-player* 1) "Player 1's move"
					    "Player 2's move"))
	))))

(defun Skip-Turn (&optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (let ((temp *current-player*))
    (setq *current-player* *other-player*)
    (setq *other-player* temp)
    (set-scores)
    (opal:update w)
    (s-value message :string
	     (if (eq *current-player* 1) "Player 1's move"
		                         "Player 2's move"))
    ))

(declaim (special score-box-game-pieces))

(defun Stop-Game (&optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (when *game-started*
    (mge:destroy-all-pieces)
    (opal:remove-component top-agg board)
    (opal:remove-component top-agg score-box-game-pieces)
    (opal:update w)
    (if (member *squares* (g-local-value board :components))
	(opal:remove-component board *squares*))
    (opal:destroy board)
    (opal:destroy board-inter)
    (opal:destroy score-box-game-pieces)
    (setq *game-started* NIL)
  )
)

(defun Start-Game (&optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (let ((n (g-value scroll-bar :value))
	(rect-index 0)
	(row-left *board-left*)
	(row-end 0)
	(old-row-end 0)
        (row-width 0)
        col-top col-end old-col-end temp
	active-list
	(piece1-size (min *score-box-piece-width*
			  (g-value *player1-text* :height)))
	(piece2-size (min *score-box-piece-width*
			  (g-value *player2-text* :height)))
	(pieces-base-left (+ (* 2 *title-line-x-offset*)
			     *score-box-x-indent*))
	score-box-piece-1
	score-box-piece-2
	)
    (if *game-started* (Stop-Game))
    (setq *game-started* T)
    (create-instance 'score-box-game-pieces opal:aggregate)
    (setq score-box-piece-1
	  (mge:create-piece (+ pieces-base-left (half (- *score-box-piece-width*
							 piece1-size)))  ; left
			    (g-value *player1-text* :top) ; top
			    piece1-size
			    piece1-size
			    1))
    (setq score-box-piece-2
	  (mge:create-piece (+ pieces-base-left (half (- *score-box-piece-width*
							 piece2-size)))  ; left
			    (g-value *player2-text* :top) ; top
			    piece2-size
			    piece2-size
			    2))

    (opal:add-components score-box-game-pieces score-box-piece-1
					       score-box-piece-2)

    (create-instance 'board opal:aggregate)

		;; These are the 4 rectangles which compose the outside
		;; dark gray region around the board...

    (dolist (rect (list (list *mat-top* *mat-bottom* *mat-left* *board-left*)
			(list *mat-top* *mat-bottom* *board-right* *mat-right*)
			(list *mat-top* *board-top* *board-left* *board-right*)
			(list *board-bottom* *mat-bottom*
					 *board-left* *board-right*)))
	(let ((top  (first rect))
	      (left (third rect)))
	  (opal:add-component board (create-instance NIL opal:rectangle
				(:line-style NIL)
				(:filling-style opal:dark-gray-fill)
				(:left left)
				(:top top)
				(:width (- (fourth rect) left))
				(:height (- (second rect) top))))))
    (create-instance 'outer-square opal:rectangle
				(:left *mat-left*)
				(:top  *mat-top*)
				(:width (- *mat-right* *mat-left*))
				(:height (- *mat-bottom* *mat-top*)))
    (opal:add-component board outer-square)
    (s-value board :n n)

		;; Now we must create the N x N squares...
    (dotimes (row n)
      (incf row-left row-width)
      (setq old-row-end row-end)
      (setq row-end (floor (* (/ (1+ row) n) *board-width*)))
      (setq row-width (- row-end old-row-end))
      (setq col-top *board-top*)
      (setq old-col-end (setq col-end 0))
      (dotimes (col n)
        (incf col-top (- col-end old-col-end))
	(setq old-col-end col-end)
        (setq col-end (floor (* (/ (1+ col) n) *board-height*)))
	(setq temp (aref *rectangles* rect-index))
	(push temp active-list)
	(incf rect-index 1)
	(s-value temp :left row-left)
	(s-value temp :width row-width)
	(s-value temp :top col-top)
	(s-value temp :height (- col-end old-col-end))
	(s-value temp :row row)
	(s-value temp :column col)
	(s-value temp :player NIL)
	(s-value temp :visible T)
	(setf (aref board-array row col) temp)))
    (s-value *squares* :active-list active-list)

		;; Set all the remaining rectangles to be invisible
    (do ((x rect-index (1+ x)))
	((>= x *rect-count*))
	(s-value (aref *rectangles* x) :visible NIL))

    (opal:add-component board *squares* :front)

    (let* ((hi-square (/ n 2))
	   (lo-square (1- hi-square)))
	(Make-Game-Piece-Pair lo-square lo-square 1)
	(Make-Game-Piece-Pair hi-square hi-square 1)
	(Make-Game-Piece-Pair lo-square hi-square 2)
	(Make-Game-Piece-Pair hi-square lo-square 2))

    (s-value message :string "Player 1's move")
    (setq *current-player* 1)
    (setq *other-player* 2)
    (setq *scores* (make-array 3))
    (setf (aref *scores* 1) 2)
    (setf (aref *scores* 2) 2)
    (set-scores)
    (opal:add-components top-agg board score-box-game-pieces)

    (create-instance 'board-inter inter:button-interactor
		(:start-where `(:list-element-of ,*squares* :active-list))
		(:window w)
		(:stop-action #'process-button-press))

    ;; For some reason, if we don't include this, the update takes forever!
    (opal:update w T)
  )
)

(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  (declare (ignore double-buffered-p))
  (mge:do-go)
  (unless board-array  ;; Must only do preliminaries very
    (Preliminaries))   ;; first time! -- ECP 2/27/92
  (Make-Window-And-Title)
  (Make-Score-Box)
  (Make-Control-Panel)
  (Make-Scroll-Bar)
  (Make-Message-Box)
  (Start-Game)
  ;;if not CMU CommonLisp, then enter the main event loop for look for events
  (unless dont-enter-main-event-loop  #-cmu (inter:main-event-loop))
)

(defun Set-Score (player score)
  (unless (numberp score) (setq score 0))
  (let  ((score-string (princ-to-string score)))
	(if (eq player 1)
		(s-value player1-score :string score-string)
		(s-value player2-score :string score-string))))

(declaim (special mge::w))

(defun Do-Stop (&optional arg1 arg2)
  (declare (ignore arg1 arg2))
  (when (and (boundp 'w) ; only destroy if my window exists
	     (schema-p w))
    (stop-game)
    (opal:destroy w)
    (when (and (boundp 'mge::w)
	       (schema-p mge::w))
      (opal:destroy mge::w)))
  )


(defun Do-Quit (&optional inter obj)
  (declare (ignore inter obj))
  (Do-Stop)
  ;;for demo-controller
    (unless (and (fboundp 'Common-Lisp-User::Garnet-Note-Quitted)
		 (Common-Lisp-User::Garnet-Note-Quitted "DEMO-OTHELLO")))
)
