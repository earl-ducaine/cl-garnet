;;;; -*- Mode: Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : garnet-loop.lisp
;;;; Author          : Frank Ritter
;;;; Created On      : Thu Sep 27 14:10:14 1990
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Mon Nov 11 11:19:47 1991
;;;; Update Count    : 72
;;;; 
;;;; PURPOSE
;;;; 	A top level read-eval-print loop that allows lisp and Garnet to run
;;;; at the same time.
;;;; TABLE OF CONTENTS
;;;;	i. 	Variable definitions
;;;	ii. 	Non-user variables
;;;;	iii.	Special commands for the loop
;;;;	iv.	Helper functions
;;;;
;;;;	I. 	Garnet read-eval-print loop (grepl)
;;;;	II.	Garnet-event-and-lisp-loop
;;;;	III.	Garnet-lisp-repl
;;;;	IV.	Change to default-event-handler
;;;; 
;;;; Copyright 1990, Frank Ritter, permission to copy subject to the 
;;;; conditions below.
;;;; The material in this file is made available according to the
;;;; terms of the GNU LGPL, modified by the Lisp LGPL preamble.  Both
;;;; of those documents are available in the doc subdirectory of the
;;;; Garnet distribution.

;;;; 21Jun04 - per agreement with Frank Ritter, the license terms of
;;;; this file are clarified to be LLGPL. [2004/06/21:rpg]

(in-package "OPAL")

#|
A small package for running the core of Garnet (the guts of
inter:main-event-loop) and lisp (eval) concurrently, for all machines,
without using parallel processes.  It's not great, but the price is
right.  Feel free to use it subject to the Free Software Foundation's
copyleft agreement.  It should also be fairly extendable.  This is not
an official part of Garnet, and comes with no guarantees whatsoever
(although I find it tremendously useful).

To use it, call the function grepl in the opal package.

Within garnet loop the prompt appears as "<garnet:current-package-name>".
In addition to running the essentials of inter:main-event-loop, 
several other actions are supported through forms on *garnet-repl-conditions*.

Grepl works best if it has its input line buffered, as emacs does for
lisps running in a buffer.  Olin Shiver's cmulisp mode goes well with
grepl.

Grepl is a busy loop.  To make this less onerous, you can nice the
lisp process down or increase the sleep time.  I'm open to other
suggestions.  I don't notice a degredation on my workstation from
running this loop, but I may no longer notice it.

The header courtesy of software from Hucka@umich, file format taken 
from Milnes@cs.cmu.edu.

Frank Ritter@psy.cmu.edu, September 28, 1990
some comments by Pedro Szekely@venera.isi.edu, Ed Pervin@cs.cmu.edu
|#



;;;
;;;	i. 	Variable definitions and Export statements
;;;
;;; You can set these in your lisp init file (such as .clinit-cl for Allegro).
;;;
;;; *'s indicate variables most developers will have or may wish to change

(defvar grepl-supersystem-name "garnet"
  #-release-garnet
  "*Name of system using garnet, such as Soar; this appears in prompt.")

(defvar grepl-prompt-function 'print-grepl-prompt  ;no sharp before definition
  #-release-garnet
  "*Function to call to print prompt.  Expects a stream to be passed as arg.")

(defparameter grepl-loading-directory "/usr/"
  #-release-garnet
  "*Default dir to load files from.")

(defvar welcome-to-grepl "Welcome to garnet"
  #-release-garnet
  "*Message to print on entering grepl.")

(defvar rewelcome-to-grepl "Re-Welcome to garnet"
  #-release-garnet
  "*Message to print on reentering grepl after pop up from a break.")

(defvar goodbye-to-grepl "Say goodnight, Garnet"
  #-release-garnet
  "*Message to print on quiting grepl.")

(defvar grepl-loop-sleep-time .4
  #-release-garnet
  "*Time in seconds to sleep each time through the loop")


;;;
;;;	i.b	Export statements
;;;
;;;   This is really just a hedge in case this stuff gets moved to the inter
;;; package, and show I have hair on my chest and can use exports.

(export '(;; variables & schema
          goodbye-to-grepl
          grepl-supersystem-name
          grepl-history
          grepl-loop-sleep-time
          grepl-prompt-function
          grepl-loading-directory
          rewelcome-to-grepl
          welcome-to-grepl
          *grepl-conditions*
          
          ;; functions & macros
          clear-X-events
          garnet-event-and-lisp-loop
          grepl
          grepl-test
          update-all-windows
          )
   (find-package "OPAL"))



;;;
;;;	ii. 	Advanced user or Non-user variables
;;;

(defparameter *quit-grepl* nil 
  #-release-garnet
  "Flag used to quit the event-and-lisp-loop.")

(defparameter grepl-history nil
  #-release-garnet
  "Where the previous command lives")

(defparameter grepl-item nil
  #-release-garnet
  "Where the current command keyword lives")


;;;
;;;	iii.	Special commands for the loop
;;;
;;;     You can only push onto this list.
;;; The format is (test-form doc-string &rest things-to-do)
;;; The test-form gets evaled.  The doc-string is required.
;;; grepl-item is bound (in a deep sense) to the current input for tests.

(defvar *grepl-conditions*   '(

  ((or (equal :? grepl-item)
       (equal :help grepl-item))
   ":? or :help will give you a copy of all help messages."
   (print-grepl-help-message))

  ((grepl-test :quit quit)
  ":quit will quit the loop.  Otherwise all errors are caught and
return to garnet-loop at the top level rather than to the standard
lisp read-eval-print loop."
   (setq *quit-grepl* t)
   (throw 'exit-grepl t))

  ((equal :pack grepl-item)
   ":pack will set the package to the package corresponding to the string, atom, 
or evaluated literal expression that it is passed as a second argument."
   (let* ((p1 (or (pop input) (read *standard-input*)))
          (p2 (cond ((stringp p1) p1) 
                    ((listp p1) (eval p1)) ;quoted list or form
                    ((boundp p1) (eval p1))
                    (t p1))) )
     (push p2 grepl-history)
     (in-package p2)))

  ((or (equal :update grepl-item)
       (equal :up grepl-item))
   ":update or :up will update all windows"
   (opal:update-all-windows))

  ((equal :user grepl-item) ":user sets the package to user." (in-package "COMMON-LISP-USER"))
  ((eq :opal item)   ":opal will set the package to OPAL."   (in-package "OPAL"))

  ((equal :redo grepl-item) 
   ":redo will redo the last command"
   (grepl grepl-history))

  ((equal :clearx grepl-item) 
   ":clearx will clear out any X events that might be out there."
   (opal:clear-X-events))

  ((equal :load grepl-item) 
   ":load will reload a file from the grepl-loading-directory you define."
   (let ((file (cond (input (pop input))
                     (t (and (not (listen *standard-input*))
                             (format t "File to load w/o extension: "))
                        (read)))))
     (push file grepl-history)
     (load (format nil "~a~a" grepl-loading-directory file))
     (terpri)))
  ;; unmatched keywords get a petite help message
  ((keywordp grepl-item)
    ""
   (format t "Type `:?' or `:help' for the list of commands."))
  ;; this need to be last
  (t "* anything else will be evaluated."
     (format t "~s~%" (eval grepl-item)))
))


;;;
;;;	iv.	Helper functions
;;;

(defmacro grepl-test (keyword symbol)
  #-release-garnet
  "Tests for keyword and unbound symbols bound to grepl-item."
  `(or (eq grepl-item ,keyword)
       (and (symbolp grepl-item)
	    (string-equal grepl-item ',symbol)
	    (not (boundp ',symbol))))  )

(defun print-grepl-prompt (&key (stream t))
  #-release-garnet
 "Print out the garnet-prompt"
 (format stream "<~a:~a> " 
         grepl-supersystem-name
         (string-downcase (package-name *package*))))

(defun print-grepl-help-message ()
  #-release-garnet
 "Print out a help message based on the commands on *grepl-conditions*."
 (do* ((help-items (copy-list *grepl-conditions*))
       (help-item (second (pop help-items))
                  (second (pop help-items))))
     ((null help-items)
      (if help-item
         (format t "* ~a~%" help-item)))
   (if help-item
     (format t "* ~a~%" help-item))))

(defconstant *whitespace-chars* 
  '(#\Space #\Newline #\Tab #\Page #\Rubout #\Linefeed #\Return #\Backspace)
  #-release-garnet
  "A list of characters read as whitespace by the garnet-repl")

(proclaim '(inline whitespace-char-p))

(defun whitespace-char-p (achar)
  #-release-garnet
  "Returns t if achar is whitespace"
  (and (member achar *whitespace-chars* :test #'char=)
       t))


;;;
;;;	iv.	Necessary Extensions to Garnet

(defun clear-X-events (&optional awindow)
  #-release-garnet
 "Clear the event queue, really more advanced version"
 (let ((display (if awindow
                    (opal::display-info-display (g-value awindow :display-info))
	            (let ((win1 (caar (opal::get-table-contents))))
		       (if win1
			   (xlib:window-display win1)
                           opal::*default-x-display*)))))
 (and (discard-current-event display)
      (clear-X-events))))

(defun update-all-windows ()
  #-release-garnet
 "Update items on the display until done."
 (do ((display  (let ((win1 (caar (opal::get-table-contents))))
		       (if win1
			   (xlib:window-display win1)
                           *default-x-display*))))
      ((not (xlib:event-listen display)))
   (default-event-handler display :timeout 0)))



;;;
;;;	I. 	Garnet read-eval-print loop (grepl)
;;;
;;;    This is an event loop that read lisp if it appears, and runs an
;;; event handler when x events appear.  Call this at the top level for
;;; programming-fanuegen (programming pleasure).

(defun grepl ()
  #-release-garnet
 "The top level function, mostly just catches errors."
  (prog ()
    (format t "~%~a ~%" welcome-to-grepl)
    start
    (setq *quit-grepl* nil)
    (unwind-protect 
      (block nil 
	    (catch 'exit-grepl (garnet-event-and-lisp-loop)))
      (cond (*quit-grepl* nil) ;user wants out
            (t (format t "~a ~%" rewelcome-to-grepl)
               (go start))))  ;user made a boo-boo
    (format t "~a~%" goodbye-to-grepl)))


;;;
;;;	II. 	Garnet-event-and-lisp-loop
;;;
;;;    This is an event loop that read lisp if it appears, and runs an
;;; event handler when x events appear.
;;;

(defun garnet-event-and-lisp-loop (&optional awindow)
  #-release-garnet
  "A loop that reads input and calls code to handle xevents
when they happen."
  (funcall grepl-prompt-function :stream *standard-output*)
  (let ((display (if awindow
                     (display-info-display (g-value awindow :display-info))
		     (let ((win1 (caar (get-table-contents))))
		       (if win1
			   (xlib:window-display win1)
                           *default-x-display*)))))
  ;; gnu will buffer input for us, so that's cool, and event-listen
  ;; tells us when there's an x-event.  If gnu is not there, a user
  ;; is committed to typing something if he starts to.
  (prog () start
        (cond ( (listen *standard-input*) 
		(garnet-lisp-repl) )
	      ( (xlib:event-listen display)
                ;; uses keyword based event-handler sent to Garnet Fall 90
                (opal::default-event-handler display :timeout 0) )
              ( t (sleep grepl-loop-sleep-time) ))
	(go start))
   (format t "~a~%" goodbye-to-grepl)
   ;; dump the event that made you quit?
   (xlib:event-case (display :discard-p t :timeout 5) ; discard current event
     (otherwise () t))
   ;(clear-X-events)
   ))


;;;
;;;	III.	Garnet-lisp-repl
;;;
;;; when you get here you have input, but if just a whitespace, pitch it
;;; could be smarter about errors, and not getting thrown past
;;; somewhat shaky history item
;;;

(defconstant *newline-chars* 
  '(#\Newline #\Linefeed #\Return)
  #-release-garnet
  "A list of characters read as new-lines by the grepl")

(defun garnet-lisp-repl (&optional input)
  #-release-garnet
  "Lisp Read-Eval-Print-Loop (repl) for Garnet that runs once per call of grepl."
  (declare (special input)) ; necc for the evals below
  (setq grepl-history (reverse grepl-history)) 
  ;; reverse up here for safty
  (if (and (not input)
           (whitespace-char-p (peek-char nil *standard-input*)))
      ;; read some deadspace, else doit
      (if (member (read-char *standard-input*) *newline-chars* :test #'char=)
	  (funcall grepl-prompt-function :stream *standard-output*))
      (progv '(grepl-item) 
              (list (or (pop input)
                        (read *standard-input*)))
        (if (not (eq grepl-item :redo))
            (setq grepl-history (list grepl-item)))
        (do* ((tests *grepl-conditions*)
             (test (pop tests) (pop tests))  )
            ( (or (not test) (eval (car test)))
              (eval `(progn ,@(cddr test)))   ))
        (if (not (eq grepl-item :redo))
            (funcall grepl-prompt-function :stream *standard-output*)))))


 
;;;
;;;	IV.	Change to default-event-handler
;;; Gives it standard keywords.
;;; Change posted to Garnet [12March91], but not included in 1.4.

;; Don't include it if loading the Soar interface, for it has been loaded
;; previously from inter-changes

#-release-dsi (in-package "INTERACTORS")

#-release-dsi
(defun opal::default-event-handler
      (display &optional &key (discard-p t) (force-output-p t)
			      (peek-p nil) (timeout nil))
  #-release-dsi
  "Event handler for the interactor windows"
  (declare (ignore force-output-p discard-p))
  (xlib:event-case (display :discard-p t :force-output-p t
                    :force-output-p force-output-p :peek-p peek-p 
                    :timeout timeout)
    (:MAP-NOTIFY (event-window)
		 (opal::Map-Notify (debug-p :event) event-window)
		 #-cmu nil)
    (:UNMAP-NOTIFY (event-window)
		   (opal::Unmap-Notify (debug-p :event) event-window)
		   #-cmu nil)
    (:REPARENT-NOTIFY (event-window x y)
		      (opal::Reparent-Notify (debug-p :event) event-window x y)
		      #-cmu nil)
    (:CIRCULATE-NOTIFY () (opal::Circulate-Notify (debug-p :event))
			   #-cmu nil)
    (:GRAVITY-NOTIFY () (opal::Gravity-Notify (debug-p :event)) #-cmu nil)
    (:DESTROY-NOTIFY (event-window)
		     (opal::Destroy-Notify (debug-p :event) event-window)
		     #-cmu nil)
    (:CONFIGURE-NOTIFY (x y width height event-window above-sibling)
		       (opal::Configure-Notify (debug-p :event) x y
					      width height
					      event-window above-sibling)
			#-cmu nil)
    (:EXPOSURE (event-window count x y width height)
	       (opal::Exposure (debug-p :event) event-window count x y width height display)
	       #-cmu nil)
    (:KEY-PRESS (event-window x y state code time)
		(if *trans-from-file* T ; ignore events when read transcript
		    (Key-Press event-window x y state code time))
		#-cmu nil)
    (:BUTTON-PRESS (event-window x y state code event-key time)
		   (if *trans-from-file* T ; ignore events when read transcript
		       (Button-Press event-window x y
				     state code event-key time))
		   #-cmu nil)
    (:BUTTON-RELEASE (event-window x y state code event-key time)
		     (if *trans-from-file* T ; ignore events when read transcript
			 (Button-Release event-window x y
					 state code event-key time))
		     #-cmu nil)
    (:MOTION-NOTIFY (event-window x y)
		    (if *trans-from-file* T ; ignore events when read transcript
			(Motion-Notify event-window x y display))
		    #-cmu nil)
    (:NO-EXPOSURE () t #-cmu nil)
    (OTHERWISE () (format t "illegal event") t #-cmu nil)))
