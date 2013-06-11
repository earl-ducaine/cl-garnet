;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;


;;; This file contains the mouse and keyboard ACCELERATORS code
;; 
;; Designed and implemented by David S. Kosbie
;; 
;; The punchline for those who hate to read is that if you type the
;; following keys into a Garnet window, you get the associated action:
;; 
;;     :SHIFT-F1 -  raise window
;;     :SHIFT-F2 -  lower window
;;     :SHIFT-F3 -  iconify window
;;     :SHIFT-F4 -  zoom window
;;     :SHIFT-F5 -  fullzoom window
;;     :SHIFT-F6 -  refresh window
;;     :SHIFT-F7 -  destroy window
;; 
;;     :HELP - INSPECT object
;;     :SHIFT-HELP - print out object under the mouse (also in inspector.lisp)
;; 
;; In interactors.lisp, the function "process-event" first checks the "first"
;; accelerators, as follows
;; 	1) Try to match event (using assoc) against one in window's
;; 		:first-Accelerators slot, which should contain an alist
;;                 of the form  ( (char1 . fun1) (char2 . fun2) ... )
;; 		[Note: these functions take 1 arg, the low-level event struct]
;; 	2) If that succeeds, invoke the found function, else repeat the
;; 	   same process using the global variable *global-first-accelerators*
;; Then each low-level event to see if any interactors (or priority-levels 
;; with :stop-when of :always) claim the event.  If not, then it does the following:
;; 	1) Try to match event (using assoc) against one in window's
;; 		:Accelerators slot, which should contain an alist of the form
;; 		( (char1 . fun1) (char2 . fun2) ... )
;; 		[Note: these functions take 1 arg, the low-level event struct]
;; 
;; 	2) If that succeeds, invoke the found function, else repeat the
;; 		same process using the global variable *global-accelerators*
;; 
;; The variables *global-accelerators* and *global-first-accelerators* are defvar'd
;; in interactors.lisp, so they can be properly referenced.
;; 
;; This file first defines the *default-global-accelerators* and all the
;; functions it references.  At the end, it invokes this function.
;; 
;; This file also defines a programmatic interface to accelerators
;; 
;; 


;;; Change log:
;;  1/18/93 Brad Myers - supply accelerators that go FIRST before the inters
;;                     - removed ident as accelerator, since garnet-debug might
;;                       not be loaded
;;  10/22/92 Dave Kosbie - created


(in-package "INTERACTORS")

;;; the exported functions and variables
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(
	    *global-accelerators*	; defined in interactors.lisp
	    *global-first-accelerators*	; defined in interactors.lisp
	    *default-global-accelerators*

	    add-global-accelerator      add-window-accelerator
	    remove-global-accelerator   remove-window-accelerator
	    clear-global-accelerators   clear-window-accelerators
	    default-global-accelerators

	    )))


;;; Default Support Fns
;;

;; args should be a list:  (variable event)
(defmacro with-event-win (args &rest body)
  `(let ((,(first args) (event-window ,(second args))))
    (when ,(first args) ,@body)))

(defun raise-acc (event)  (with-event-win (win event) (opal:raise-window win)))
(defun lower-acc (event)  (with-event-win (win event) (opal:lower-window win)))
(defun iconify-acc (event)(with-event-win (win event)
				(opal:iconify-window win)))
(defun zoom-acc (event)   (with-event-win (win event) (opal:zoom-window win)))
(defun fullzoom-acc (event)   (with-event-win (win event)
				(opal:fullzoom-window win)))
(defun refresh-acc (event)(with-event-win (win event) (opal:update win T)))
(defun destroy-acc (event)(with-event-win (win event) (opal:destroy win)))

(defvar *default-global-accelerators* '(
    (:SHIFT-F1 .  raise-acc)
    (:SHIFT-F2 .  lower-acc)
    (:SHIFT-F3 .  iconify-acc)
    (:SHIFT-F4 .  zoom-acc)
    (:SHIFT-F5 .  fullzoom-acc)
    (:SHIFT-F6 .  refresh-acc)
    (:SHIFT-F7 .  destroy-acc)))

                         ;;;;;;;;;;;;;;;;;;;;;;;;
                         ;;;  Basic Interface ;;;
                         ;;;;;;;;;;;;;;;;;;;;;;;;


;;; internal

(defun add-accelerator (alist key fn replace-existing?)
  (let ((alist-entry (assoc key alist)))
    (if (and replace-existing? alist-entry)
        (setf (cdr alist-entry) fn)
        (push (cons key fn) alist))
    alist))

(defun remove-accelerator (alist key remove-all?)
  (delete key alist :count (if remove-all? NIL 1) :key #'car))


;;; exported

(defun add-global-accelerator (key fn &key replace-existing? first?)
  (if first?
      (setq *global-first-accelerators*
	    (add-accelerator *global-first-accelerators*
			     key fn replace-existing?))
      (setq *global-accelerators*
	    (add-accelerator *global-accelerators*
			     key fn replace-existing?))))

(defun add-window-accelerator (win key fn &key replace-existing? first?)
  (if first?
      (s-value win :first-accelerators
	       (add-accelerator (g-value win :first-accelerators)
				key fn replace-existing?))
      (s-value win :accelerators
	       (add-accelerator (g-value win :accelerators)
				key fn replace-existing?))))

(defun remove-global-accelerator (key &key remove-all? first?)
  (if first?
      (setq *global-first-accelerators*
	    (remove-accelerator *global-first-accelerators* key remove-all?))
      (setq *global-accelerators*
	    (remove-accelerator *global-accelerators* key remove-all?))))

(defun remove-window-accelerator (win key &key remove-all? first?)
  (if first?
      (s-value win :first-accelerators
	       (remove-accelerator (g-value win :first-accelerators)
				   key remove-all?))
      (s-value win :accelerators
	       (remove-accelerator (g-value win :accelerators)
				   key remove-all?))))

(defun clear-global-accelerators ()
  (setf *global-accelerators* NIL)
  (setf *global-first-accelerators* NIL))

(defun clear-window-accelerators (win)
  (s-value win :accelerators NIL)
  (s-value win :first-accelerators NIL))

(defun default-global-accelerators ()
  (setf *global-accelerators* (copy-tree *default-global-accelerators*))
  (setf *global-first-accelerators* NIL))

;; Finally, invoke function to actually set the accelerators
(default-global-accelerators)
