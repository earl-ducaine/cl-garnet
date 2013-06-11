;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Pull Down Menu Function
;;;
;;;
;;;  This file contains the functions that are applicable to BOTH the
;;;  motif-menubar and the GARNET menubar.  It is loaded in the loader
;;;  files for both the menubars.  What used to be functions for the
;;;  menubars are now METHODS.  The macros in this file just call the
;;;  appropriate method.
;;;
;;;  CHANGE LOG:
;;;
;;;  06/12/93 Andrew Mickish - Added Check-Bar-Item
;;;  06/10/93 Andrew Mickish - opal::safe-functionp ---> gu:safe-functionp
;;;  03/17/93 Brad Myers - add special-popdown functions for gilt, etc.
;;;  03/12/93 Andrew Mickish - Now Check-Menubar-Items allows empty lists of
;;;             submenu items
;;;  03/08/93 Andrew Mickish - Added error-checking: Check-Menubar-Items
;;;  03/03/93 Andrew Mickish - Added Menubar-Change-Item
;;;  01/07/93 Rajan Parthasarathy - Created
;;;

(in-package "GARNET-GADGETS")

(eval-when (eval load compile)
  (export '(Add-Submenu-Item Remove-Submenu-Item
	    Menubar-Components Submenu-Components Get-Submenu-Component
	    Find-Submenu-Component Get-Bar-Component Set-Menubar Set-Submenu
	    Menubar-Enable-Component Menubar-Disable-Component
	    Menubar-Enabled-P Menubar-Installed-P
	    Menubar-Get-Title Menubar-Set-Title)))
	  
;;; This is the DESTROYER (*gasp*) of the menubars

(defun MENUBAR-DESTROY (a-menubar &optional erase)
  ; first, make sure no submenus are popped up
  (kr-send a-menubar :special-popdown-func a-menubar)
  ; Destroy each window that is associated with this menubar
  (dolist (win (g-value a-menubar :submenu-window-list))
    (opal:destroy win))
  ; Destroy the menubar itself
  (call-prototype-method a-menubar erase))

;;; This is the macro that calls add-submenu-item

(defmacro Add-Submenu-Item (a-menubar b-item s-item &rest args)
  `(let ((my-menubar ,a-menubar))
    (kr-send my-menubar :add-submenu-item my-menubar ,b-item ,s-item ,@args)))

;;; And this, of course, is the macro that calls remove-submenu-item

(defmacro Remove-Submenu-Item (a-menubar b-item s-item)
  `(let ((my-menubar ,a-menubar))  
    (kr-send my-menubar :remove-submenu-item my-menubar ,b-item ,s-item)))

;;; This one.. for menubar-components.  The next few are self-explanatory.

(defmacro Menubar-Components (a-menubar)
  `(let ((my-menubar ,a-menubar))  
    (kr-send my-menubar :menubar-components my-menubar)))

(defmacro Submenu-Components (a-bar-item)
  `(let ((my-bar-item ,a-bar-item))  
    (kr-send my-bar-item :submenu-components my-bar-item)))

(defmacro Get-Submenu-Component (a-bar-item item)
  `(let ((my-bar-item ,a-bar-item))    
    (kr-send my-bar-item :get-submenu-component my-bar-item ,item)))

(defmacro Find-Submenu-Component (a-menubar submenu-title submenu-item)
  `(let ((my-menubar ,a-menubar))  
    (kr-send my-menubar :find-submenu-component my-menubar ,submenu-title ,submenu-item)))

(defmacro Set-Menubar (a-menubar new-menus)
  `(let ((my-menubar ,a-menubar))  
    (kr-send my-menubar :set-menubar my-menubar ,new-menus)))

;;; This function is IDENTICAL for both the motif and regular menubars.
;;; So we just define it here and s-value it into the :set-submenu-fn
;;; slot of both menubars.  The macro will then call the correct function.

(defun Set-Submenu-Fn (a-bar-item new-desc)
  (let ((components (copy-list (Submenu-Components a-bar-item))))
    (dolist (old-comp components)
      (opal:remove-item a-bar-item old-comp)))
  (dolist (comp new-desc)
    (opal:add-item a-bar-item comp)))

(defmacro Set-Submenu (a-bar-item new-desc)
  `(let ((my-bar-item ,a-bar-item))
    (kr-send my-bar-item :set-submenu-fn my-bar-item ,new-desc)))

;;; Same as above for Get-Bar-Component

(defun Get-Bar-Component-Fn (a-menubar item)
  (find-if #'(lambda (a-bar-item)
	       (if (listp item)
		   (equal item (g-value a-bar-item :desc))
		   (equal item (g-value a-bar-item :menu-obj))))
	   (Menubar-Components a-menubar)))

(defmacro Get-Bar-Component (a-menubar item)
  `(let ((my-menubar ,a-menubar))  
    (kr-send my-menubar :get-bar-component-fn my-menubar ,item)))

;;; The following four functions are identical to both the menubars.
;;; Not only that, they are extremely primitive.  And since they
;;; work for BOTH menubars, we don't need to have a macro or a
;;; define method.  We can just use them as they are!

(defun Menubar-Disable-Component (menubar-component)
  (s-value menubar-component :enabled NIL))

(defun Menubar-Enable-Component (menubar-component)
  (s-value menubar-component :enabled T))
  
(defun Menubar-Enabled-P (menubar-component)
  (g-value menubar-component :enabled))
  
(defun Menubar-Installed-P (menubar-component)
  (g-value menubar-component :parent))

;;; This macro is to call the get-title function.  Now, menubar-component
;;; can be a BAR-ITEM, SUBMENU-ITEM, MOTIF-BAR-ITEM, or MOTIF-MENU-ITEM.
;;; BAR-ITEM and SUBMENU-ITEM have the same function in their :get-title-fn,
;;; as do MOTIF-BAR-ITEM and MOTIF-MENU-ITEM.  

(defmacro Menubar-Get-Title (menubar-component)
  `(let ((menubar-comp ,menubar-component))  
    (kr-send menubar-comp :get-title-fn menubar-comp)))

;;; This macro is for the set-title.  Works on the same principles as the previous
;;; macro.

(defmacro Menubar-Set-Title (menubar-component string)
  `(let ((menubar-comp ,menubar-component))    
    (kr-send menubar-comp :set-title-fn menubar-comp ,string)))


;; The :change-item method for both menubars
;;
(defun Menubar-Change-Item (a-bar-item new-item n)
  ;; Never use the aggrelist trick of just setting the :items list, even
  ;; though the two items are of the same type.  We have to do elaborate
  ;; bookkeeping in the higher-level menubar :items list with the
  ;; add/remove-item methods.
  (let ((items (g-value a-bar-item :items)))
    (if (or (>= n (length items))
	    (< n 0))
	(warn "Bad index in change-item: ~A" n)
	(progn
	  (opal:remove-item a-bar-item (nth n items))
	  (opal:add-item a-bar-item new-item :at n)))))


(defun Check-Bar-Item (top-level-item)
  (let ((bar-item-title (first top-level-item))
	(bar-item-action (second top-level-item))
	(submenu-items (third top-level-item)))
    (unless (or (stringp bar-item-title) (keywordp bar-item-title)
		(schema-p bar-item-title))
      (error "Got ~S for the title of a bar-item, but it should be a string, keyword, or schema." bar-item-title))
    (unless (or (null bar-item-action)
		(and (symbolp bar-item-action)
		     (not (keywordp bar-item-action)))
		(gu:safe-functionp bar-item-action))
      (error "Got ~S for the action of a bar-item, but it should be a function, symbol, or NIL." bar-item-action))
    
    (unless (listp submenu-items)
      (error "Got ~S for a list of submenu items." submenu-items))
    (dolist (submenu-item submenu-items)
      (unless (consp submenu-item)
	(error "Got ~S for a submenu-item description, but it should be a list." submenu-item))
      (let ((submenu-item-title (first submenu-item))
	    (submenu-item-action (second submenu-item)))
	(unless (or (stringp submenu-item-title)
		    (keywordp submenu-item-title)
		    (schema-p submenu-item-title))
	  (error "Got ~S for a submenu-item, but it should be a string, keyword, or schema." submenu-item-title))
	(unless (or (null submenu-item-action)
		    (and (symbolp submenu-item-action)
			 (not (keywordp submenu-item-action)))
		    (gu:safe-functionp submenu-item-action))
	  (error "Got ~S for the action of a submenu-item, but it should be a function, symbol, or NIL." submenu-item-action)))))
  )
  
(defun Check-Menubar-Items (items)
  (or (null items)
      (dolist (top-level-item items)
	(Check-Bar-Item top-level-item)))
  T) ;; Would crash inside Check-Bar-Item if there was a problem.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Put down the display of the submenu, if popped up from gilt, etc.
;;; Works for both motif and regular menubars
      
(defun Put-Down-MenuBar-Popups (gadget)
  (let (obj)
    (when (schema-p gadget)
    (s-value gadget :*bar-item-popped-up NIL)
    (when (and (setq obj (g-value gadget :menubar-select))
	       (schema-p obj))
      (s-value obj :prev-baritem NIL))
      (dolist (bar-item (Menubar-Components gadget))
	(when (and (schema-p bar-item)
		   (setq obj (g-value bar-item :submenu-window))
		   (schema-p obj))
	  (s-value obj :visible NIL))))))

(defun DoSpecialPopUpMenubar (gadget bar-item)
  (unless (eq bar-item (g-value gadget :*bar-item-popped-up))
    (Put-Down-MenuBar-Popups gadget)	; put any other sub-menus down
    (s-value gadget :*bar-item-popped-up bar-item)
    (let ((win (g-value bar-item :submenu-window)))
      (if win
	  (let* ((inter (g-value gadget :menubar-select))
		 (prev (g-value inter :prev-baritem)))
	    (when prev
	      (s-value prev :interim-selected NIL))
	    (s-value inter :prev-baritem bar-item)
	    (s-value (g-value bar-item :submenu) :value-obj NIL)
	    (s-value win :visible T)
	    (opal:raise-window win)
	    (opal:update-all)
	    win)			; return window if successful
	  ;; else error
	  (progn
	    (format T "**Menubar ~s item ~s window=NIL; not initialized?~%"
		    gadget bar-item)
	    NIL)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

