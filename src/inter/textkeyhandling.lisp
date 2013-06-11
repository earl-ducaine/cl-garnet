;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This file contains the commands to allow the text interactor's keyboard
;;; bindings to be changed.
;;; This should be loaded before Textinter.
;;;
;;; Designed and implemented by Brad A. Myers

#|
============================================================
Change log:
         6/15/93 Brad Myers - safe-functionp
         5/26/93 Mickish/Goldberg - Fixed get-or-make-local-key-table to copy
                   both standard and lisp translation tables
         3/19/93 Brad Myers - added ^k and ^o
        12/24/92 Brad Myers - allow binding of mouse events
	 3/20/92 Ed Pervin - Changed all control characters to keywords,
				got rid of those :num-pad keywords.
         1/21/92 Ed Pervin - adjusted for CMUCL on Sparc
	 3/27/91 Greg Sylvain - adjusted for kcl
	 8/22/90 Brad Myers - remove #+cmu #\leftarrow kinds of things
	 4/9/90 Pervin/Cook - changed eq to eql
         4/9/90 Brad Myers - New functions: Unbind-All-Keys, Insert-Text-Into-String
         3/14/90 Brad Myers - Created.
============================================================
|#

(in-package "INTERACTORS")

;;;============================================================
;;; Helper functions
;;;============================================================

(defun bind-key-internal (key function-operation-or-char hash-table)
  (setf (gethash key hash-table) function-operation-or-char))

(defun copy-hash-table (old-table) ; why isn't this built-in?
  (unless old-table
    (error "Key translation table missing"))
  (if-debug :event
      (format t "** Making a new Key-translation-table~%"))
  (let ((new (make-hash-table)))
    ;; for each item in the old table, put it into the new table
    (maphash #'(lambda (key value)
		 (setf (gethash key new) value))
	     old-table)
    new))

;;If the hash table is not local to the interactor, copy its prototype's
;; table.  This is done because next the local one will be edited.
(defun get-or-make-local-key-table (an-interactor)
  (let ((ht (get-local-value an-interactor :key-translation-table)))
    (cond
      ((hash-table-p ht) ht)
      ;; if it is a formula, return eval of formula
      (ht (g-value an-interactor :key-translation-table))
      ;; if it is NIL, see what the prototype holds
      (t (setf ht (get-value an-interactor :key-translation-table))
	 (cond
	   ;; prototype had a hash table, so copy it down
	   ((hash-table-p ht)
	    (s-value an-interactor :key-translation-table
		     (copy-hash-table ht)))
	   ;; prototype had a formula, so copy down both tables
	   (ht 
	    (s-value an-interactor :standard-translation-table
		     (copy-hash-table
		      (g-value an-interactor :standard-translation-table)))
	    (s-value an-interactor :lisp-translation-table
		     (copy-hash-table
		      (g-value an-interactor :lisp-translation-table)))
	    ;; Make value local, so that tables aren't copied down next time
	    (s-value an-interactor :key-translation-table (formula ht))
	    (g-value an-interactor :key-translation-table))
	   (t (error "Could not copy key translation table from prototype of ~S"
		     an-interactor)))))))



;;;============================================================
;;; Exported functions
;;;============================================================

(defun Insert-Text-Into-String (string-obj new-text
					   &optional (move-back-cursor 0))
"Inserts the new text into the cursor-text (or multi-cursor-text) string object
at the current cursor index.  Cursor is moved to the end of the new text.  
If move-back-cursor is supplied, it should be an integer and the cursor is
moved back over that number of characters"
  (let* ((old-string (g-value string-obj :string))
	 (index (or (g-value string-obj :cursor-index)
		   (length old-string))))
    (s-value string-obj :string 
	     (concatenate 'string (subseq old-string 0 index)
			  new-text (subseq old-string index)))
    (when (g-value string-obj :cursor-index) ; when it used to be on, update it
      (s-value string-obj :cursor-index (+ index (length new-text)
					   (- move-back-cursor))))))

(defun Bind-Key (key val an-interactor)
"   Binds a key in the key translation table of the interactor.  If the key
translation table used to be inherited, it is copied to the local
interactor and then modified.  
    The key can either be a Lisp character or a special keyword
used in a define-keysym call (see the file inter/define-keys.lisp) to a
particular editing operation.  The second parameter to bind-key can either be a
character to map into (e.g: #\super-4 maps to #\4), one of the built-in
editing operations which are keywords (see list in the manual), a string
(so that the key acts like a macro and expands into a string), or a
function that performs an edit.  The function should take three parameters:
the interactor, the cursor-text object and the inter:event."
  (if-debug :event
      (format t "Setting the binding of ~s to ~s for ~s~%"
	      key val an-interactor))
  (let ((ht (get-or-make-local-key-table an-interactor)))
    (bind-key-internal key val ht)))

(defun Unbind-Key (key an-interactor)
"Remove the translation for the key from the keytranslation table of the
interactor.  If the key translation table used to be inherited, it is
copied to the local interactor and then modified."
  (if-debug :event
      (format t "Un-binding ~s for ~s~%" key an-interactor))
  (let ((ht (get-or-make-local-key-table an-interactor)))
    (remhash key ht)))

(defun Unbind-All-Keys (an-interactor)
"Removes the translations for all keys from the keytranslation table of the
interactor."
  (if-debug :event
      (format t "Un-binding ALL KEYS for ~s~%" an-interactor))
  (let ((ht (get-local-value an-interactor :key-translation-table)))
    (if (not (hash-table-p ht))
      (s-value an-interactor :key-translation-table (setq ht (make-hash-table)))
      ; else re-initialize ht
      (clrhash ht))))

(defun Set-Default-Key-Translations (an-interactor)
"Initializes the hash table of an-interactor with the standard
translations.  If there is no table in an-interactor, creates one.
Otherwise, removes any translations that are there before adding the new ones."
  (let ((ht (get-local-value an-interactor :key-translation-table)))
    (if (not (hash-table-p ht))
      (s-value an-interactor :key-translation-table (setq ht (make-hash-table)))
      ; else re-initialize ht
      (clrhash ht))
      (bind-key-internal :leftarrow   :prev-char ht)
      (bind-key-internal :control-b   :prev-char ht)
      (bind-key-internal :control-\b  :prev-char ht)
    
      (bind-key-internal :rightarrow  :next-char ht)
      (bind-key-internal :control-f   :next-char ht)
      (bind-key-internal :control-\f  :next-char ht)
    
      (bind-key-internal :uparrow     :up-line ht)
      (bind-key-internal :control-p   :up-line ht)
      (bind-key-internal :control-\p  :up-line ht)
    
      (bind-key-internal :downarrow   :down-line ht)
      (bind-key-internal :control-n   :down-line ht)
      (bind-key-internal :control-\n  :down-line ht)
    
#+kcl (bind-key-internal #\rubout     :delete-prev-char ht)
#-kcl (bind-key-internal #\delete     :delete-prev-char ht)
      (bind-key-internal #\backspace  :delete-prev-char ht)
      (bind-key-internal :control-h   :delete-prev-char ht)
      (bind-key-internal :control-\h  :delete-prev-char ht)
    
#+kcl (bind-key-internal #\\377       :delete-prev-word ht)
      (bind-key-internal :control-backspace :delete-prev-word ht)
      (bind-key-internal :control-delete    :delete-prev-word ht)
      (bind-key-internal :control-w   :delete-prev-word ht)
      (bind-key-internal :control-\w  :delete-prev-word ht)
    
      (bind-key-internal :control-d   :delete-next-char ht)
      (bind-key-internal :control-\d  :delete-next-char ht)
    
      (bind-key-internal :control-u   :delete-string ht)
      (bind-key-internal :control-\u  :delete-string ht)

      (bind-key-internal :control-o   :insert-lf-after ht)
      (bind-key-internal :control-\o  :insert-lf-after ht)

      (bind-key-internal :control-k   :kill-line ht)
      (bind-key-internal :control-\k  :kill-line ht)

      (bind-key-internal :home        :beginning-of-string ht)
      (bind-key-internal :control-\,  :beginning-of-string ht)
      (bind-key-internal :control-<   :beginning-of-string ht)
    
      (bind-key-internal :control-a   :beginning-of-line ht)
      (bind-key-internal :control-\a  :beginning-of-line ht)
    
      (bind-key-internal :end         :end-of-string ht) 
      (bind-key-internal :control-.   :end-of-string ht)
      (bind-key-internal :control->   :end-of-string ht)
    
      (bind-key-internal :control-e   :end-of-line ht)
      (bind-key-internal :control-\e  :end-of-line ht)
    
      (bind-key-internal :control-c   :copy-to-X-cut-buffer ht)
      (bind-key-internal :control-\c  :copy-to-X-cut-buffer ht)
    
      (bind-key-internal :insert      :copy-from-X-cut-buffer ht)
      (bind-key-internal :insert-line :copy-from-X-cut-buffer ht)
#+(or vax dec3100 dec5000)
      (bind-key-internal :insert-here :copy-from-X-cut-buffer ht)
      (bind-key-internal :control-y   :copy-from-X-cut-buffer ht)
      (bind-key-internal :control-\y  :copy-from-X-cut-buffer ht)
    
      (bind-key-internal #\return     #\Newline ht)
      (bind-key-internal :control-j   #\Newline ht)
      (bind-key-internal :control-\j  #\Newline ht)
    
    ))

;;Look up in translation table and either return value there or the
;; original key
(defun Translate-key (key an-interactor)
  (let ((val (gethash key (g-value an-interactor :key-translation-table) key)))
    (if-debug :event
      (format t " key ~s translated to ~s for ~s ~%" key val an-interactor))
    val))

;;;---------------------------------------------------------------------------
;;; Internal functions used for editing
;;;---------------------------------------------------------------------------

;; adds char as the new index char
(defun add-char (char str index)
  (let ((s (concatenate 'string (subseq str 0 index)
			" " (subseq str index))))
    (fill s char :start index :end (1+ index))
    s))

;; removes char BEFORE index
(defun remove-char (str index)
  (concatenate 'string (subseq str 0 (1- index)) (subseq str index)))

;; check if char is whitespace
(defun white-space-p (char)
  (member char '(#\space #\tab #\newline)))

;; returns both the new string and the new index, removes the previous word
(defun remove-word (str index)
  (if (> (length str) 0)
    (let* ((start-search (1+ (or (position-if-not #'white-space-p
						  str :from-end T :end index)
				 0)))
	   (prev-space (1+ (or (position-if #'white-space-p
					    str :from-end T :end start-search)
			       -1)))) ; use beginning of string if no space
      (values (concatenate 'string (subseq str 0 prev-space) (subseq str index))
	      prev-space))
    (values str 0)))

(defparameter newlinestr "
")

;;; Does kill line from string and adds string to X cutbuffer.
;;; Returns the new string.  Index always stays the same.
(defun DoKillLine (str index alreadycutting win)
  (let ((crpos (position #\newline str :start index))
	new-str cut-str)
    (cond ((null crpos) ;; no newline
	   (setq new-str (subseq str 0 index))
	   (setq cut-str (subseq str index)))
	  ((= crpos index) ;; at the newline, delete it
	   (setq new-str (remove-char str (1+ index)))
	   (setq cut-str newlinestr))
	  (T ; otherwise, remove from index to crpos
	   (setq new-str (concatenate 'string (subseq str 0 index)
				      (subseq str crpos)))
	   (setq cut-str (subseq str index crpos))))
    (when alreadycutting
      (setq cut-str (concatenate 'string alreadycutting cut-str)))
    (unless (zerop (length cut-str))
      (opal:Set-X-Cut-Buffer win cut-str))
    (values new-str cut-str)))

(defun Add-X-Cut-Buffer (str index window)
  (let ((xstring (Opal:Get-X-Cut-Buffer window)))
    (values 
     (concatenate 'string (subseq str 0 index) xstring
		  (subseq str index))
     (+ index (length xstring)))))

;; String-object is modified based on char entered, based on the current
;; values in the :key-translation-table of the interactor.
(defun Edit-String (an-interactor string-object event)
  (if (or (null event) (not (schema-p string-object)))
      NIL ; ignore this event and keep editing
      ; else
      (let ((index (g-value string-object :cursor-index))
	    (str (g-value string-object :string))
	    (new-trans-char (Translate-key (event-char event) an-interactor))
	    (alreadycutting NIL)
	    pos)
	(if (eq new-trans-char :kill-line)
	    (setq alreadycutting (g-value string-object :in-kill-mode))
	    (s-value string-object :in-kill-mode NIL))
	(when new-trans-char
	  (case new-trans-char
	    (:prev-char (s-value string-object :cursor-index
				 (max 0 (1- index))))
	    (:next-char (s-value string-object :cursor-index
				 (min (length str) (1+ index))))
	    (:up-line (opal:move-cursor-up-one-line string-object))
	    (:down-line (opal:move-cursor-down-one-line string-object))
	    (:delete-prev-char
	     (when (> index 0)
	       (s-value string-object :string (remove-char str index))
	       (s-value string-object :cursor-index (1- index))))
	    (:delete-prev-word
	     (multiple-value-setq (str index)(remove-word str index))
	     (s-value string-object :cursor-index index)
	     (s-value string-object :string str))
	    (:delete-next-char
	     (when (< index (length str))
	       (s-value string-object :string (remove-char str (1+ index)))))
	    (:kill-line
	     (multiple-value-setq (str alreadycutting)
	       (DoKillLine str index alreadycutting (event-window event)))
	     (s-value string-object :string str)
	     (s-value string-object :in-kill-mode alreadycutting))
	    (:delete-string 
	     (s-value string-object :cursor-index 0)
	     (s-value string-object :string ""))
	    (:beginning-of-string (s-value string-object :cursor-index 0))
	    (:Insert-LF-after
	     (s-value string-object :string (add-char #\newline str index)))
	    (:beginning-of-line
	     (opal:move-cursor-to-beginning-of-line string-object))
	    (:end-of-string
	     (s-value string-object :cursor-index (length str)))
	    (:end-of-line (opal:move-cursor-to-end-of-line string-object))
	    (:copy-to-X-cut-buffer	; don't modify string, but copy it to
					; the X cut buffer
	     (opal:Set-X-Cut-Buffer (event-window event) str))
	    (:copy-from-X-cut-buffer
	     (multiple-value-setq (str index)
	       (Add-X-Cut-Buffer str index (event-window event)))
	     (s-value string-object :cursor-index index)
	     (s-value string-object :string str))
	    (T;; here might be a keyword, character, string, or function
	     (cond 
	       ((and (characterp new-trans-char)
		     (or (graphic-char-p new-trans-char)
			 (eql new-trans-char #\NewLine)))
					; then is a regular character, so add to str
		(s-value string-object :string
			 (add-char new-trans-char str index))
		(s-value string-object :cursor-index (1+ index)))
	       ;; check if a string
	       ((stringp new-trans-char) ; then insert into string
		(Insert-Text-Into-String string-object
					 new-trans-char))
					; now check for functions
	       ((garnet-utils:safe-functionp new-trans-char)
					; then call the function
		(funcall new-trans-char an-interactor
			 string-object event))
	       ((event-mousep event)	; see if want to move cursor
		(when (and (event-downp event)
			   (g-value an-interactor
				    :cursor-where-press)
			   (setq pos (opal:get-cursor-index string-object
							    (event-x event)
							    (event-y event))))
		  ;; then change the cursor position
		  (s-value string-object :cursor-index pos)))
	       (T   ;; otherwise, must be a bad character
		(Beep)))))))))
