;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;         The Garnet User Interface Development Environment.        ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;

;;; $Id$
;;


;;  ^f ^b ^d ^h = forward, backwards, delete forwards, delete backwards char
;;  left-arrow, right-arrow =  backwards, forwards
;;  meta-f, meta-b, meta-d, meta-h  = same but by words
;;  ^p = previous line, ^n = next line
;;  uparrow, downarrow = previous line, next line
;;  ^, or HOME = beginning of document
;;  ^. or END = end of document
;;  ^a = beginning of line
;;  ^e = end of line
;;
;;  ^k = kill line, ^u = delete entire string, ^w, CUT = delete selection
;;  META-w, COPY = copy selection to cut buffer
;;  ^c = copy entire string to X cut buffer
;;  ^y, PASTE = yank kill buffer or X cut buffer
;;  ^Y, ^PASTE = yank X buffer
;;  meta-y, meta-PASTE = yank kill buffer
;;
;; The following ones extend the selection while moving
;;   ^leftarrow, ^rightarrow = prev, next char selecting
;;   meta-leftarrow, meta-rightarrow = prev, next word selecting
;;   ^uparrow, ^downarrow = up-line, down-line selecting
;;   ^HOME ^END = beginning, end of string selecting
;;   ^* = select all
;;
;;  CONTROL-META is Lisp stuff:
;;     ^-META-b, ^-META-leftarrow = prev lisp expression
;;     ^-META-f, ^-META-rightarrow =  next lisp expression
;;     ^-META-h, ^-META-backspace, ^-META-delete = delete prev s-expr
;;     ^-META-d  = delete next s-expr
;;
;;  ^-shift- = font stuff:
;;     ^-shift-B = toggle bold
;;     ^-shift-I = toggle italic
;;     ^-shift-F = fixed font (courier)
;;     ^-shift-T = times font (serif)
;;     ^-shift-H = helvetica font (sans-serif)
;;     ^-shift-< = smaller font
;;     ^-shift-> = bigger font
;;     ^1 ^2 ^3 ^4  = small, medium, large, and very-large fonts


 (in-package "INTERACTORS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(MULTIFONT-TEXT-INTERACTOR))
  (proclaim '(special MULTIFONT-TEXT-INTERACTOR)))


;;; Initializes the hash table of an-interactor with the standard
;;  translations.  If there is no table in an-interactor, creates one.
;;  Otherwise, removes any translations that are there before adding
;;  the new ones.
(defun Set-MultiFont-Default-Key-Translations (an-interactor)
  (let ((ht (get-local-value an-interactor :standard-translation-table)))
    (if (not (hash-table-p ht))
	(s-value an-interactor :standard-translation-table
		 (setq ht (make-hash-table)))
	;; else re-initialize ht
	(clrhash ht))

    ;; backwards
    (bind-key-internal :LEFTARROW   :prev-char ht)
    (bind-key-internal :control-\b  :prev-char ht)

    (bind-key-internal :meta-\b     :prev-word ht)
    (bind-key-internal :meta-B      :prev-word ht)

    (bind-key-internal :control-meta-\b         :lisp-prev-expr ht)
    (bind-key-internal :control-meta-B          :lisp-prev-expr ht)
    (bind-key-internal :control-meta-LEFTARROW  :lisp-prev-expr ht)
    
    (bind-key-internal :control-LEFTARROW   :prev-char-select ht)
    (bind-key-internal :meta-LEFTARROW      :prev-word-select ht)

    ;; forwards

    (bind-key-internal :RIGHTARROW  :next-char ht)
    (bind-key-internal :control-\f  :next-char ht)
    
    (bind-key-internal :meta-F      :next-word ht)
    (bind-key-internal :meta-\f     :next-word ht)

    (bind-key-internal :control-meta-\f :lisp-next-expr ht)
    (bind-key-internal :control-meta-F  :lisp-next-expr ht)
    (bind-key-internal :control-meta-RIGHTARROW  :lisp-next-expr ht)

    (bind-key-internal :control-RIGHTARROW   :next-char-select ht)
    (bind-key-internal :meta-RIGHTARROW      :next-word-select ht)
    
    ;; PREVIOUS LINE
    
    (bind-key-internal :UPARROW     :up-line ht)
    (bind-key-internal :control-P   :up-line ht)
    (bind-key-internal :control-\p  :up-line ht)

    (bind-key-internal :control-UPARROW   :up-line-select ht)
    
    ;; NEXT LINE

    (bind-key-internal :DOWNARROW   :down-line ht)
    (bind-key-internal :control-N   :down-line ht)
    (bind-key-internal :control-\n  :down-line ht)

    (bind-key-internal :control-DOWNARROW   :down-line-select ht)
    
    ;; BEGINNING/END  LINE/STRING

    (bind-key-internal :control-\a  :beginning-of-line ht)
    
    (bind-key-internal :control-\e  :end-of-line ht)

    (bind-key-internal :HOME      :beginning-of-string ht)
    (bind-key-internal :R7        :beginning-of-string ht) ; HOME key on Sun
    (bind-key-internal :control-\,  :beginning-of-string ht)

    (bind-key-internal :control-HOME :beginning-of-string-select ht)
    (bind-key-internal :control-R7   :beginning-of-string-select ht) ;HOME key
    
    (bind-key-internal :END         :end-of-string ht) 
    (bind-key-internal :R13         :end-of-string ht) ; END key on Sun
    (bind-key-internal :control-\.  :end-of-string ht)

    (bind-key-internal :control-END :end-of-string-select ht)
    (bind-key-internal :control-R13 :end-of-string-select ht) ;END key
    
    (bind-key-internal :control-\*  :select-all ht)
    
    ;; delete previous

    (bind-key-internal #\delete     :delete-prev-char ht)
    (bind-key-internal #\backspace        :delete-prev-char ht)
    (bind-key-internal :control-\h        :delete-prev-char ht)
    
    (bind-key-internal :meta-H         :delete-prev-word ht)
    (bind-key-internal :meta-\h        :delete-prev-word ht)
    (bind-key-internal :meta-BACKSPACE :delete-prev-word ht)
    (bind-key-internal :meta-DELETE    :delete-prev-word ht)
    (bind-key-internal :meta-RUBOUT    :delete-prev-word ht)

    (bind-key-internal :control-meta-\h         :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-H          :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-BACKSPACE  :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-DELETE     :lisp-delete-prev-expr ht)
    (bind-key-internal :control-meta-RUBOUT     :lisp-delete-prev-expr ht)

    ;; delete next

    (bind-key-internal :control-\d  :delete-next-char ht)

    (bind-key-internal :meta-D      :delete-next-word ht)
    (bind-key-internal :meta-\d     :delete-next-word ht)

    (bind-key-internal :control-meta-\d    :lisp-delete-next-expr ht)
    (bind-key-internal :control-meta-D     :lisp-delete-next-expr ht)

    ;; other deletes and copies

    (bind-key-internal :control-\u  :delete-string ht)
    
    (bind-key-internal :control-\k  :kill-line ht)

    (bind-key-internal :control-W  :delete-selection ht)
    (bind-key-internal :control-\w :delete-selection ht)
    (bind-key-internal :CUT        :delete-selection ht)
    (bind-key-internal :L10        :delete-selection ht) ;; cut key on Sun
    
    (bind-key-internal :meta-W     :copy-selection ht)
    (bind-key-internal :meta-\w    :copy-selection ht)
    (bind-key-internal :COPY       :copy-selection ht)
    (bind-key-internal :L6         :copy-selection ht) ;; copy key on Sun

    (bind-key-internal :control-c  :copy-to-X-cut-buffer ht)
    
    ;; PASTES
    
    (bind-key-internal :control-\y  :yank-buffer-or-X-cut-buffer ht)
    (bind-key-internal :insert  :yank-buffer-or-X-cut-buffer ht)
    (bind-key-internal :L8      :yank-buffer-or-X-cut-buffer ht) ;paste on Sun
    (bind-key-internal :insert-line :yank-buffer-or-X-cut-buffer ht)

    (bind-key-internal :control-insert  :copy-from-X-cut-buffer ht)
    (bind-key-internal :control-Y       :copy-from-X-cut-buffer ht)
    (bind-key-internal :control-L8      :copy-from-X-cut-buffer ht)

    (bind-key-internal :meta-insert     :yank-buffer ht)
    (bind-key-internal :meta-Y          :yank-buffer ht)
    (bind-key-internal :meta-L8         :yank-buffer ht)

    ;; NEWLINES

    (bind-key-internal #\return     #\Newline ht)
    (bind-key-internal :control-J   #\Newline ht)
    (bind-key-internal :control-\j  #\Newline ht)

    (bind-key-internal :control-\o :Insert-LF-after ht)
    (bind-key-internal :control-O  :Insert-LF-after ht)
    
    ;; FONT STUFF
    
    (bind-key-internal :control-B  :toggle-bold ht)
    (bind-key-internal :control-I  :toggle-italic ht)
    (bind-key-internal :control->  :bigger ht)
    (bind-key-internal :control-<  :smaller ht)

    (bind-key-internal :control-1  :small ht)
    (bind-key-internal :control-2  :medium ht)
    (bind-key-internal :control-3  :large ht)
    (bind-key-internal :control-4  :very-large ht)
    (bind-key-internal :control-!  :small ht)
    (bind-key-internal :control-@  :medium ht)
    (bind-key-internal :control-#  :large ht)
    (bind-key-internal :control-$  :very-large ht)

    (bind-key-internal :control-F  :fixed ht)
    (bind-key-internal :control-T  :serif ht)
    (bind-key-internal :control-H  :sans-serif ht)
      
    ;; translate the number pad into regular characters (if CMU)
    #+cmu (bind-key-internal :num-pad-1 #\1 ht)
    #+cmu (bind-key-internal :num-pad-2 #\2 ht)
    #+cmu (bind-key-internal :num-pad-3 #\3 ht)
    #+cmu (bind-key-internal :num-pad-4 #\4 ht)
    #+cmu (bind-key-internal :num-pad-5 #\5 ht)
    #+cmu (bind-key-internal :num-pad-6 #\6 ht)
    #+cmu (bind-key-internal :num-pad-7 #\7 ht)
    #+cmu (bind-key-internal :num-pad-8 #\8 ht)
    #+cmu (bind-key-internal :num-pad-9 #\9 ht)
    #+cmu (bind-key-internal :num-pad-0 #\0 ht)
    #+cmu (bind-key-internal :num-pad-/ #\/ ht)
    #+cmu (bind-key-internal :num-pad-* #\* ht)
    #+cmu (bind-key-internal :num-pad-- #\- ht)
    #+cmu (bind-key-internal :num-pad-+ #\+ ht)
    #+cmu (bind-key-internal :num-pad-. #\. ht)
    #+cmu (bind-key-internal :num-pad-enter #\NewLine ht) ; the enter key

    ))

;; In addition to the standard key bindings, the lisp translation table
;; also handles tabs, parens, semi-colons, etc. specially for lisp mode
;;
(defun Set-Lisp-Key-Translations (an-interactor)
   (let ((ht (copy-hash-table
	      (get-local-value an-interactor :standard-translation-table))))
     (s-value an-interactor :lisp-translation-table ht)
     (bind-key-internal #\tab #'tab ht)
     (bind-key-internal #\; #'semi-func ht)
     (bind-key-internal #\return #'return-func ht)
     (bind-key-internal #\\ #'bslash-func ht)
     (bind-key-internal #\- #'bslash-func ht)
     (bind-key-internal #\rubout #'rubout-func ht)
     (bind-key-internal :control-\d #'cd-func ht)
     (bind-key-internal :meta-\d #'md-func ht)
     (bind-key-internal :control-\k #'ck-func ht) 
     (bind-key-internal :control-\y #'cy-func ht) 
     (bind-key-internal :control-meta-\f #'cmf-func ht)
     (bind-key-internal :control-meta-f #'cmf-func ht)
     (bind-key-internal :control-meta-rightarrow #'cmf-func ht)
     (bind-key-internal :control-meta-\b #'cmb-func ht)
     (bind-key-internal :control-meta-b #'cmb-func ht)
     (bind-key-internal :control-meta-leftarrow #'cmb-func ht)
     (bind-key-internal :control-meta-\d #'cmd-func ht)
     (bind-key-internal :control-meta-d #'cmd-func ht)
     (bind-key-internal :control-meta-\h #'cmh-func ht)
     (bind-key-internal :control-meta-h #'cmh-func ht)
     (bind-key-internal :control-meta-rubout #'cmh-func ht)     
     (bind-key-internal #\# #'hash-func ht)
     (bind-key-internal #\( #'open-paren-func ht)
     (bind-key-internal #\) #'close-paren-func ht)
     (bind-key-internal #\space #'space-func ht)
     (bind-key-internal #\| #'bar-func ht)
     (bind-key-internal #\" #'quote-func ht)))


;; Turn the cursor visibility on or off.
(defun mf-obj-or-feedback-cursor-on-off (obj-over feedback-obj turn-on-p)
  (when (or feedback-obj (schema-p obj-over)) ; otherwise, just exit because
					; no object to set
    (let ((obj (or feedback-obj obj-over))
	  line\# char\#)
      (if turn-on-p
	  (progn
	    (setq line\# (g-value obj :saved-line-index))
	    (setq char\# (g-value obj :saved-char-index))
	    (opal:SET-CURSOR-TO-LINE-CHAR-POSITION obj line\# char\#)
	    (opal:SET-CURSOR-VISIBLE obj T))
					; else save current index and turn off cursor
	  (progn
	    (multiple-value-setq (line\# char\#)
	      (opal:GET-CURSOR-LINE-CHAR-POSITION obj))
	    (s-value obj :saved-line-index line\#)
	    (s-value obj :saved-char-index char\#)
	    (opal:SET-CURSOR-VISIBLE obj nil))))))


(defun Multifont-Text-Interactor-Initialize (new-Text-schema)
   (if-debug new-Text-schema (format T "Text initialize ~s~%" new-Text-schema))
   (Check-Interactor-Type new-Text-schema inter:multifont-text-interactor)
   (Check-Required-Slots new-Text-schema)
   (Set-Up-Defaults new-Text-schema)
)

;; end initialize procedure


;; Make a copy of the orignal string in case :abort happens.
(defun Multifont-Text-Int-Start-Action (an-interactor new-obj-over start-event)
   (if-debug an-interactor (format T "Text int-start over ~s~%" new-obj-over))
   (let ((feedback (g-value an-interactor :feedback-obj))
         (startx (event-x start-event))
         (starty (event-y start-event)))
      (s-value an-interactor :startx startx)
      (s-value an-interactor :starty starty)
      (if feedback
         (progn
            (if-debug an-interactor
               (format T
                     "  * Setting :box of ~s (feedback-obj) to (~s ~s ..)~%"
                     feedback startx starty))
            (set-obj-list2-slot feedback :box startx starty)
            (s-value feedback :obj-over new-obj-over)
            (s-value an-interactor :original-string
                  (opal:GET-TEXT feedback))
            (s-value feedback :visible T)
            (opal:SET-CURSOR-TO-X-Y-POSITION feedback startx starty)
            (opal:SET-CURSOR-VISIBLE feedback T))
         ;; else modify new-obj-over
         (progn
            (s-value an-interactor :original-string
                  (opal:GET-TEXT new-obj-over))
            (when (schema-p new-obj-over)
               (opal:SET-CURSOR-TO-X-Y-POSITION new-obj-over startx starty)
               (opal:SET-CURSOR-VISIBLE new-obj-over T))))
      (obj-or-feedback-edit an-interactor new-obj-over feedback start-event)))


(defun Multifont-Text-Int-Outside-Action (an-interactor last-obj-over)
   (if-debug an-interactor (format T "Text int-outside object=~s~%"
         last-obj-over))
   (mf-obj-or-feedback-cursor-on-off last-obj-over
         (g-value an-interactor :feedback-obj) NIL))


(defun Multifont-Text-Int-Back-Inside-Action (an-interactor obj-over event)
   (if-debug an-interactor (format T "Text int-back-inside, obj-ever = ~S ~% "
         obj-over))
   (let ((feedback (g-value an-interactor :feedback-obj)))
      (mf-obj-or-feedback-cursor-on-off obj-over feedback T)
      (obj-or-feedback-edit an-interactor obj-over feedback event)))


(defun Multifont-Text-Int-Stop-Action (an-interactor obj-over event)
   (if-debug an-interactor (format T "Text int-stop over ~s~%" obj-over))
   (let ((feedback (g-value an-interactor :feedback-obj)))
      ;; ** NOTE final character is NOT edited into the string
      (mf-obj-or-feedback-cursor-on-off obj-over feedback NIL)
      (when (and feedback (schema-p obj-over))
         (opal:SET-TEXT obj-over (opal:GET-TEXT feedback)))
      (when feedback
         (s-value feedback :visible NIL))
      (when (g-value an-interactor :final-function)
         (let ((str ; try to come up with a final string for final-function
                     (if (schema-p obj-over)
                        (opal:GET-TEXT obj-over)
                        (if feedback
                           (opal:GET-TEXT feedback)
                            NIL)))
               startx starty)
            (if (g-value an-interactor :continuous)
               (progn
                  (setf startx (g-value an-interactor :startx))
                  (setf starty (g-value an-interactor :starty)))
               (progn
                  (setf startx (event-x event))
                  (setf starty (event-y event))))
            (KR-Send an-interactor :final-function an-interactor obj-over event
                  str startx starty)))))


(defun Multifont-Text-Int-Abort-Action (an-interactor orig-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Text int-abort over ~s~%" orig-obj-over))
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (if feedback
	(progn
	  (opal:SET-TEXT feedback
			 (g-value an-interactor :original-string))
	  (opal:SET-CURSOR-VISIBLE feedback nil)
	  (s-value feedback :visible NIL))
	(when (schema-p orig-obj-over)
	  (opal:SET-TEXT orig-obj-over
			 (g-value an-interactor :original-string))
	  (opal:SET-CURSOR-VISIBLE orig-obj-over NIL)))))


;;; some additional commands

;; Kill line (like in Emacs)
(defun do-kill-line (inter obj event)
  (let ((deleted-stuff (opal:kill-rest-of-line obj))
	(cut-buffer (g-value inter :cut-buffer))
	(a-window (event-window event)))
    (when deleted-stuff
      (if (g-value inter :kill-mode)
	  (setq deleted-stuff (opal:concatenate-text
				    cut-buffer deleted-stuff))
	  (s-value inter :kill-mode t))
      (s-value inter :cut-buffer deleted-stuff)
      (opal:set-x-cut-buffer a-window
			     (opal:text-to-string deleted-stuff)))))

;; remove selection into cut buffer
(defun do-delete-selection (inter obj event set-cut-buf?)
  (let* ((deleted-stuff (if (g-value inter :lisp-mode-p)
			    (inter:delete-lisp-region obj)
			    (opal:delete-selection obj)))
	 (deleted-string (opal:text-to-string deleted-stuff))
	 (a-window (event-window event)))
    (unless (string= deleted-string "")
      (when set-cut-buf? (s-value inter :cut-buffer deleted-stuff))
      (opal:set-x-cut-buffer a-window deleted-string)
      (when set-cut-buf? (curs-move inter obj)))))

;; copy the selection into cut buffer, but don't remove it
(defun do-copy-selection (inter obj event)
  (let* ((copied-stuff (opal:copy-selected-text obj))
	 (copied-string (opal:text-to-string copied-stuff))
	 (a-window (event-window event)))
    (unless (string= copied-string "")
      (s-value inter :cut-buffer copied-stuff)
      (opal:set-x-cut-buffer a-window copied-string))))

;; Yank buffer (like Emacs)
(defun do-yank-buffer (inter obj event)
  (declare (ignore event))
  (let ((yanked-stuff (g-value inter :cut-buffer)))
    (opal:insert-text obj yanked-stuff)
    (curs-move inter obj)))

;; if there is a cut-buffer, then insert it, else insert X cut buffer
(defun do-yank-buffer-or-X-cut-buffer (inter obj event)
  (let ((yanked-stuff (g-value inter :cut-buffer)))
    (if yanked-stuff
      (opal:insert-text obj yanked-stuff)
      (opal:insert-string obj
			  (Opal:Get-X-Cut-Buffer (inter:event-window event))))
    (curs-move inter obj)))


;; sets the font of the current selection, if any.  If none, then sets the
;; default font for the string-object.  Family, face and size can be NIL, a
;; real value or a special value.
(defun Set-Font-Changing (string-object family face size)
  (if (g-value string-object :selection-p)
      (progn
	(when family 
	  (opal:change-font-of-selection string-object NIL :family family))
	(when size
	  (opal:change-font-of-selection string-object NIL :size size))
	(when face
	  (case face
	    ((NIL) NIL)
	    (:bold (opal:change-font-of-selection string-object NIL :bold T))
	    (:italic (opal:change-font-of-selection string-object NIL
						    :italic T))
	    (:bold-italic (opal:change-font-of-selection string-object NIL
							 :italic T :bold T))
	    (:toggle-bold (opal:change-font-of-selection string-object NIL
							  :bold :toggle-first))
	    (:toggle-italic (opal:change-font-of-selection string-object NIL
						   :italic :toggle-first)))))

      ;; otherwise set the default font
      (let* ((font (g-value string-object :current-font)))
	(unless family
	  (setf family (g-value font :family)))
	(ecase face
	  ((NIL) (setq face (g-value font :face)))
	  ((:bold :italic :bold-italic :roman)) ; face is fine
	  (:toggle-bold (setq face (ecase (g-value font :face)
				     (:bold NIL)
				     (:roman :bold)
				     (:italic :bold-italic)
				     (:bold-italic :italic))))
	  (:toggle-italic (setq face (ecase (g-value font :face)
				       (:italic NIL)
				       (:roman :italic)
				       (:bold :bold-italic)
				       (:bold-italic :bold)))))
	(ecase size
	  ((NIL) (setq size (g-value font :size)))
	  ((:small :medium :large :very-large)) ; size is fine
	  (:bigger (setq size (ecase (g-value font :size)
				(:small :medium)
				(:medium :large)
				(:large :very-large)
				(:very-large :very-large))))
	  (:smaller (setq size (ecase (g-value font :size)
				 (:small :small)
				 (:medium :small)
				 (:large :medium)
				 (:very-large :large)))))
	(s-value string-object :current-font
		 (opal:get-standard-font family face size)))))


(defun curs-move (inter string-object)
  (kr-send inter :after-cursor-moves-func inter string-object)) 

(defparameter Shift-Bit
  (gem:create-state-mask (g-value gem:device-info :current-root) :shift))

;; event is a mouse event, not a move, see if down or shift-down
(defun Handle-Move-Cursor (an-interactor string-object event)
  ;; then see if want to move cursor
  (let ((x (event-x event))
	(y (event-y event)))
    (cond ((and (event-downp event)
		(g-value an-interactor :cursor-where-press))
	   ;; see if shift down.
	   (if (zerop (logand (event-state event) Shift-Bit))
	       ;; not shift
	       (progn
		 (opal:toggle-selection string-object NIL)
		 (opal:set-cursor-to-x-y-position string-object x y)
		 (opal:SET-CURSOR-VISIBLE string-object T)
		 (curs-move an-interactor string-object)
		 (when (g-value an-interactor :drag-through-selection?)
		   (s-value an-interactor :dragging-now T)))
	       ;; else extend selection
	       (when (g-value an-interactor :drag-through-selection?)
		 (opal:toggle-selection string-object T)
		 (opal:set-selection-to-x-y-position string-object x y))
	       ))
	  ((and (event-code event)(null (event-downp event)))
	   (s-value an-interactor :dragging-now NIL)))))
    
(defun Handle-Drag-Through (string-object event)
  (opal:toggle-selection string-object T)
  (opal:set-selection-to-x-y-position string-object
				      (event-x event) (event-y event)))

;; On a delete operation, if there is a selection, delete it and don't do the
;; regular delete operation.
(defun check-delete-selection (string-object lisp-mode-p)
  (and (g-value string-object :selection-p)
       (if lisp-mode-p
	   (inter:delete-lisp-region string-object)
	   (opal:delete-selection string-object))))

;; Does the same stuff as inter:Edit-String (in textkeyhandling.lisp)
;; but string-object is of type opal:multifont-text.
(defun MultiFont-Edit-String (an-interactor string-object event)
   (if (or (null event) (not (schema-p string-object)))
      NIL ; ignore this event and keep editing
      ; else
      (let* ((char (event-char event))
	     (new-trans-char (inter::Translate-key char an-interactor))
	     (last-edited-string (g-value an-interactor :last-edited-string)))
	;; make sure there isn't a selection visible in another
	;; string, in case this interactor is operating over multiple
	;; string objects.
	(unless (eq last-edited-string string-object)
	  (s-value an-interactor :last-edited-string string-object)
	  (when (and last-edited-string
		     (is-a-p last-edited-string opal:multifont-text))
	    (opal:toggle-selection last-edited-string NIL)))
	(if (and (eq char :mouse-moved)
		 (g-value an-interactor :drag-through-selection?)
		 (g-value an-interactor :dragging-now))
	    (Handle-Drag-Through string-object event)
	    ;; else deal with the character
	    (when new-trans-char
	      (unless (eq new-trans-char :kill-line)
		(s-value an-interactor :kill-mode nil))
	      (if-debug an-interactor
			(format T "Key ~s translated to ~s~%" char
				new-trans-char))
	      (case new-trans-char
                  (:prev-char (opal:toggle-selection string-object nil)
			      (opal:go-to-prev-char string-object)
			      (curs-move an-interactor string-object))
		  (:prev-char-select (opal:toggle-selection string-object t)
				     (opal:go-to-prev-char string-object)
				     (curs-move an-interactor string-object))
                  (:next-char (opal:toggle-selection string-object nil)
			      (opal:go-to-next-char string-object)
			      (curs-move an-interactor string-object))
		  (:next-char-select (opal:toggle-selection string-object t)
				     (opal:go-to-next-char string-object)
				     (curs-move an-interactor string-object))
		  (:prev-word (opal:toggle-selection string-object nil)
			      (opal:go-to-prev-word string-object)
			      (curs-move an-interactor string-object))
		  (:prev-word-select (opal:toggle-selection string-object t)
				     (opal:go-to-prev-word string-object)
				     (curs-move an-interactor string-object))
		  (:next-word (opal:toggle-selection string-object nil)
			      (opal:go-to-next-word string-object)
			      (curs-move an-interactor string-object))
		  (:next-word-select (opal:toggle-selection string-object t)
				     (opal:go-to-next-word string-object)
				     (curs-move an-interactor string-object))
                  (:up-line (opal:toggle-selection string-object nil)
			    (opal:go-to-prev-line string-object)
			    (curs-move an-interactor string-object))
		  (:up-line-select (opal:toggle-selection string-object t)
				   (opal:go-to-prev-line string-object)
				   (curs-move an-interactor string-object))
                  (:down-line (opal:toggle-selection string-object nil)
			      (opal:go-to-next-line string-object)
			      (curs-move an-interactor string-object))
		  (:down-line-select (opal:toggle-selection string-object t)
				     (opal:go-to-next-line string-object)
				     (curs-move an-interactor string-object))
                  (:beginning-of-string
		   (opal:toggle-selection string-object nil)
		   (opal:go-to-beginning-of-text string-object)
		   (opal:set-cursor-visible string-object t)
		   (curs-move an-interactor string-object))
		  (:beginning-of-string-select
                     (opal:toggle-selection string-object t)
                     (opal:go-to-beginning-of-text string-object)
		     (curs-move an-interactor string-object))
		  (:beginning-of-line
		   (opal:toggle-selection string-object nil)
		   (opal:go-to-beginning-of-line string-object)
		   (curs-move an-interactor string-object))
		  (:beginning-of-line-select
                     (opal:toggle-selection string-object t)
                     (opal:go-to-beginning-of-line string-object)
		     (curs-move an-interactor string-object))
                  (:end-of-string
		   (opal:toggle-selection string-object nil)
		   (opal:go-to-end-of-text string-object)
		   (opal:set-cursor-visible string-object t)
		   (curs-move an-interactor string-object))
		  (:end-of-string-select
                     (opal:toggle-selection string-object t)
                     (opal:go-to-end-of-text string-object)
		     (curs-move an-interactor string-object))
		  (:end-of-line (opal:toggle-selection string-object nil)
				(opal:go-to-end-of-line string-object)
				(curs-move an-interactor string-object))
		  (:end-of-line-select (opal:toggle-selection string-object t)
				       (opal:go-to-end-of-line string-object)
				       (curs-move an-interactor string-object))
		  (:select-all 
		   (opal:toggle-selection string-object NIL)
		   (opal:go-to-beginning-of-text string-object)
		   (opal:toggle-selection string-object T)
		   (opal:go-to-end-of-text string-object)
		   (curs-move an-interactor string-object))
                  (:delete-prev-char
		   (unless (check-delete-selection
			    string-object (g-value an-interactor :lisp-mode-p))
		     (opal:delete-prev-char string-object))
		   (curs-move an-interactor string-object))
                  (:delete-prev-word
		   (unless (check-delete-selection
			    string-object (g-value an-interactor :lisp-mode-p))
		     (opal:delete-prev-word string-object))
		   (curs-move an-interactor string-object))
                  (:delete-next-char
		   (unless (check-delete-selection
			    string-object (g-value an-interactor :lisp-mode-p))
		     (opal:delete-char string-object)))
		  (:delete-next-word
		   (unless (check-delete-selection
			    string-object (g-value an-interactor :lisp-mode-p))
		     (opal:delete-word string-object)))
                  (:delete-string (opal:set-text string-object nil)
				  (curs-move an-interactor string-object))
		  (:kill-line (do-kill-line an-interactor string-object event))
		  (:delete-selection
		   (do-delete-selection an-interactor string-object event T))
		  (:copy-selection
		   (do-copy-selection an-interactor string-object event))
		  (:copy-to-X-cut-buffer
		   (Opal:Set-X-Cut-Buffer (inter:event-window event)
					  (opal:get-string string-object)))

		  (:yank-buffer
		   (do-yank-buffer an-interactor string-object event)
		   (curs-move an-interactor string-object))
		  (:yank-buffer-or-X-cut-buffer
		   (do-yank-buffer-or-X-cut-buffer an-interactor string-object
						   event))
		  (:copy-from-X-cut-buffer
		   (opal:insert-string string-object
			   (Opal:Get-X-Cut-Buffer (inter:event-window event)))
		   (curs-move an-interactor string-object))

		  (:Insert-LF-after
		     (if (g-value an-interactor :lisp-mode-p)
			 (inter:add-lisp-char string-object #\newline)
			 (opal:add-char string-object #\newline))
		     (opal:go-to-prev-char string-object))

		  (:toggle-bold
		   (Set-Font-Changing string-object NIL :toggle-bold NIL))
		  (:bold (Set-Font-Changing string-object NIL :bold NIL))
		  (:italic (Set-Font-Changing string-object NIL :italic NIL))
		  (:bold-italic (Set-Font-Changing string-object NIL
						   :bold-italic NIL))
		  (:roman (Set-Font-Changing string-object NIL :roman NIL))
		  (:toggle-italic
		   (Set-Font-Changing string-object NIL :toggle-italic NIL))
		  (:bigger (Set-Font-Changing string-object NIL NIL :bigger))
		  (:smaller (Set-Font-Changing string-object NIL NIL :smaller))
		  (:small (Set-Font-Changing string-object NIL NIL :small))
		  (:medium (Set-Font-Changing string-object NIL NIL :medium))
		  (:large (Set-Font-Changing string-object NIL NIL :large))
		  (:very-large (Set-Font-Changing string-object NIL NIL`
						  :very-large))
		  (:fixed (Set-Font-Changing string-object :fixed NIL NIL))
		  (:serif (Set-Font-Changing string-object :serif NIL NIL))
		  (:sans-serif (Set-Font-Changing string-object
						  :sans-serif NIL NIL))
                  (T ;; here might be a keyword, char, string, func, or mouse
		   (cond
		     ((and (characterp new-trans-char)
			   (or (graphic-char-p new-trans-char)
			       (eql new-trans-char #\NewLine)))
		      ;; then is a regular character
		      (when (g-value string-object :selection-p)
			(do-delete-selection an-interactor string-object event
					     NIL))
		      ;; add char to string
		      (if (g-value an-interactor :lisp-mode-p)
			 (inter:add-lisp-char string-object new-trans-char)
			 (opal:add-char string-object new-trans-char))
		      (curs-move an-interactor string-object))
		     ;; check if a string
		     ((stringp new-trans-char) ; then insert into string
		      (when (g-value string-object :selection-p)
			(do-delete-selection an-interactor string-object event
					     T))
		      (opal:Insert-String string-object new-trans-char)
		      (curs-move an-interactor string-object))
		     ;; now check for functions
		     ((garnet-utils:safe-functionp new-trans-char)
					; then call the function
		      (funcall new-trans-char an-interactor string-object
			       event)
		      (curs-move an-interactor string-object))
		     ((event-mousep event)  
		      (if (is-a-p an-interactor MULTIFONT-TEXT-INTERACTOR)
			 (Handle-Move-Cursor an-interactor string-object event)
			  ;; else is a focus-multifont so don't do anything
			  NIL))
		     (T ; otherwise, must be a bad character
		      (Beep))))))))))


(defun MultiFont-Text-do-start (an-interactor obj-over event)
  (if-debug an-interactor (format T "Multi-Font starting over ~s~%" obj-over))
        ;; if obj-to-change supplied, then use that, otherwise use whatever was
	;; under the mouse when started
  (let ((obj (or (g-value an-interactor :obj-to-change) obj-over))
	need-moved)
    (if (g-value an-interactor :continuous)  ;then will go to running state
	(progn
	  (setq need-moved (g-value an-interactor :drag-through-selection?))
	  (Fix-Running-Where an-interactor obj-over)
	  (s-value an-interactor :remembered-object obj) ; object to edit
	  (s-value an-interactor :dragging-now NIL)
	  (GoToRunningState an-interactor
			    (if (eq T (Get-Running-where an-interactor))
				need-moved
				T))  ; need mouse moved to see if outside
	  (kr-send an-interactor :start-action an-interactor obj event))
	;else call stop-action
	(progn
	  (GoToStartState an-interactor NIL)
	  (kr-send an-interactor :stop-action an-interactor obj event)))))


;;; Text schema
;;

;; Here's the actual interactor.
(Create-Schema 'inter:MULTIFONT-TEXT-INTERACTOR
      (:is-a inter:text-interactor)
      (:lisp-mode-p NIL)
      (:match-parens-p NIL)
      (:match-obj NIL)
      (:key-translation-table (o-formula (if (gvl :lisp-mode-p)
					     (gvl :lisp-translation-table)
					   (gvl :standard-translation-table))))
      (:standard-translation-table NIL) ;table of translations; set below
      (:lisp-translation-table NIL) ;table of translations for lisp; set below
      (:after-cursor-moves-func (o-formula (when (gvl :match-parens-p)
						 #'check-parens)))
      (:running-where T)
      (:drag-through-selection? T) 
      (:start-action 'Multifont-Text-Int-Start-Action)
      (:Do-Start 'MultiFont-Text-do-start) ; special start for drag-through
      (:edit-func 'Multifont-Edit-String)
      (:stop-action 'Multifont-Text-Int-Stop-Action)
      (:abort-action 'Multifont-Text-Int-Abort-Action)
      (:outside-action 'Multifont-Text-Int-Outside-Action)
      (:back-inside-action 'Multifont-Text-Int-Back-Inside-Action)
      (:initialize 'Multifont-Text-Interactor-Initialize))

(Set-MultiFont-Default-Key-Translations inter:MULTIFONT-TEXT-INTERACTOR)
(Set-Lisp-Key-Translations inter:MULTIFONT-TEXT-INTERACTOR)
