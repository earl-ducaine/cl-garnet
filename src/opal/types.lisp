;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: KR; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Change log:
;;;
;;;  3-05-94 amickish - Allowed lists for font-face (for Mac)
;;; 12-20-93 amickish - Added (or string cons) for font-names
;;; 08-23-93 amickish - Named lots of types, and added documentation strings
;;; 07-07-93 amickish - Added Check-Menubar-Items type; put some types in
;;;            different packages for LispWorks
;;; 03-06-93 amickish - Added (integer 1); added interactor type declarations


(in-package "KR")

;; kr-boolean is now defined in KR itself

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(items-type accelerators-type known-as-type filename-type
	    inter-window-type))

  (proclaim '(special opal::color opal::bitmap opal::filling-style
	      opal::line-style opal::font opal::font-from-file opal::aggregate
	      inter::interactor-window inter::priority-level)))

;;;; Named types used in Opal, Interactors, etc.

(def-kr-type ITEMS-TYPE () 'list
  "[list of items: (\"Label2\"...)]")

(def-kr-type ACCELERATORS-TYPE () 'list
  "[list of lists: ((#\r \"Alt-r\" #\meta-r)...)]")

(def-kr-type KNOWN-AS-TYPE () '(or keyword null)
  "[keyword]")

(def-kr-type FILENAME-TYPE () '(or string pathname)
  "[file namestring]")

(def-kr-type PATHNAME () 'pathname
  "[pathname]")

(def-kr-type FONT-FAMILY ()
  '(MEMBER :FIXED :SERIF :SANS-SERIF)
  "[one of :FIXED, :SERIF, or :SANS-SERIF]")

(def-kr-type FONT-FACE ()
  '(or (member :roman :bold :italic :bold-italic) list)
  "[one of :ROMAN, :BOLD, :ITALIC, or :BOLD-ITALIC or a list]")

(def-kr-type FONT-SIZE ()
  '(member :small :medium :large :very-large)
  "[one of :SMALL, :MEDIUM, :LARGE, or :VERY-LARGE]")

(def-kr-type FILL-STYLE ()
  '(member :solid :stippled :opaque-stippled)
  "[one of :SOLID, :STIPPLED, or :OPAQUE-STIPPLED]")

(def-kr-type DRAW-FUNCTION ()
  '(member :copy :xor :no-op :or :clear :set
    :copy-inverted :invert :and :equiv :nand :nor
    :and-inverted :and-reverse :or-inverted :or-reverse)
  "[one of :COPY, :XOR, :NO-OP, :OR, :CLEAR, :SET,
                :COPY-INVERTED, :INVERT, :AND, :EQUIV, :NAND, :NOR,
                :AND-INVERTED, :AND-REVERSE, :OR-INVERTED, or :OR-REVERSE]")

(def-kr-type H-ALIGN ()
  '(member :left :center :right)
  "[one of :LEFT, :CENTER, or :RIGHT]")

(def-kr-type V-ALIGN ()
  '(member :top :center :bottom)
  "[one of :TOP, :CENTER, or :BOTTOM]")

(def-kr-type DIRECTION ()
  '(member :vertical :horizontal)
  "[either :VERTICAL or :HORIZONTAL]")

(def-kr-type DIRECTION-OR-NIL ()
  '(or (member :vertical :horizontal) null)
  "[either :VERTICAL, :HORIZONTAL, or NIL]")

(def-kr-type COLOR ()
  '(is-a-p opal::color)
  "[an instance of opal:COLOR]")

(def-kr-type COLOR-OR-NIL ()
  '(or null (is-a-p opal::color))
  "[either an instance of opal:COLOR or NIL]")

(def-kr-type BITMAP ()
  '(is-a-p opal::bitmap)
  "[an instance of opal:BITMAP]")

(def-kr-type BITMAP-OR-NIL ()
  '(or null (is-a-p opal::bitmap))
  "[either an instance of opal:BITMAP or NIL]")

(def-kr-type LINE-STYLE ()
  '(is-a-p opal::line-style)
  "[an instance of opal:LINE-STYLE]")

(def-kr-type LINE-STYLE-OR-NIL ()
  '(or (is-a-p opal::line-style) null)
  "[either an instance of opal:LINE-STYLE or NIL]")

(def-kr-type FILLING-STYLE ()
  '(is-a-p opal::filling-style)
  "[an instance of opal:FILLING-STYLE]")

(def-kr-type FILLING-STYLE-OR-NIL ()
  '(or (is-a-p opal::filling-style) null)
  "[either an instance of opal:FILLING-STYLE or NIL]")

(def-kr-type WINDOW ()
  '(is-a-p inter::interactor-window)
  "[an instance of inter::INTERACTOR-WINDOW]")

(def-kr-type WINDOW-OR-NIL ()
  '(or (is-a-p inter::interactor-window) null)
  "[either an instance of inter:INTERACTOR-WINDOW or NIL]")

(def-kr-type FONT ()
  '(or (is-a-p opal::font) (is-a-p opal::font-from-file))
  "[either an instance of opal:FONT or opal:FONT-FROM-FILE]")

(def-kr-type PRIORITY-LEVEL ()
  '(is-a-p inter::priority-level)
  "[an instance of inter:PRIORITY-LEVEL]")

(def-kr-type AGGREGATE ()
  '(is-a-p opal::aggregate)
  "[an instance of opal:AGGREGATE]")

(def-kr-type AGGREGATE-OR-NIL ()
  '(or (is-a-p opal::aggregate) null)
  "[either an instance of opal:AGGREGATE or NIL]")

(def-kr-type BOOLEAN ()
  '(member t nil))

(def-kr-type FIXNUM ()
  '(satisfies
    #+allegro excl:fixnump
    #+ccl ccl:fixnump
    #+cmu ext:fixnump
    #+sbcl sb-int:fixnump
    #-(or allegro ccl cmu sbcl) integerp)
  "Potential efficiency hack.")

;;;; Unnamed types used in Opal, Interactors, etc.

(def-kr-type '(member :even-odd :winding))

(def-kr-type '(MEMBER 0 1 2 3))

(def-kr-type '(OR (MEMBER :BELOW :LEFT :RIGHT) LIST))

(def-kr-type '(OR LIST STRING))

(def-kr-type '(OR LIST (MEMBER T)))

(def-kr-type '(OR LIST (SATISFIES SCHEMA-P)))

(def-kr-type '(OR STRING ATOM))

(def-kr-type '(OR STRING CONS))

(def-kr-type '(OR STRING (SATISFIES SCHEMA-P)))

(def-kr-type '(OR FUNCTION SYMBOL))

(def-kr-type '(OR LIST INTEGER FUNCTION SYMBOL))

(def-kr-type '(OR NULL FUNCTION SYMBOL))

(def-kr-type '(OR NULL INTEGER))

(def-kr-type '(OR NULL (INTEGER 0)))

(def-kr-type '(OR NULL STRING))

(def-kr-type '(OR NULL (SATISFIES SCHEMA-P)))

(def-kr-type '(OR LIST (IS-A-P inter::INTERACTOR-WINDOW)))

(def-kr-type '(OR NUMBER NULL))

(def-kr-type '(REAL 0 1))

(def-kr-type '(INTEGER 0 1))

(def-kr-type '(INTEGER 0))

(def-kr-type '(INTEGER 1))

(def-kr-type '(INTEGER 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "OPAL")

(def-kr-type '(OR KEYWORD (INTEGER 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GARNET-GADGETS")

(def-kr-type '(OR NULL STRING KEYWORD (SATISFIES SCHEMA-P)))

(def-kr-type '(OR STRING KEYWORD (SATISFIES SCHEMA-P)))

(def-kr-type '(OR KEYWORD CHARACTER LIST))

(def-kr-type '(OR NULL KEYWORD CHARACTER))

(def-kr-type '(SATISFIES CHECK-MENUBAR-ITEMS))




;;;;;;;;;;;;;;;;;; Types specifically for inter::interactor ;;;;;;;;;;;;;;;;

(in-package "INTER")

(defun list-of-wins-p (l)
  (let ((succeeded? T))
    (when (listp l)
      (dolist (w l)
	(if (not (is-a-p w inter::interactor-window))
	    (setf succeeded? NIL)))
      succeeded?)))

(def-kr-type inter-window-type ()
  '(or null (is-a-p inter::interactor-window)
    (member T)
    (satisfies list-of-wins-p)))

