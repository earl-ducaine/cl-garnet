
(defpackage :garnet-utils
  (:use :common-lisp)
  (:nicknames :gu)
  (:export *garnet-break-key*
	   safe-functionp
	   while))


(defpackage :kr
  (:use :common-lisp )
  (:export schema
	   create-instance
	   create-prototype
	   create-relation
	   create-schema
	   formula
	   o-formula
	   schema-p
	   relation-p
	   is-a-p
	   has-slot-p
	   formula-p
	   s-value
	   g-value
	   g-cached-value
	   g-local-value
	   gv
	   gvl
	   gv-local
	   get-value
	   get-local-value
	   dovalues
	   doslots
	   define-method
	   kr-send
	   call-prototype-method
	   *print-as-structure*
	   with-constants-disabled
	   with-types-disabled
	   with-demons-disabled
	   with-demon-disabled
	   with-demon-enabled
	   change-formula
	   move-formula
	   recompute-formula
	   copy-formula
	   kr-path
	   mark-as-changed
	   mark-as-invalid
	   ps
	   call-on-ps-slots
	   name-for-schema
	   declare-constant
	   slot-constant-p
	   destroy-slot
	   destroy-schema
	   destroy-constraint
	   def-kr-type
	   g-type
	   s-type
	   check-slot-type
	   kr-boolean
	   get-slot-doc
	   set-slot-doc
	   get-type-documentation
	   set-type-documentation
	   get-type-definition
	   get-declarations
	   get-slot-declarations
	   g-formula-value
	   s-formula-value
	   self-old-value))


(defpackage :gem
  (:use :common-lisp :kr )
  (:export *black*

  ))



(defpackage :garnet-user
  (:use common-lisp kr)
  (:export go-demos))



(defpackage :test
  (:use :common-lisp))

(defpackage :opal
  (:use :common-lisp :kr)


)

(defpackage :garnet-debug
  (:use :common-lisp :kr :opal) (:nicknames :gd))

(defpackage :gilt
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :lapidary
  (:use common-lisp kr)
  (:export clean-up
	   directional-move-grow-interactor
	   do-go
	   do-stop
	   fix-it
	   lapidary-angle-interactor
	   lapidary-button-interactor
	   lapidary-menu-interactor
	   lapidary-text-interactor
	   lapidary-two-point-interactor))


(defpackage :agate
  (:use :common-lisp :kr))

(defpackage :demo-3d
  (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :xomax
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-multiwin
  (:use kr common-lisp)
  (:export do-go do-stop))

(defpackage :demo-multifont
  (:use common-lisp kr)
  (:export do-go do-stop))

(defpackage :demo-animator (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-angle (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-othello (:use :kr :common-lisp) (:nicknames :doth)
	    (:export do-go do-stop start-game stop-game set-score))

(defpackage :demo-pixmap (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-arith (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-schema-browser (:use :common-lisp :kr)
	    (:export do-go do-stop schema-browser schema-browser-win
		     schema-browser-top-agg))
(defpackage :demo-array (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-scrollbar (:use :common-lisp :kr)
	    (:export do-go do-stop
		     mac-obj mac-go mac-stop
		     open-obj open-go open-stop
		     next-obj next-go next-stop
		     motif-obj motif-go motif-stop))
(defpackage :demo-clock (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-sequence (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-editor (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-text (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-file-browser (:use :common-lisp :kr)
	    (:export do-go do-stop file-browser file-browser-win
		     file-browser-top-agg))
(defpackage :demo-truck (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-gadgets (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-twop (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-gesture (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-unistrokes
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-graph (:use :common-lisp :kr)
	    (:export do-go do-stop schema-graph
		     demo-graph-error-gadget root-box relayout
		     demo-graph-win))

(defpackage :demo-virtual-agg
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-grow (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-xasperate
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-logo
  (:use :common-lisp :kr)
  (:export do-go do-stop re-animate))

(defpackage :demos-controller
  (:use :common-lisp :kr)
  (:export do-go do-stop message))

(defpackage :demo-manyobjs
  (:use :common-lisp :kr)
  (:export do-go do-stop move))

(defpackage :demo-menu (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :garnet-calculator (:use :common-lisp :kr)
	    (:export start-calc stop-calc do-go do-stop))

(defpackage :demo-mode (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :garnetdraw (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-motif (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :mge (:use :common-lisp :kr)
	    (:export do-go do-stop
		     create-piece destroy-piece destroy-all-pieces
		     go-initialize editor-show-window))

(defpackage :demo-moveline (:use :kr :common-lisp) (:export do-go do-stop))
