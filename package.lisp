
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
	   is-a-p
	   has-slot-p
	   formula-p
	   s-value
	   g-value
	   gv
	   gvl
	   define-method
	   ))

(defpackage :opal
  (:use :common-lisp :kr))

(defpackage :mge (:use :common-lisp :kr)
	    (:export do-go do-stop
		     create-piece destroy-piece destroy-all-pieces
		     go-initialize editor-show-window))
