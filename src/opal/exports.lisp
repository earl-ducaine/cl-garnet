
(in-package "OPAL")

(defvar *debug-opal-mode* nil)
(setf (get :garnet-modules :opal) T)

;;; Import some stuff from GEM that used to be in OPAL.
(eval-when (:execute :load-toplevel :compile-toplevel)
  (import '(gem:Display-Info
	    gem:Make-Display-Info gem:Copy-Display-Info
	    gem:Display-Info-Display gem:Display-Info-Screen gem:Display-Info-Root-Window
	    gem:Display-Info-Line-Style-GC gem:Display-Info-Filling-Style-GC

	    gem:*Small-Font-Point-Size* gem:*Medium-Font-Point-Size*
	    gem:*Large-Font-Point-Size* gem:*Very-Large-Font-Point-Size*
	    gem:default-font-from-file)
	  (find-package "OPAL")))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(*debug-opal-mode* bottom right center-x center-y
	    *garnet-windows*
	   aggregate
	    read-image
	    reconnect-garnet
	    rectangle
	    clip-and-map
	    drawable-to-window)))
