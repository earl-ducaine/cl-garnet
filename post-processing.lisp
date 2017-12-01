(in-package :common-lisp-user)


(eval-when (:load-toplevel :compile-toplevel :execute)
  (let ((demo-apps
	 '("(demo-3d::do-go)"
	   "(demo-angle::do-go)"
	   "(demo-animator::do-go)"
	   "(demo-arith::do-go)"
	   "(demo-array::do-go)"
	   "(demo-clock::do-go)"
	   "(demo-editor::do-go)"
	   "(demo-file-browser::do-go)"
	   "(demo-gadgets::do-go)"
	   "(demo-3d::do-go)"
	   "(demo-gesture::do-go)"
	   "(demo-graph::do-go)"
	   "(demo-grow::do-go)"
	   "(demo-logo::do-go)"
	   "(demo-manyobjs::do-go)"
	   "(demo-menu::do-go)"
	   "(demo-mode::do-go)"
	   "(demo-motif::do-go)"
	   "(demo-moveline::do-go)"
	   "(demo-multifont::do-go)"
	   "(demo-multiwin::do-go)"
	   "(demo-othello::do-go)"
	   "(demo-pixmap::do-go)"
	   "(demo-schema-browser::do-go)"
	   "(demos-compiler::do-go)"
	   "(demos-controller::do-go)"
	   "(demo-scrollbar::do-go)"
	   "(demo-sequence::do-go)"
	   "(demo-text::do-go)"
	   "(demo-truck::do-go)"
	   "(demo-twop::do-go)"
	   "(demo-unistrokes::do-go)"
	   "(demo-virtual-agg::do-go)"
	   "(demo-xasperate::do-go)"
	   "(garnet-calculator::do-go)"
	   "(garnetdraw::do-go)"
	   "(mge::do-go)"
	   "(quicklisp::do-go)"
	   "(tourcommands::do-go)"
	   "(tour::do-go)"
	   "(tour-transcript::do-go)")))
    (format t "To run various app eval one of the following:")
    (dolist (demo-app demo-apps)
      (format t "~a~%" demo-app))))
