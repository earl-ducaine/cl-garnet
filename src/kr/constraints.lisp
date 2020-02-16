
(in-package :kr)


(add-new-type "KNOWN-AS-TYPE" '(or keyword null)
              #'(lambda (value)
                  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
                  (or (keywordp value) (null value))))


(ADD-NEW-TYPE "FIXNUM" '(SATISFIES SB-INT:FIXNUMP) 'SB-INT:FIXNUMP)


(create-instance 'graphical-object nil
  :declare ((:type
	     ((or (is-a-p line-style) null) :line-style)
	     ((or (is-a-p filling-style) null) :filling-style))))

(create-instance 'rectangle graphical-object
  :declare ((:update-slots :fast-redraw-p :top)))

(define-method :initialize graphical-object (gob)
	       (S-VALUE-FN GOB :UPDATE-INFO 'A))
