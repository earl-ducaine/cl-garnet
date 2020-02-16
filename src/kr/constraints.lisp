
(in-package :kr)


(ADD-NEW-TYPE "KNOWN-AS-TYPE" '(OR KEYWORD NULL)
              #'(LAMBDA (VALUE)
                  (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (SPACE 0) (DEBUG 0)))
                  (OR (KEYWORDP VALUE) (NULL VALUE))))


(ADD-NEW-TYPE "FIXNUM" '(SATISFIES SB-INT:FIXNUMP) 'SB-INT:FIXNUMP)


(create-instance 'graphical-object nil
  :declare ((:type
	     ((or (is-a-p line-style) null) :line-style)
	     ((or (is-a-p filling-style) null) :filling-style))))

(create-instance 'rectangle graphical-object
  :declare ((:update-slots :fast-redraw-p :top)))

(define-method :initialize graphical-object (gob)
	       (S-VALUE-FN GOB :UPDATE-INFO 'A))
