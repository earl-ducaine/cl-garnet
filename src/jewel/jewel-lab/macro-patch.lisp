(in-package :xlib)


(defmacro with-buffer-output ((buffer &key (sizes '(8 16 32)) length index) &body body)
  (unless (listp sizes) (setq sizes (list sizes)))
  `(let ((%buffer ,buffer))
     (declare (type display %buffer))
     ,(declare-bufmac)
     ,(when length
	`(when (index>= (index+ (buffer-boffset %buffer) ,length) (buffer-size %buffer))
	   (buffer-flush %buffer)))
     (let* ((buffer-boffset (the array-index ,(or index `(buffer-boffset %buffer))))
	    (buffer-bbuf (buffer-obuf8 %buffer)))
       (format pixmap-lab::*standard-output* "(length buffer-bbuf) -- ~s~%" (length buffer-bbuf))
       (finish-output pixmap-lab::*standard-output*)
       buffer-boffset
       buffer-bbuf
       ,@body)))



;; (defun get-put-items (index type-args putp &optional body-function)
;;   ;; Given a lists of the form (type item item ... item)
;;   ;; Calls body-function with four arguments, a function name,
;;   ;; index, item name, and optional arguments.
;;   ;; The results are appended together and retured.
;;   (format t "(index: ~s, type-args:~s, putp:~s, body-function:~s,~%"
;; 	  index type-args putp body-function)
;;   (unless body-function
;;     (setq body-function
;; 	  #'(lambda (type index item args)
;; 	      `((check-put ,index ,item ,type ,@args)))))
;;   (do* ((items type-args (cdr items))
;; 	(type (caar items) (caar items))
;; 	(args nil nil)
;; 	(result nil)
;; 	(sizes nil))
;;        ((endp items) (values result index sizes))
;;     (when (consp type)
;;       (setq args (cdr type)
;; 	    type (car type)))
;;     (cond ((member type '(return buffer)))
;; 	  ((eq type 'mask) ;; Hack to enable mask-get/put to return multiple values
;; 	   (setq result
;; 		 (append result (if putp
;; 				    (mask-put index (cdar items) body-function)
;; 				  (mask-get index (cdar items) body-function)))
;; 		 index nil))
;; 	  (t (do* ((item (cdar items) (cdr item))
;; 		   (increment (index-increment type)))
;; 		  ((endp item))
;; 	       (when (constantp index)
;; 		 (case increment		;Round up index when needed
;; 		   (2 (setq index (wround index)))
;; 		   (4 (setq index (lround index)))))
;; 	       (setq result
;; 		     (append result (funcall body-function type index (car item) args)))
;; 	       (when (constantp index)
;; 		 ;; Variable length requests have null length increment.
;; 		 ;; Variable length requests set the request size
;; 		 ;; & maintain buffer pointers
;; 		 (if (null increment)
;; 		     (setq index nil)
;; 		   (progn
;; 		     (incf index increment)
;; 		     (when (and increment (zerop increment)) (setq increment 1))
;; 		     (pushnew (* increment 8) sizes)))))))))
