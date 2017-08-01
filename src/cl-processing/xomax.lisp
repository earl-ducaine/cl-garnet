
(in-package :xomax)

(defvar *current-buffer* nil)
(defvar *buffers* '())

(defun current-buffer ()
  *current-buffer*)

(defun buffer-name (&optional buffer)
  (let ((current-buffer (or buffer *current-buffer*)))
    (when current-buffer
      (kr:gv current-buffer :buffer-name))))

(defun get-buffer (name)
  (let ((buffers *buffers*))
    (do ((buffer  (car buffers) (car buffers))
	 (buffers (cdr buffers) (cdr buffers)))
	((or (null buffer)
	     (string= (buffer-name buffer) name))
	 buffer))))

(defun generate-anonymous-buffer ()
  (kr:create-instance nil opal:multifont-text 
    (:word-wrap-p t)			      
    (:auto-scroll-p T)
    (:fast-redraw-p :rectangle)
    (:fast-redraw-filling-style opal:motif-gray-fill)))

(defun generate-new-buffer-name (name)
  (let ((i 0))
    (do ((generated-name name (concatenate 'string name (write-to-string i))))
	((not (get-buffer generated-name))
	 (let ((buffer (generate-anonymous-buffer)))
	   (kr:s-value buffer :buffer-name name)
	   (push buffer *buffers*)
	   buffer)))))

;; return a buffer which is:
;; - newly alocated
;; - empty
;; - the buffer name is the return value of (generate-new-buffer-name
;;   name), i.e. not necessarily the value of name.  Note, name must be a string.
;; - the newly created is not the current one.  (presumably this is the
;;   case even when there are fewer buffers than frames.
;; - the buffer will have a mode of fundamental mode.
;; todo: needs no return name if name is buffer
(defun generate-new-buffer (name)
  (generate-new-buffer-name name))

(defun buffer-name (buffer)
  (kr:gv (or buffer
	     *current-buffer*)
	 :buffer-name))






)






