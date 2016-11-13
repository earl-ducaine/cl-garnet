(in-package :ccl)

(defvar *allow-gcable-macptrs?* t)

(defvar *all-regions* nil)

(defun make-region (&optional (gcable t))
  (let ((it (if gcable (%new-rgn) (#_newrgn))))
    ;(push (list gcable (my-caller) it) *all-regions*)
    it))

;;; *** RGA is redundant.
#-ccl-3
(defun dispose-region (region)
  ;(setq *all-regions* (delete region *all-regions* :test 'eql :key 'cdr))
  (let* ((flags (macptr-flags region))
         (gcflags (logand flags #x3))) ;; look at lower 2 bits only - this may change
    (cond ((= gcflags $flags_normal)
           (#_disposergn region))
          ((= gcflags $flags_disposhandle)
           (without-interrupts
            (set-macptr-flags region (logior (logandc2 flags #x3) $flags_normal))
            (#_disposergn region)))
          (t
           (error "Assumption in writing dispose-region is violated - Flags for
macptr is not $flags_normal or $flags_disposhandle. This means that dispose-region
needs to be rewritten. -alan")))))

(defun gworld-macptr (gworld &optional (gcable t))
  (let ((macptr (if (and gcable *allow-gcable-macptrs?*)
                  (make-gcable-macptr $flags_Disposgworld)
                  (%null-ptr))))
    (%setf-macptr macptr gworld)
    macptr
    ))

(defun dispose-gworld (gworld)
  (cond ((%null-ptr-p gworld)
         (error "null pointer!"))
        ((= (macptr-flags gworld) $flags_disposgworld)
         (set-macptr-flags gworld $flags_normal)
         (#_disposegworld gworld)
         (%setf-macptr gworld (%null-ptr)))
        ((= (macptr-flags gworld) $flags_normal)
         (#_disposegworld gworld)
         (%setf-macptr gworld (%null-ptr)))
        (t (error "flags should be $flags_normal or $flags_disposgworld"))))

(defun gcable-pointer (pointer)
  (let ((it (if *allow-gcable-macptrs?*
              (make-gcable-macptr $flags_disposptr)
              (%null-ptr))))
    (%setf-macptr it pointer)
    it))
         