
(defun coco-load (&optional (scoco *coco-object-name*)
			    &key (location (list 700 550)))
;;;   (if (not (boundp ' *cocolib*))
;;;       (setf *cocolib* "/user/jhb/CoCo/source"))
  (if (eq 0 (system (concatenate 'string "/usr/bin/test -r "
                     *cocolib* "/X" scoco)))
   (def scoco (concatenate 'string "X" scoco)))
  (setf *dll-object* (concatenate 'string *cocolib* "/" scoco))
  (setf *dll-lib* (shlib::shlib-open *dll-object*))
  (let ((result *dll-lib*))
    (setf *apix-version* 140)
    (setf *coco-errors* nil)
    (setf *fixed-coco* 2147483646)
    (setf *ended-coco* 2147483647)
    (setf *no-ifail* 0)
    (setf *no-ifail-return* -1)
    (setf *coco-identifications* NIL)
    (setf *current-coco* *ended-coco*)
    (if (boundp 'manager-proto)
	(setf *manager*
	      (return-manager nil :title "All CoCo objects"
			      :hide-window T :location location)))
    result)
  )

(defun call-coco (code &optional (sub-code 0) &key (coco-id *current-coco*)
		       (arg-char NIL) (arg-long NIL) (arg-double NIL))
  (let* ((arg-long (if (listp arg-long) arg-long 
		     (if arg-long (list arg-long))))
	 (arg-double (if (listp arg-double) arg-double
		       (if arg-double (list arg-double))))
	 (arg-char-int (concatenate 'list
				    (if arg-char
					(string-int
					 (if (listp arg-char)
					     (car arg-char) arg-char))) '(0)))
	 (n-arg (mapcar #'length (list nil arg-long arg-double arg-char-int))))
    (shlib::old-call-cfun *coco-entry-name* *dll-lib* *no-ifail*
			  coco-id code sub-code n-arg 0 arg-long 
			  (if arg-double (float arg-double)) arg-char-int))
  )
