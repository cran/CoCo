
;; Directory of CoCo ('COCOHOME'):
;; (setf *cocohome* "/usr/lib/R/library/CoCo")

;; Directory of object-file (now 'COCOLIB'):
(setf *cocolib* "/usr/lib/R/library/CoCo/lib/mips")

;; Directory of lsp-files (now 'XCOCOLIB'):
(setf *xlisp-cocolib* "/usr/lib/R/library/CoCo/lsp")

;; Name of object-file, with extension:
(setf *coco-object-name* "libxcoco.so")

(expand 5)

(load (concatenate 'string *xlisp-cocolib* "/mipsfiles.lsp"))

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
    (call-cfun "Mips" *no-ifail* coco-id code sub-code n-arg
	       0 arg-long (if arg-double (float arg-double)) arg-char-int))
  )

(load (concatenate 'string *xlisp-cocolib* "/tail-unix.lsp"))

