
;;; Copyright 1992, 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This is the functions for starting and ending CoCo within XLISP-STAT


(setf *coco-apix-version* "1.4")

;;; Variable *coco-type* is redefined in "cococg.lsp":

(setf *coco-type* "coco")

;;; (if (not (boundp '*coco-object-name*))
;;;     (setf *coco-object-name* "scoco.o"))

(defun coco-load (&optional (scoco *coco-object-name*)
			    &key (location (list 700 550)))
;;;   (if (not (boundp ' *cocolib*))
;;;       (setf *cocolib* "/user/jhb/CoCo/source"))
  (if (eq 0 (system (concatenate 'string "/usr/bin/test -r "
                     *cocolib* "/X" scoco)))
   (def scoco (concatenate 'string "X" scoco)))
  (let ((result (dyn-load (concatenate 'string *cocolib* "/" scoco)
			  :verbose T :fortran nil)))
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

(defun ok-coco-start (result)
  (let* ((ifail (car (car result))))
    (if (or (= ifail 0) (= ifail *apix-version*))
	T
      (if (and (< 50 ifail) (< ifail 60))
	  (ok-coco result)
	(format t  "Old version of CoCo object file ~%"))))
  )

;;; Function coco-start is updated in "cococg.lsp":

(defun coco-start (&optional (n 65536) (p 65536) (q 1024)
			     ;; New arguments:
                             ;; (r 65534) (s 65533) (ss 65532) (tt 65531) 
                             ;; (object *coco-object-name*)
			     )
  (if (not (boundp '*current-coco*)) (coco-load))
  (if (eql *current-coco* *fixed-coco*)
      (coco-resume)
    (if (if *coco-identifications*
	    (> (length (which (if-else (= (cdr *coco-identifications*)
					  *ended-coco*) nil T))) 1023))
	(format t "Too many CoCo-objects ~%")
      (let ((result (call-coco -3 nil
			       :arg-long (list n p q)
			       ;; :arg-long (list n p q r s ss tt)
			       :arg-double '(-1)
			       :coco-id *fixed-coco*)))
	(setf *current-coco* (car (cadr result)) )
	(setf *coco-identifications*
	      (combine *coco-identifications* *current-coco*) )
	(ok-coco-start result))))
  )

;;; Update of coco-start from "cocoapix.lsp":

(defun coco-start (&optional (n 65536) (p 65536) (q 1024)
			     ;; New arguments:
                             (r 65534) (s 65533) (ss 65532) (tt 65531) 
                             (object *coco-object-name*))
  (if (not (boundp '*current-coco*)) (coco-load object))
  (if (eql *current-coco* *fixed-coco*)
      (coco-resume)
    (if (if *coco-identifications*
	    (> (length (which (if-else (= (cdr *coco-identifications*)
					  *ended-coco*) nil T))) 1023))
	(format t "Too many CoCo-objects ~%")
      (let ((result (call-coco -3 nil
			       ;; :arg-long (list n p q)
			       :arg-long (list n p q r s ss tt)
                               :arg-double '(-1)
			       :coco-id *fixed-coco*)))
	(setf *current-coco* (car (cadr result)) )
	(setf *coco-identifications*
	      (combine *coco-identifications* *current-coco*) )
	(ok-coco-start result))))
  )

(defun get-lib-and-tmp ()
  nil
  )

;;; Function coco-init is updated in "cococg.lsp":

(defun coco-init (&optional (n 131071) (p 65536) (q 65536)
			    ;; New arguments:
                            ;; (r 65534) (s 65533) (ss 65532) (tt 65531) 
			    &key (title nil) (location (list 700 550))
			    (manager T))
  (if (not (boundp '*current-coco*)) (coco-load))
  (if (eql *current-coco* *fixed-coco*)
      (format t "CoCo already started ~%")
    (if (if *coco-identifications*
	    (> (length (which (if-else (= (cdr *coco-identifications*)
					  *ended-coco*) nil T))) 1023))
	;; Number of files pr. object: ~ 2 + (3 +) 4 + 2 = 8 approx.
	;; The integer constant FOPEN_MAX specifies the minimum  number
	;; of  files  that  the  implementation  guarantees can be open
	;; simultaneously.  `stdio.h': FOPEN_MAX = _NFILE = 20.
	(format t "Too many CoCo-objects ~%")
      (let ((result (call-coco -2 (length *coco-identifications*)
			       :arg-char (get-lib-and-tmp)
			       :arg-long (list n p q)
			       ;; :arg-long (list n p q r s ss tt)
			       :arg-double (list *my-not-a-number*)
			       ; #.POSITIVE-INFINITY
			       :coco-id *fixed-coco*)))
	(setf *current-coco* (car (cadr result)) )
	(setf *coco-identifications*
	      (combine *coco-identifications* *current-coco*) )
	(if (and (boundp 'manager-proto) manager)
	    (setf *current-manager*
		  (return-manager *current-coco* :title title
				  :hide-window nil :location location)))
	(if (ok-coco-start result) (list *current-coco* *current-manager*)))))
  )

;;; Update of coco-init from "cocoinit.lsp":

(defun coco-init (&optional (n 131071) (p 65536) (q  65535)
			    ;; New arguments:
                            (r 65534) (s 65533) (ss 65532) (tt 65531) 
			    &key (title nil) (location (list 700 550))
			    (manager T) (object *coco-object-name*))
  (if (not (boundp '*current-coco*)) (coco-load object))
  (if (eql *current-coco* *fixed-coco*)
      (format t "CoCo already started ~%")
    (if (if *coco-identifications*
	    (> (length (which (if-else (= (cdr *coco-identifications*)
					  *ended-coco*) nil T))) 1023))
	;; Number of files pr. object: ~ 2 + (3 +) 4 + 2 = 8 approx.
	;; The integer constant FOPEN_MAX specifies the minimum  number
	;; of  files  that  the  implementation  guarantees can be open
	;; simultaneously.  `stdio.h': FOPEN_MAX = _NFILE = 20.
	(format t "Too many CoCo-objects ~%")
      (let ((result (call-coco -2 (length *coco-identifications*)
			       :arg-char (get-lib-and-tmp)
			       ;; :arg-long (list n p q)
			       :arg-long (list n p q r s ss tt)
			       :arg-double (list *my-not-a-number*)
			       ; #.POSITIVE-INFINITY
			       :coco-id *fixed-coco*)))
	(setf *current-coco* (car (cadr result)) )
	(setf *coco-identifications*
	      (combine *coco-identifications* *current-coco*) )
	(if (and (boundp 'manager-proto) manager)
	    (setf *current-manager*
		  (return-manager *current-coco* :title title
				  :hide-window nil :location location)))
	(if (ok-coco-start result) (list *current-coco* *current-manager*)))))
  )

;;
(defun make-current-coco (nr)
  (setf *current-coco* (nth nr *coco-identifications*) )
  (if (= *ended-coco* *current-coco*)
      (format t "This CoCo is ended ~%") T)
  )

(defun coco-resume (&optional &key (coco-id *current-coco*))
;;; Modified in `cocograph.lsp'
  (if (= *ended-coco* *current-coco*)
      (format t "This CoCo is ended ~%")
    (ok-coco (call-coco -1 nil :coco-id coco-id)))
  )

(defun coco-end (&optional &key (coco-id *current-coco*))
;;; Modified in `cocograph.lsp'
  (if (/= *ended-coco* coco-id)
      (call-coco 0 (1- (length (which (if-else (= (cdr *coco-identifications*)
						  *ended-coco*) nil T))))
		 :coco-id coco-id) nil)
  (setf *coco-identifications*
	(combine nil (if-else
		      (= (cdr *coco-identifications*) coco-id)
		      *ended-coco* (cdr *coco-identifications*))))
  (if (= coco-id *current-coco*) (setf *current-coco* *ended-coco*))
  T
  )

(defun end-all-coco ()
  (dolist (i (cdr *coco-identifications*) NIL)
	  (if (/= *ended-coco* i)
	      (coco-end :coco-id i)))
  (setf *coco-identifications* NIL)
  (setf *current-coco* *ended-coco*)
  T
  )

(defun quit ()
  (end-all-coco)
  (exit)
  )

(provide "cocoinit")
