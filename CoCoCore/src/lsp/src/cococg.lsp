
;;; Copyright 1995, 2003 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; New methods for "CoCoCg": 


;;; To "cocoinit.lsp"

;;; (if (not (boundp '*coco-object-name*))
;;;    (setf *coco-object-name* "smips.o"))


;;; Redefinition of *coco-type* of "cocoinit.lsp":

(setf *coco-type* "cococg")


;;; To "cocometh.lsp":


;;; New method for CoCoCg:

(defmeth coco-proto :export (&rest file-name)
  (coco-set-char-options 14 file-name nil
			 :coco-id (slot-value 'identification)))

;;; New method for CoCoCg:

(defmeth coco-proto :import (&rest file-name)
  (coco-set-char-options 15 file-name nil
			 :coco-id (slot-value 'identification)))

;;; New method:

(defmeth coco-proto :enter-discrete-list
  (list &key (accumulated nil) (ncol nil) (select-case-fun nil set)
	(columns nil subset) (silent nil))
  ;; (format t "Interface-procedure not fully tested ~%")
  (if (and (= 0 (mod (length list)
		     (if columns ncol
		       (+ (length (send self :return-name-list :full T))
			  (if accumulated 1 0)))))
	   (if columns
	       (= (length columns)
		  (+ (length (send self :return-name-list :full T))
		     (if accumulated 1 0))) T))
      (let* ((a (if select-case-fun
		    (list-select-cases
		     list
		     (if ncol ncol
		       (+ (length (send self :return-name-list :full T))
			  (if accumulated 1 0)))
		     select-case-fun) list))
	     (b (if columns (select-columns a ncol columns) a)))
	(coco-set-long-options 105 b (length b) (if accumulated 2 1)
			       :coco-id (slot-value 'identification)))
    (format T "List of wrong length"))
  )

;;; New method:

(defmeth coco-proto :enter-real-list
  (list &key (accumulated nil) (ncol nil) (select-case-fun nil set)
	(columns nil subset) (silent nil))
  ;; (format t "Interface-procedure not fully tested ~%")
  (if (and (= 0 (mod (length list)
		     (if columns ncol
		       (+ (length (send self :return-name-list :full T))
			  (if accumulated 1 0)))))
	   (if columns
	       (= (length columns)
		  (+ (length (send self :return-name-list :full T))
		     (if accumulated 1 0))) T))
      (let* ((a (if select-case-fun
		    (list-select-cases
		     list
		     (if ncol ncol
		       (+ (length (send self :return-name-list :full T))
			  (if accumulated 1 0)))
		     select-case-fun) list))
	     (b (if columns (select-columns a ncol columns) a)))
	(coco-set-real-options 105 b (length b) (if accumulated 2 1)
			       :coco-id (slot-value 'identification)))
    (format T "List of wrong length"))
  )

;;; New method:

(defmeth coco-proto :enter-double-list
  (list &key (accumulated nil) (silent nil))
  ;; (format t "Interface-procedure not fully tested ~%")
  (let ((factor-list (car list))
        (continuous-list (cadr list)))
   (if (= 0 (mod (+ (length factor-list) (length continuous-list))
	       (+ (length (send self :return-name-list :full T))
                (if accumulated 1 0))))
    (coco-enter-all 105 nil factor-list continuous-list (if accumulated 2 1)
     :coco-id (slot-value 'identification))
    (format T "List of wrong length")))
 )

;;; New method:

(defmeth coco-proto :enter-list
  (list &key (accumulated nil) (ncol nil) (select-case-fun nil set)
	(columns nil subset) (silent nil))
  ;; (format t "Interface-procedure not fully tested ~%")
 (if (= 0 (length (which (= (send self :return-level-list :full nil) 0))))
  (send self :enter-discrete-list list :accumulated accumulated :ncol ncol
   :select-case-fun select-case-fun :columns columns :silent silent)
  (if (> 1 (length (car list)))
   (send self :enter-double-list list :accumulated accumulated :ncol ncol
    :select-case-fun select-case-fun :columns columns :silent silent)
   (send self :enter-real-list list :accumulated accumulated :ncol ncol
    :select-case-fun select-case-fun :columns columns :silent silent)))
  )

;;; New method:

(defmeth coco-proto :enter-list
  (list &rest keyword-pairs &allow-other-keys)
  ;; (format t "Interface-procedure not fully tested ~%")
 (if (= 0 (length (which (= (send self :return-level-list :full nil) 0))))
  (apply #'send self :enter-discrete-list list
         :allow-other-keys t keyword-pairs)
  (if (listp (car list))
   (apply #'send self :enter-double-list list
          :allow-other-keys t keyword-pairs)
   (apply #'send self :enter-real-list list
          :allow-other-keys t keyword-pairs)))
  )

;;; New method:

(defmeth coco-proto :print-continuous (type &optional (set "*")
				       &key (model 'current modelset)
				       (random nil) (log-transformed nil)
				       (complete nil) (permuted T)
				       (table nil) (matrix nil)
                                       )
  (let ((old-current (send self :before-set-current model modelset))
	(result
         (call-coco (if permuted 123 121) -1
          :arg-char set
          :arg-long
          (encode-type-and-options type random log-transformed complete
                                   permuted
           :uniform nil :rankit table :probit matrix)
          :coco-id (slot-value 'identification))))
   (send self :after-set-current old-current result 'ok nil)))

;;;  Segmentation Fault at (send rats :return-vector 'observed "ab")

;;; New method:

(defmeth coco-proto :return-continuous (type &optional (set "*")
				        &key (model 'current modelset)
				        (random nil) (log-transformed nil)
				        (complete nil) (permuted T)
				        (dump nil))
  (let* ((old-current (send self :before-set-current model modelset))
	 (a (send self :return-level-list :full nil))
         (n (prod (select a (which (> a 0)))))
         (n (length (which (> a 0))))
         (m (+ 1 n (/ (* n (1+ n)) 2)))
         (d (send self :return-marginal-dimension set))
	 (result
	    (call-coco (if permuted 124 122) (if dump 0 -1)
		       :arg-char set
		       :arg-long
                       (encode-type-and-options type random log-transformed
                                                complete permuted
                                                :uniform nil :rankit nil
                                                :probit nil)
		       :arg-double (iseq 1 (* d m))
		       :coco-id (slot-value 'identification))))
    (send self :after-set-current old-current result 'double nil)))

;;; Fixes:

;; Update of same in "cocoapix.lsp":

;; (defun call-coco (code &optional (sub-code 0) &key (coco-id *current-coco*)
;; 		       (arg-char NIL) (arg-long NIL) (arg-double NIL))
;;   (let* ((arg-long (if (listp arg-long) arg-long 
;; 		     (if arg-long (list arg-long))))
;; 	 (arg-double (if (listp arg-double) arg-double
;; 		       (if arg-double (list arg-double))))
;; 	 (arg-char-int (concatenate 'list
;; 				    (if arg-char
;; 					(string-int
;; 					 (if (listp arg-char)
;; 					     (car arg-char) arg-char))) '(0)))
;; 	 (n-arg (mapcar #'length (list nil arg-long arg-double arg-char-int))))
;;     (call-cfun *coco-entry-name* *no-ifail* coco-id code sub-code n-arg
;; 	       0 arg-long (if arg-double (float arg-double)) arg-char-int))
;;   )
