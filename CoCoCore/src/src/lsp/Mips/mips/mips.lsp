

;;; cocoinit.lsp

(if (not (boundp '*coco-object-name*))
    (setf *coco-object-name* "smips.o"))

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

;;; Update of coco-init from "cocoapix.lsp":

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

;;; cocofunc.lsp

(defun table-value (code)
 (let ((x (list 'counts 'observed 'probabilities 'expected 'unadjusted 'absolute
                'f-res 'r-f 'g-res 'r-g 'adjusted 'leverage 'c-res 'm-res
                'standardized 'standard 'x-res 'deviance '-2log 'l-res
                'freeman-tukey '2n-m 'sqrt 'power 'index 'zero 'error
                'canonical 'gs 'hs 'ks 'moment 'means 'covariance
                'raw 'total 'ss 'ssds 'sigma 'determinants 'mk 'ms))
       (y (list 0 0 1 2 3 3 4 5 6 7 8 16 8 9 9 9 9 10 10 10 11 12 12 13 14 15
                63 2 3 4 5 6 7 8 9 10 11 12 12 13 14 15)))
 (encode x code y code))
 )

(defun split-string (str)
  (let ((result nil)
	(block nil)
	(long-names nil)
        (name nil))
    (dolist (c (map-elements #'char str (iseq (length str))) result)
      (if (not (eql c #\Space))
	  (if (eql c #\:)
	      (progn
		(setf long-names T)
		(if name
		    (setf block
			  (concatenate 'list block
				       (list (concatenate 'string name)))))
		;; (print (list "block" block))
		(setf name (list #\:)))
	    (if (or (eql c #\<) (eql c #\\) (eql c #\>)
		    (eql c #\,) (eql c #\[) (eql c #\]))
		(progn
		  (if long-names
		      (if name
			  (progn
			    (setf block
				  (concatenate 'list block
					       (list
						(concatenate 'string name))))
			    (setf result
				  (concatenate 'list result (list block)))
			    (setf name nil)))
		    (setf result (concatenate 'list result (list block))))
		  ;; (print (list "result" result))
		  (setf block nil))
	      (if long-names
		  (setf name (concatenate 'list name (list c)))
                (setf block (concatenate 'list block (list c))))))))
;;    (concatenate 'list result (list block))
    (if long-names
	(if name
	    (concatenate 'list result
			 (list (concatenate 'list block
					    (list
					     (concatenate 'string name))))))
      (concatenate 'list result (list block)))
    )
  )

;;; cocometh.lsp

(defmeth coco-proto :status (&optional (hit 'all) (dump nil))
 (coco-simple-command (if dump 12 13)
                      (encode (list 'all 'formats 'tests 'exact 'fix 'ips
                                       'em 'old 'specification 'factors
                                       'observations 'data 'limits 'files
                                       'other 'search 'eh) hit
                          (list 1 2 3 4 5 6 7 18 8 8 9 9 9 10 11 12 12) hit)
  :coco-id (slot-value 'identification)))

(defmeth coco-proto :export (&rest file-name)
  (coco-set-char-options 14 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :import (&rest file-name)
  (coco-set-char-options 15 file-name nil
			 :coco-id (slot-value 'identification)))

(defmeth coco-proto :return-name-list (&key (full nil))
  (let ((names (split-string
		(cadr (split-string-chr
		       (send self :return-name-list-string :full full)
		       #\[ #\] nil)))))
    (car names))
  )

(defun make-coco
  (&key (n 65536) (p 65536) (q 1024) (r 65534) (s 65533) (ss 65532) (tt 65531) 
        (title nil) (location (list 700 550)) (manager T) (silent nil)
        (object *coco-object-name*))
  (if (boundp '*current-coco*)
      (if (= *current-coco* *fixed-coco*)
	  (format t "Only one CoCo-object with this object file ~%")
	(coco-init n p q r s ss tt :title title
                   :location location :manager manager :object object))
    (coco-init n p q r s ss tt :title title
               :location location :manager manager :object object))
  (send coco-proto :new *current-coco* :title title)
  )

(defun make-mips
  (&key (n 65536) (p 65536) (q 1024) (r 65534) (s 65533) (ss 65532) (tt 65531) 
        (title nil) (location (list 700 550)) (manager T) (silent nil)
        (object "smips.o"))
 (make-coco :n n :p p :q q :r r :s s :ss ss :tt tt :title title
            :location location :manager manager :object object)
 )

(defmeth coco-proto :enter-names (names levels
                                        &optional (missing-levels nil))
  (let ((names (split-name-string names)))
    (cond ((not (= (length names) (length levels)))
	   (format t "Wrong number of number of levels ~%"))
	  ((and missing-levels (not (= (length names)
				       (length missing-levels))))
	   (format t "Wrong number of factors to be marked ~%"))
	  (t (ok-coco
	      (call-coco 91 1
			 :arg-char (to-string names "")
			 :arg-long (concatenate 'list
						(list (length
						       (to-string names ""))
						      (length levels)
						      (length
                                                       missing-levels))
                                    (mapcar #'(lambda (i)
                                                (if (eq 'continuous i) 0 i))
                                     levels)
                                    missing-levels)
			 :coco-id (slot-value 'identification))))))
  )

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

(defmeth coco-proto :return-model
  (&optional (model 'current modelset) (number nil)
	     &key (type 'gc))
  (let ((old-current (send self :before-set-current model modelset number))
	(tp (encode (list 'gc 'cs 'both 'discrete 'linear 'hierarchical)
		    type (1+ (iseq 6)) 3))
	(m (encode (list 'base 'current 'last) model (1+ (iseq 3)) 2)))
    (let ((result
	   (coco-enter-string 127 'what (- (+ (* 10 tp) m))
			      :coco-id (slot-value 'identification))))
      (send self :after-set-current old-current result 'unconditioned nil)))
  )

(defmeth coco-proto :compute-deviance (&optional (model-1 nil model1set)
                                                 (model-2 nil model2set))
  (let ((old (send self :before-set-both model-1 model-2 model1set model2set)))
   (let ((result (call-coco 162 1
		    :arg-long   (repeat 0 9) ;; 8
		    :arg-double (repeat 0 7) ;; 5
                  :coco-id (slot-value 'identification))))
    (if (stringp model-2) (send self :dispose-of-model 'base))
    (if (and (cadr old) model-2) (send self :make-base (cadr old)))
    (send self :after-set-current (car old) result 'long-and-double model-1)))
 )

(defmeth coco-proto :compute-test (&optional (model-1 nil model1set)
                                             (model-2 nil model2set))
  (let ((old (send self :before-set-both
              ;; model-1 = nil for test of 'current in call from 
              ;; :return-test-object at model-object?
                   (if (and model-1 model1set) model-1 'current)
              ;; model-2 = nil for test against 'base in call from 
              ;; :return-test-object at model-object?
                   (if (and model-2 model2set) model-2 'base)
                   model1set model2set)))
   (let ((result (call-coco 160 1
                  :arg-long   (repeat 0  5) ;;  4
                  :arg-double (repeat 0 13) ;; 13
                  :coco-id (slot-value 'identification))))
    (if (stringp model-2) (send self :dispose-of-model 'base))
    (if (and (cadr old) model-2) (send self :make-base (cadr old)))
    (send self :after-set-current (car old) result 'long-and-double model-1)))
 )

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





