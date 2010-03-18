
(defun which-index (x i)
  (car (which (mapcar #'(lambda (j k)
			  (if (equalp i j) k)) x (iseq (length x))))))

(defmeth coco-graph-window-proto :split
  (split-name-list &key (ignore-name-list nil) (use-name-list nil)
		   (list-file-name nil) (table-file-name nil)
		   (return-graphs T))
  (let* ((names (send self :return-name-list))
	 (levels (send self :return-level-list))
	 (missing-levels (send self :return-missing-list))

	 (indices
	  (mapcar #'(lambda (name) (which-index names name)) split-name-list))
	 (IGNORE-INDICES (if ignore-name-list
			     (mapcar #'(lambda (name)
					 (which-index names name))
				     ignore-name-list)))
	 (USE-INDICES (if use-name-list
		       (mapcar #'(lambda (name)
				   (which-index names name))
			       use-name-list)))
	 (READ-INDICES (if use-name-list
			   (reverse (set-difference
				     use-indices indices))
			 (reverse (set-difference
				   (iseq (length names))
				   (union indices IGNORE-INDICES)))))
	 (select-names (to-string (select names indices) ""))
	 (read-names (to-string (select names READ-INDICES) ""))

	 (tables (1+ (return-table-indices (select levels indices))))

	 (model (send self :return-meet-model select-names read-names))

	 (use-table (< (full-marginal-dimension "*" :coco-id
						(slot-value 'identification))
		       (* (round (send self :return-vector 'observed "."))
			  (length names))))
	 (data (if use-table (round (send self :return-vector 'observed "*"))
		 ;; Uses (send self :return-vector 'observed "*"):
		 (element-seq (send self :return-case-list)))))
    (mapcar
     #'(lambda (table delta)
	 (let* ((title (concatenate 'string read-names ", " select-names ": "
				    (format nil "~d" table)))
		(new-object
		 (make-coco :location (list (+ 700 (round (* delta 100))) 500)
			    :title title :manager nil)))
	   (send new-object :enter-names names levels missing-levels)
	   (send new-object :set-read 'subset read-names)
	   (send new-object :select-cases select-names table)
	   (if list-file-name
	       (progn
		 ;; Not tested:
		 (send new-object :set-observations-file list-file-name)
		 (send new-object :read-list))
	     (if table-file-name
		 (progn
		   ;; Not tested:
		   (send new-object :set-observations-file list-file-name)
		   (send new-object :read-table))
	       (if use-table (send new-object :enter-table data)
		 (send new-object :enter-list data))))
	   (let ((graph
		  (send new-object :make-graph :model model :title title
			:location (+ (send self :location) (list -5 -59)
				     (list (round (* delta 500)) 0)))))
;;	     (send self :add-manager-edge self graph :type 'sibling)
	     ;; Alternative to sharing vertices:
	     (mapcar
	      #'(lambda (name)
		  (send graph :vertex-position (car name)
			(send self :vertex-position (car name)))
		  )
	      (send graph :names))
	     (if return-graphs graph new-object))))
     tables (rseq 0 1 (length tables)))
;;  (list indices ignore-indices read-indices select-names read-names)
    )
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth coco-graph-window-proto :split-in-block-recursive (argument)
  (if (and argument (send self :blocks))
      (let* ((stratum (if (objectp argument)
			  (send self :stratum argument)
			(if (listp argument)
			    (min (mapcar
				  #'(lambda (name) (send self :stratum name))
				  argument))
			  (if (stringp argument)
			      (min (mapcar
				    #'(lambda (name)
					(send self :stratum name))
				    (split-name-string argument)))))))
	     (current-number (send self :return-model-number 'current))
	     (result
	      (if (send self :make-graph-current-model :redraw-plots nil)
		  (let ((x (send self :return-history-and-future stratum)))
		    (if x (let ((history (car x))
				(current (cadr x))
				(future (caddr x)))
			    (send self :split
				  (if (objectp argument)
				      (list (send argument :name))
				    (if (listp argument)
					argument
				      (if (stringp argument)
					  (split-name-string argument))))
				  :use-name-list
				  (concatenate 'list current future)
				  )))))))
	(send self :make-current current-number)
	result))
  )

(defmeth coco-graph-window-proto :return-collaps-model (set)
  (let ((current-number (send self :return-model-number 'current))
	(result (if (send self :make-graph-current-model :redraw-plots nil)
		    (progn
		      (send self :collaps-model set)
		      (send self :print-model 'all)
		      (send self :return-model 'last))
		  "*")))
    (send self :make-current current-number)
    result)
  )

(defmeth coco-graph-window-proto :return-meet-model (select-set set)
  (let ((current-number (send self :return-model-number 'current))
	(base-number (send self :return-model-number 'base))
	(result (if (send self :make-graph-current-model :redraw-plots nil)
		    (progn
		      (send self :collaps-model select-set)
		      (send self :base)
		      (send self :read-model set)
		      (send self :meet-of-models)
		      (send self :print-model 'all)
		      (send self :return-model 'last))
		  "*")))
    (send self :make-current current-number)
    (send self :make-base base-number)
    result)
  )

; (send last-graph-object :split-in-block-recursive "AF")

;; Kaere Soeren,

;; Problemer med ``split'' og ``select-cases'' skyldes en lidt
;; for restriktiv kontrol af argumeter i select-funktionerne.
;; Load foelgende, hvor kontrol af parametre er disabled:

;; /jhb

;;   96   $ SELECT CASES <set> <marginal cell>;
(defun select-cases (set cell &optional &key (coco-id *current-coco*))
  ;; (format t "Consider using the argument function `select-case-fun' ~%")
  ;; (format t "at `enter-list' etc. ~%")
  ;; (format t "Interface-procedure not fully tested ~%")
  (if T ;; (= (length set) (length cell))
      (okcom (call-cfun "X__select_reject_cases"
			(string-int-0 (concatenate 'string set "."))
			cell (length cell) 1 *no-ifail* coco-id)))
  )

;;   97   $ OR SELECT CASES <set> <marginal cell>;
(defun or-select-cases (set cell &optional &key (coco-id *current-coco*))
  ;; (format t "Consider using the argument function `select-case-fun' ~%")
  ;; (format t "at `enter-list' etc. ~%")
  ;; (format t "Interface-procedure not fully tested ~%")
  (if T ;; (= (length set) (length cell))
      (okcom (call-cfun "X__select_reject_cases"
			(string-int-0 (concatenate 'string set "."))
			cell (length cell) 2 *no-ifail* coco-id)))
  )

;;   98   $ REJECT CASES <set> <marginal cell>;
(defun reject-cases (set cell &optional &key (coco-id *current-coco*))
  ;; (format t "Consider using the argument function `select-case-fun' ~%")
  ;; (format t "at `enter-list' etc. ~%")
  ;; (format t "Interface-procedure not fully tested ~%")
  (if T ;; (= (length set) (length cell))
      (okcom (call-cfun "X__select_reject_cases"
			(string-int-0 (concatenate 'string set "."))
			cell (length cell) 3 *no-ifail* coco-id)))
  )

;;   99   $ OR REJECT CASES <set> <marginal cell>;
(defun or-reject-cases (set cell &optional &key (coco-id *current-coco*))
  ;; (format t "Consider using the argument function `select-case-fun' ~%")
  ;; (format t "at `enter-list' etc. ~%")
  ;; (format t "Interface-procedure not fully tested ~%")
  (if T ;; (= (length set) (length cell))
      (okcom (call-cfun "X__select_reject_cases"
			(string-int-0 (concatenate 'string set "."))
			cell (length cell) 4 *no-ifail* coco-id)))
  )



