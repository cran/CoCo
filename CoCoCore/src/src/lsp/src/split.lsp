
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.



(defmeth coco-graph-window-proto :split-in-block-recursive
  (argument &key (list-file-name nil) (table-file-name nil) (return-graphs T))
  (if (and argument (send self :blocks))
      (let* ((vertices
	      (if (objectp argument) (list argument)
		(mapcar #'(lambda (vertex) (send self :vertex vertex))
			(if (listp argument) argument
			  (if (stringp argument)
			      (send self :string-to-vertices argument))))))
	     (stratum (min (mapcar #'(lambda (vertex) (send vertex :stratum))
				   vertices)))
	     (current-number (send self :return-model-number 'current))
	     (result
	      (if (send self :make-graph-current-model :redraw-plots nil)
		  (let ((x (send self :return-history-and-future stratum)))
		    (if x
			(let ((history (car x))
			      (current (cadr x))
			      (future (caddr x)))
			  (send self :split
				(mapcar #'(lambda (vertex)
					    (send vertex :name)) vertices)
				:use-name-list (concatenate 'list current
							    future)
				:list-file-name list-file-name
				:table-file-name table-file-name
				:return-graphs return-graphs)))))))
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
		      (send self :base)
		      (send self :read-model set)
		      (send self :meet-of-models T) ;; Silent ??
		      (send self :return-model 'last))
		  "*")))
    (send self :make-current current-number)
    (send self :make-base base-number)
    result)
  )

;;; 
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmeth coco-graph-window-proto :split
  (split-name-list &key (ignore-name-list nil) (use-name-list nil)
		   (list-file-name nil) (table-file-name nil)
		   (return-graphs T))
  (let* ((names (send self :return-name-list))
	 (levels (send self :return-level-list))
	 (missing-levels (send self :return-missing-list))
	 (indices (mapcar #'(lambda (name) (which-index names name))
			  split-name-list))
	 (a (if use-name-list
		(mapcar #'(lambda (name) (which-index names name))
			use-name-list)
	      (iseq (length names))))
	 (b (union indices
		   (if ignore-name-list
		       (mapcar #'(lambda (name) (which-index names name))
			       ignore-name-list))))
	 (read-names (to-string (select names
					(reverse (set-difference a b))) ""))
	 (select-names (to-string (select names indices) ""))
	 (select-slice (1+ (return-table-indices (select levels indices))))
	 (model (send self :return-meet-model select-names read-names))
	 (use-table (< (send self :return-marginal-dimension "*" :full T)
		       (* (round (send self :return-vector 'observed "."))
			  (length names))))
	 (data (if (not (or list-file-name table-file-name))
		   (if use-table
		       (round (send self :return-vector 'observed "*"))
		     ;; Uses (send self :return-vector 'observed "*"):
		     (element-seq (send self :return-case-list))))))
    (if indices
	(mapcar
	 #'(lambda (table z)
	     (let* ((title (concatenate 'string read-names ", " select-names
					": " (format nil "~d" table)))
		    (new-object
		     (make-coco :location (list (+ 700 (round (* z 100))) 500)
				:title title :manager nil :silent T)))
	       (send new-object :add-slot 'select-names  select-names)
	       (send new-object :add-slot 'select-values table)
	       (send new-object :enter-names names levels missing-levels)
	       (send new-object :set-read 'subset read-names)
	       (send new-object :select-cases select-names table :silent T)
	       (if list-file-name
		   (progn ;; Not tested:
		     (send new-object :set-observations-file list-file-name)
		     (send new-object :read-list :silent T))
		 (if table-file-name
		     (progn ;; Not tested:
		       (send new-object :set-observations-file list-file-name)
		       (send new-object :read-table :silent T))
		   (if use-table (send new-object :enter-table data :silent T)
		     (send new-object :enter-list data :silent T))))
	       (let ((graph
		      (send new-object :make-graph :model model :title title
			    :location (+ (send self :location) (list -5 -59)
					 (list (round (* z 500)) 0)))))
		 (send graph :add-slot 'coco-object new-object)
		 (send self :add-manager-edge self graph :type 'sibling)
		 ;; Alternative to sharing vertices:
		 (mapcar
		  #'(lambda (vertex)
		      (send vertex :position
			    (send self :vertex-position (send vertex :name))))
		  (send graph :vertices))
		 (if return-graphs graph new-object))))
	 select-slice (rseq 0 1 (length select-slice)))))
  )

(defmeth coco-graph-window-proto :select-names ()
  (send (slot-value 'coco-object) :select-names)
  )

(defmeth coco-graph-window-proto :select-values ()
  (send (slot-value 'coco-object) :select-values)
  )

(defmeth coco-proto :select-names ()
  (slot-value 'select-names)
  )

(defmeth coco-proto :select-values ()
  (slot-value 'select-values)
  )
