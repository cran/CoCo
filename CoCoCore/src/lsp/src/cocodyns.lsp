
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This defines the dynamic-coco-spin-proto
;;; and adds the basic methods to this proto


(defproto dynamic-coco-spin-proto
  '(expressions) '(instances-dynamic-coco-spin)
  (list spin-proto coco-graph-window-proto))

(send dynamic-coco-spin-proto :documentation
      'proto "CoCo Spin based on spin proto type")

(defmeth dynamic-coco-spin-proto :isnew (dim &key location title
					     variable-labels)
  (let ((plot (apply #'call-method spin-proto :isnew dim
		     :location location :title title
		     :variable-labels (list variable-labels))))
    (send dynamic-coco-spin-proto :slot-value 'instances-dynamic-coco-spin
	  (cons self (slot-value 'instances-dynamic-coco-spin)))
    plot)
  )

(defmeth coco-graph-window-proto :return-dynamic-coco-spin-plot
  (expressions &optional &key (location nil) (title nil) (variable-labels nil))
  (let ((plot (send dynamic-coco-spin-proto :new (length expressions)
		    :location location :title title
		    :variable-labels variable-labels)))
    (send plot :slot-value 'identification (slot-value 'identification))
    (send plot :slot-value 'model-number (slot-value 'model-number))
    (send plot :slot-value 'expressions expressions)
    (send plot :change-models T)
    (send plot :adjust-to-data)
    plot)
  )

(def *all-points* nil)

(defun exclude-missing (points)
  (mapcar
   #'(lambda (x)
       (select x (which 
		  (mapcar #'(lambda (z) (eq (length z) (length (which z))))
			  (transpose
			   (mapcar
			    #'(lambda (y) (mapcar #'not (invalid-real-list y)))
			    points))))))
   points))

(defmeth dynamic-coco-spin-proto :change-models (&optional (init nil))
  (let ((points (send self :values)))
    (if (and (send self :linked) (not init))
	(if (not (eql (send self :num-points) (length (car points))))
	    (progn
	      (format t "Invalid values: ~%")
	      (format t "Vectors not of same length in :change-models.~%")
	      (format t "Clearing points (unhighlighting).~%")
	      (send self :clear-points)
	      (send self :add-points points))
	  (mapcar #'(lambda (i x)
		      (send self :point-coordinate i (iseq (length x)) x))
		  ;; Problems with :point-coordinate after 
		  ;; ``3.40282346638528859812E38''.
		  (iseq (length points)) points))
      (progn
	(send self :clear-points)
	(send self :add-points
	      (if *all-points* points (exclude-missing points))))))
  (send self :adjust-to-data)
  )

(defmeth dynamic-coco-spin-proto :change-models (&optional (init nil))
  (let ((points (send self :values)))
    (if (and nil (and (send self :linked) (not init)))
	(if (or T (not (eql (send self :num-points) (length (car points)))))
	    (progn
	      (format t "Invalid values: ~%")
	      (format t "Vectors not of same length in :change-models.~%")
	      (format t "Clearing points (unhighlighting).~%")
	      (send self :clear-points)
	      (send self :add-points points))
	  (mapcar #'(lambda (i x)
		      (send self :point-coordinate i (iseq (length x)) x))
		  ;; Problems with :point-coordinate after 
		  ;; ``3.40282346638528859812E38''.
		  (iseq (length points)) points))
      (progn
	(send self :clear-points)
	(send self :add-points
	      (if *all-points* points (exclude-missing points))))))
  (send self :adjust-to-data)
  )

(defmeth dynamic-coco-spin-proto :expressions (&optional (val nil set))
  (if set (progn (setf (slot-value 'a) val)
		 (send self :variable-labels nil)
		 (send self :change-models)))
  (slot-value 'expressions)
  )

(defmeth dynamic-coco-spin-proto :nth-expression (n &optional (val nil set))
  (if set (progn (setf (nth n (slot-value 'a)) val)
		 (send self :change-models)))
  (nth n (slot-value 'expressions))
  )

					;

(defmeth dynamic-coco-spin-proto :values ()
  (if (functionp (car (slot-value 'expressions)))
      (mapcar #'(lambda (expression) (funcall expression self))
	      (slot-value 'expressions))
    (progn
      (setf *this-graph* self)
      (mapcar #'eval (slot-value 'expressions))))
  )

(defmeth dynamic-coco-spin-proto :nth-value (n)
  (if (functionp (nth n (slot-value 'expressions)))
      (funcall (nth n (slot-value 'expressions)) self)
    (progn
      (setf *this-graph* self)
      (eval (nth n (slot-value 'expressions)))))
  )

(defmeth dynamic-coco-spin-proto :close ()
  (send dynamic-coco-spin-proto :slot-value 'instances-dynamic-coco-spin
	(remove self (send dynamic-coco-spin-proto :slot-value
			   'instances-dynamic-coco-spin)))
  (call-next-method)
  )

(defmeth coco-graph-window-proto :make-graph-current-model
  (&key (redraw-plots nil))
  (let ((c (send self :make-current (slot-value 'model-number))))
    (if (and c redraw-plots)
	(progn
	  (dolist (i (send current-control-proto :slot-value
			   'instances-current-control))
		  (send i :redraw))
	  (dolist (i (send dynamic-coco-spin-proto :slot-value
			   'instances-dynamic-coco-spin))
		  (if (= (send i :slot-value 'identification)
			 (slot-value 'identification))
		      (send i :change-models)))))
    c)
  )

(defmeth coco-graph-window-proto :make-graph-base-model
  (&key (redraw-plots nil))
  (let ((c (send self :make-base (slot-value 'model-number))))
    (if (and c redraw-plots)
	(progn
	  (dolist (i (send base-control-proto :slot-value
			   'instances-base-control))
		  (send i :redraw))
	  (dolist (i (send dynamic-coco-spin-proto :slot-value
			   'instances-dynamic-coco-spin))
		  (if (= (send i :slot-value 'identification)
			 (slot-value 'identification))
		      (send i :change-models)))))
    c)
  )

;;

(provide "cocodyns")
