
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for describing values:


;;; :plot and :return-matrix ???

(defmeth coco-proto :new-return-vector (arguments set)
  (send self :return-vector (car arguments) set
	:model (cadr arguments)
	:complete (car (caddr arguments))
	:random (cadr (caddr arguments))
	:log-transformed (caddr (caddr arguments))
	:permuted (cadddr (caddr arguments)))
  )

(defun encode-return-vector (arguments set)
  `(send *this-graph* :return-vector (quote ,(car arguments)) ,set
	 :model ,(if (numberp (cadr arguments))
		     (cadr arguments)
		   (backquote (quote ,(cadr arguments))))
	 :complete ,(car (caddr arguments))
	 :random ,(cadr (caddr arguments))
	 :log-transformed ,(caddr (caddr arguments))
	 :permuted ,(cadddr (caddr arguments)))
  )

(defun encode-return-vector (arguments set)
  (eval
   `#'(lambda (graph)
	(send graph :return-vector (quote ,(car arguments)) ,set
	      :model ,(if (numberp (cadr arguments))
			  (cadr arguments)
			(if (not (cadr arguments))
			    nil
			  (backquote (quote ,(cadr arguments)))))
	      :complete ,(car (caddr arguments))
	      :random ,(cadr (caddr arguments))
	      :log-transformed ,(caddr (caddr arguments))
	      :permuted ,(cadddr (caddr arguments)))))
  )

(defun encode-argument (arguments)
  (concatenate 'string (string (car arguments))
	       (format nil ": ~a" (cadr arguments))
	       (if (car (caddr arguments)) ", Complete")
	       (if (cadr (caddr arguments)) ", Random")
	       (if (caddr (caddr arguments)) ", Log-transformed"))
  )

(defun values-dialog (x)
  (let* ((dismiss-values
	  (send modal-button-proto :new (modal-text "Dismiss values window")
		:action #'(lambda () (send values-dialog-window :remove))))

	 (argument-label
	  (send text-item-proto :new
		"Vertex-set:"))

	 (argument-set-item
	  (send edit-text-item-proto :new 
		(to-string (send x :return-name-list) "")
		:text-length 10
		))

	 (all-values-item
	  (send toggle-item-proto :new
		(modal-text "Present all possible values")
		:value nil))

	 (list-item
	  (send modal-button-proto :new
		(modal-text "List values <set> ")
		:action #'(lambda () (send x :list-values
					   (send argument-set-item :text)))))

	 (case-list-item
	  (send modal-button-proto :new
		(modal-text "Case list <set> ")
		:action #'(lambda () (send x :case-list
					   (send argument-set-item :text)))))

	 (print-table-item
	  (send modal-button-proto :new
		(modal-text "Print table <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value))))
		      (if arguments
			  (send x :print-table
				(car arguments)
				(send argument-set-item :text)
				:model (cadr arguments)
				:complete (car (caddr arguments))
				:random (cadr (caddr arguments))
				:log-transformed (caddr (caddr arguments))
				:permuted (cadddr (caddr arguments))
				))))))


	 (describe-table-item
	  (send modal-button-proto :new
		(modal-text "Describe table <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value))))
		      (if arguments
			  (send x :describe-table
				(car arguments)
				(send argument-set-item :text)
				:model (cadr arguments)
				:complete (car (caddr arguments))
				:random (cadr (caddr arguments))
				:log-transformed (caddr (caddr arguments))
				:permuted (cadddr (caddr arguments))
				))))))

	 (return-vector-item
	  (send modal-button-proto :new
		(modal-text "Bind and Return-vector <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value))))
		      (if arguments
			  (setf *vector*
				(send x :new-return-vector arguments
				      (send argument-set-item :text))))))))

	 (histogram-item
	  (send modal-button-proto :new
		(modal-text "Histogram <value> <set> ...")
		:action
		#'(lambda ()
		    (let ((arguments (one-value-dialog
				      (send x :slot-value 'model-number)
				      "Table-value: "
				      (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if arguments
			  (setf *histogram*
				(histogram
				 (send x :new-return-vector arguments factors)
				 :title
				 (format nil "~a: ~a" factors (cadr arguments))
				 :variable-labels
				 (list (encode-argument arguments)))))))))


	 (plot-item
	  (send modal-button-proto :new
		(modal-text "X-Y-plot of two cell-values ... ... ")
		:action
		#'(lambda ()
		    (let ((X-args (one-value-dialog
				   (send x :slot-value 'model-number) "X"
				   (send all-values-item :value)))
			  (Y-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if (and X-args Y-args)
			  (setf *plot*
				(plot-points
				 (send x :new-return-vector X-args factors)
				 (send x :new-return-vector Y-args factors)
				 :title
				 (format nil "~a: ~a, ~a"
					 factors (cadr X-args) (cadr Y-args))
				 :variable-labels
				 (list (encode-argument X-args)
				       (encode-argument Y-args)))))))))
	
	 (spin-item
	  (send modal-button-proto :new
		(modal-text "Spin-plot of three cell-values .. .. .. ")
		:action
		#'(lambda ()
		    (let ((X-args (one-value-dialog
				   (send x :slot-value 'model-number) "X"
				   (send all-values-item :value)))
			  (Y-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (Z-args (one-value-dialog
				   (send x :slot-value 'model-number) "Z"
				   (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if (and X-args Y-args Z-args)
			  (setf *spin-plot*
				(spin-plot
				 (list
				  (send x :new-return-vector X-args factors)
				  (send x :new-return-vector Y-args factors)
				  (send x :new-return-vector Z-args factors))
				 :title
				 (format nil "~a: ~a, ~a, ~a"
					 factors (cadr X-args)
					 (cadr Y-args) (cadr Z-args))
				 :variable-labels
				 (list (encode-argument X-args)
				       (encode-argument Y-args)
				       (encode-argument Z-args)))))))))

	 (dynamic-spin-item
	  (send modal-button-proto :new
		(modal-text "Dynamic Spin-plot ... ... ...")
		:action
		#'(lambda ()
		    (let ((X-args (one-value-dialog
				   (send x :slot-value 'model-number) "X"
				   (send all-values-item :value)))
			  (Y-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (Z-args (one-value-dialog
				   (send x :slot-value 'model-number) "Y"
				   (send all-values-item :value)))
			  (factors (send argument-set-item :text)))
		      (if (and X-args Y-args Z-args)
			  (setf *dynamic-spin-plot*
				(send x :return-dynamic-coco-spin-plot
				      (list
				       (encode-return-vector X-args factors)
				       (encode-return-vector Y-args factors)
				       (encode-return-vector Z-args factors))
				      :title
				      (format nil "~a: ~a, ~a, ~a"
					      factors (cadr X-args)
					      (cadr Y-args) (cadr Z-args))
				      :variable-labels
				      (list (encode-argument X-args)
					    (encode-argument Y-args)
					    (encode-argument Z-args)))))))))

	 (old-dynamic-spin-item
	  (send modal-button-proto :new
		(modal-text "Dynamic Spin-plot .?. .?. .?.")
		:action #'(lambda ()
			    (setf *this-graph* x)
			    (let ((a (get-value-dialog
				      "X-value" :initial
				      ''(send *this-graph*
					      :return-vector 'adjusted "*"
					      :model 'current)))
				  (b (get-value-dialog
				      "Y-value" :initial
				      ''(send *this-graph*
					      :return-vector 'adjusted "*"
					      :model 'base)))
				  (c (get-value-dialog
				      "Z-value" :initial
				      ''(send *this-graph*
					      :return-vector 'observed "*"
					      :model nil))))
			      (setf *dynamic-spin-plot*
				    (send x :return-dynamic-coco-spin-plot
					  (list (car a) (car b) (car c))))))))
	
	 (values-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-values)
		      all-values-item
		      (list (list argument-label) (list argument-set-item))
		      list-item case-list-item
		      print-table-item describe-table-item return-vector-item
		      histogram-item plot-item spin-item dynamic-spin-item)))))
  )
