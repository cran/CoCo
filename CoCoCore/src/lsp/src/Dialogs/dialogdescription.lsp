
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for describing models:


;;; :is-in-one-clique, :is-submodel-of?


(def *model-names*
     (list "'current" "'base" "'last" "'all" "'graph" "'number:"))

(def *model-list*
     (list 'current 'base 'last 'all 'graph 'number))

(defun model-description-dialog (x)
  (let* ((dismiss-model-description
	  (send modal-button-proto :new
		(modal-text "Dismiss model dialog")
		:action #'(lambda ()
			    (send model-description-dialog-window :remove))))

	 (current-item ;;; Marker ???
	  (send modal-button-proto :new
		(modal-text "`c': Make model of graph ``Current''")
		:action #'(lambda () (send x :make-graph-current-model
					   :redraw-plots T))))
	
	 (base-item ;;; Marker ???
	  (send modal-button-proto :new
		(modal-text "`b': Make model of graph ``Base''")
		:action #'(lambda () (send x :make-graph-base-model
					   :redraw-plots T))))

	 (model-item (send choice-item-proto :new *model-names* :value 0))

	 (argument-item
	  (send edit-text-item-proto :new
		(format nil "~d"
			(send x :return-model-number 'current))
		:text-length 3))

	 (is-decomposable-item
	  (send modal-button-proto :new
		(modal-text "Is decomposable ? <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (print 
			       (if (equal 'graph model)
				   (if (send x :make-graph-current-model
					     :redraw-plots nil)
				       (send x :is-decomposable))
				 (send x :is-decomposable
				       (if (equal 'number model)
					   (send argument-item :value)
					 model))))
			      (send x :make-current current-number)))))

	 (is-graphical-item
	  (send modal-button-proto :new
		(modal-text "Is graphical ? <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (print 
			       (if (equal 'graph model)
				   (if (send x :make-graph-current-model
					     :redraw-plots nil)
				       (send x :is-graphical))
				 (send x :is-graphical
				       (if (equal 'number model)
					   (send argument-item :value)
					 model))))
			      (send x :make-current current-number)))))

	 (print-formula-item
	  (send modal-button-proto :new
		(modal-text "Print formula <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :print-formula))
				(progn
				  (send x :make-current
				        (if (equal 'number model)
					    (send argument-item :value)
					  model))
				  (send x :print-formula)))
			      (send x :make-current current-number)))))

	 (print-vertex-order-item
	  (send modal-button-proto :new
		(modal-text "Print vertex order <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :print-vertex-order))
				(progn
				  (send x :make-current
				        (if (equal 'number model)
					    (send argument-item :value)
					  model))
				  (send x :print-vertex-order)))
			      (send x :make-current current-number)))))
	
	 (print-item
	  (send modal-button-proto :new
		(modal-text "Print model <argument>   ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :print-model 'current))
				(send x :print-model
				      (if (equal 'number model)
					  (send argument-item :value)
					model)))
			      (send x :make-current current-number)))))

	 (describe-item
	  (send modal-button-proto :new
		(modal-text "Describe model <argument>  ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :describe-model 'current))
				(send x :describe-model
				      (if (equal 'number model)
					  (send argument-item :value)
					model)))
			      (send x :make-current current-number)))))

	 (dispose-of-item
	  (send modal-button-proto :new
		(modal-text "Dispose of model <argument>  ")
		:action #'(lambda ()
			    (let ((model (nth (send model-item :value)
					      *model-list*))
				  (current-number
				   (send x :return-model-number 'current)))
			      (if (equal 'graph model)
				  (if (send x :make-graph-current-model
					    :redraw-plots nil)
				      (send x :dispose-of-model 'current))
				(send x :dispose-of-model
				      (if (equal 'number model)
					  (send argument-item :value)
					model)))
			      (send x :make-current current-number)))))


	 (model-description-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-model-description)
		 current-item base-item
		 (list
		  (list
		   print-formula-item print-vertex-order-item
		   print-item describe-item dispose-of-item
		   is-decomposable-item is-graphical-item)
		  (list model-item argument-item))
		 ))))

    (defmeth argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )
    )
  
  )
