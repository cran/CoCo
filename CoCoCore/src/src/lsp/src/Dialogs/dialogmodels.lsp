
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the editing model:


(defun model-dialog (x)
  (let* ((dismiss-model
	  (send modal-button-proto :new (modal-text "Dismiss model dialog")
		:action #'(lambda () (send model-dialog-window :remove))))
	
	 (number-label (send text-item-proto :new "Number/Order:"))

	 (number-item
	  (send edit-text-item-proto :new
		(format nil "~d" 2) :text-length 3))
	
	 (argument-label
	  (send text-item-proto :new
		"Argument, GC (or 'graph, 'current, 'base, 'last, number or set):"))

	 (argument-item
	  (let ((current-number (send x :return-model-number 'current))
		(result (send edit-text-item-proto :new (send x :return-model)
			      :text-length 66)))
	    (send x :make-current current-number)
	    result))
	
	 (create-graph-item
	  (send toggle-item-proto :new
		(modal-text "Make graph of the created model")
		:value T))

	 (copy-vertices-item
	  (send toggle-item-proto :new
		(modal-text "Share vertices, (i.e. positions)")
		:value T))

	 (copy-graph-item
	  (send modal-button-proto :new (modal-text "Copy graph")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current)))
		      (progn
			(send x :make-graph-current-model)
			(send x :return-child-coco-graph-window
			      :copy-vertices
			      (send copy-vertices-item :value)
			      :title (send argument-item :gc x)
			      :sibling T))
		      (send x :make-current current-number)))))

	 (graph-number-item
	  (send modal-button-proto :new
		(modal-text "Make graph for model number <number>")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (number (send number-item :value)))
			      (if (and number (send x :make-current number))
				  (if (send create-graph-item :value)
				      (send x :return-child-coco-graph-window
					    :copy-vertices
					    (send copy-vertices-item :value)
					    :title
					    (send x :return-model number)
					    :sibling T)))
			      (send x :make-current current-number)))))

	 (read-n-interactions-item
	  (send modal-button-proto :new
		(modal-text "<order>-order interactions on <argument>")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (number (send number-item :value))
				  (set (send argument-item :text)))
			      (if number
				  (progn
				    (send x :read-n-interactions number set)
				    (if (send create-graph-item :value)
					(send x :return-child-coco-graph-window
					      :copy-vertices
					      (send copy-vertices-item :value)
					      :title
					      (concatenate 'string
							   " Interactions: "
							   (format nil "~d" 
								   number) ";")
					      :sibling T))))
			      (send x :make-current current-number)))))

	 (read-model-item
	  (send modal-button-proto :new (modal-text "Read model <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :read-model gc)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title gc
				      :sibling T))))
		      (send x :make-current current-number)))))

	 (generate-decomposable-item
	  (send modal-button-proto :new
		(modal-text "Generate decomposable: Add Fill In")
		:action
		#'(lambda ()
		    (send x :add-fill-in
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))

	 (generate-graphical-item
	  (send modal-button-proto :new (modal-text "Generate graphical model")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    ;; (send x :make-graph-current-model)
			    (send x :generate-graphical)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title " -> Graphical;"
				      :child T))))
		      (send x :make-current current-number)))))

	 (dual-item
	  (send modal-button-proto :new (modal-text "Transform Normal to Dual")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    ;; (send x :make-graph-current-model)
			    (send x :normal-to-dual)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title " -> Dual;"
				      :sibling T))))
		      (send x :make-current current-number)))))

	 (normal-item
	  (send modal-button-proto :new (modal-text "Transform Dual to Normal")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    ;; (send x :make-graph-current-model)
			    (send x :dual-to-normal)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title " -> Normal;"
				      :sibling T))))
		      (send x :make-current current-number)))))

	 (collaps-item
	  (send modal-button-proto :new (modal-text "Collaps onto <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :make-graph-current-model)
			    (send x :collaps-model gc)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title (concatenate 'string " Collaps: "
							  gc ";")
				      :parant T))))
		      (send x :make-current current-number)))))

	 (meet-item
	  (send modal-button-proto :new (modal-text "Meet with <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (base-number 
			   (send x :return-model-number 'base))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :make-graph-base-model)
			    (send x :read-model gc)
			    (send x :meet-of-models)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title (concatenate 'string " Meet: "
							  gc ";")
				      :offset (list 50 -10)
				      :parant T))))
		      (send x :make-current current-number)
		      (send x :make-base base-number)
		      ))))

	 (join-item
	  (send modal-button-proto :new (modal-text "Join with <argument>")
		:action
		#'(lambda ()
		    (let ((current-number 
			   (send x :return-model-number 'current))
			  (base-number 
			   (send x :return-model-number 'base))
			  (gc (send argument-item :gc x)))
		      (if gc
			  (progn
			    (send x :make-graph-base-model)
			    (send x :read-model gc)
			    (send x :join-of-models)
			    (send x :current)
			    (if (send create-graph-item :value)
				(send x :return-child-coco-graph-window
				      :copy-vertices
				      (send copy-vertices-item :value)
				      :title (concatenate 'string " Join: "
							  gc ";")
				      :offset (list 50 -110)
				      :child T))))
		      (send x :make-current current-number)
		      (send x :make-base base-number)
		      ))))

	 (drop-edges-item
	  (send modal-button-proto :new (modal-text "Drop edges <argument>")
		:action
		#'(lambda ()
		    (send x :graph-drop-gc (send argument-item :gc x) nil
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))

	 (add-edge-item
	  (send modal-button-proto :new (modal-text "Add edges <argument>")
		:action
		#'(lambda ()
		    (send x :graph-add-gc (send argument-item :gc x)
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))
	
	 (drop-interactions-item
	  (send modal-button-proto :new
		(modal-text "Drop interactions <argument>")
		:action
		#'(lambda ()
		    (send x :graph-drop-gc (send argument-item :gc x) T
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))

	 (add-interactions-item
	  (send modal-button-proto :new
		(modal-text "Add interactions <argument>")
		:action
		#'(lambda ()
		    (send x :graph-add-gc (send argument-item :gc x)
			  :hierarchical T
			  :create-graph (send create-graph-item :value)
			  :copy-vertices (send copy-vertices-item :value)
			  ))))
	
	 (split-item
	  (send modal-button-proto :new
		(modal-text "Split <argument> ")
		:action
		#'(lambda ()
		    (let ((argument (string-to-block-list
				     (send argument-item :gc x))))
		    (send x :split (car (car argument))
			  :ignore-name-list (cadr (car argument)))))))

	 (split-block-item
	  (send modal-button-proto :new
		(modal-text "Split block recursive model <argument> ")
		:action
		#'(lambda ()
		    (send x :split-in-block-recursive
			  (send argument-item :gc x)))))
	 

	 (model-dialog-window
	  (send modal-dialog-proto :new
		(if (kind-of-p x manager-proto)
		    (list
		     (list dismiss-model)
		     argument-label argument-item
		     create-graph-item ;; copy-vertices-item
		     (list
		      (list read-model-item read-n-interactions-item
			    graph-number-item
			    generate-decomposable-item generate-graphical-item
			    dual-item normal-item
			    ;; split-item
			    )
		      (list  collaps-item meet-item join-item
			     drop-edges-item add-edge-item
			     drop-interactions-item add-interactions-item
			     ;; split-block-item
			     ))
		     )
		  (list
		   (list dismiss-model)
		   argument-label argument-item
		   create-graph-item copy-vertices-item 
		   (list number-label number-item)
		   (list
		    (list read-model-item
			  graph-number-item
			  generate-decomposable-item generate-graphical-item
			  dual-item normal-item
			  read-n-interactions-item
			  ;; split-item
			  )
		    (list  meet-item join-item
			   drop-edges-item add-edge-item
			   drop-interactions-item add-interactions-item
			   collaps-item
			   ;; split-block-item
			   ))
		   )))))

    (defmeth argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (defmeth number-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (defmeth argument-item :gc (graph)
      (let ((number (send self :value)))
	(if number
	    (send graph :return-model number)
	  (let ((gc (send self :text)))
	    (cond
	     ((equalp gc "'graph")
	      (send graph :return-model))
	     ((equalp gc "'current")
	      (send graph :return-model 'current))
	     ((equalp gc "'base")
	      (send graph :return-model 'base))
	     ((equalp gc "'last")
	      (send graph :return-model 'last))
	     (t gc))))))
    )
  )
