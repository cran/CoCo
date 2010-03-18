
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the EH-procedure:

;;; Base-model and fixing?
;;; fit/accept/reject more models ?
;;; Force add duals ???

(defun eh-dialog (x)
  (let* ((dismiss-eh
	  (send modal-button-proto :new (modal-text "Dismiss eh search window")
		:action #'(lambda () (send eh-dialog-window :remove))))


;;;	 (significance-label
;;;	  (send text-item-proto :new "Significance level:  "))

	 (significance-label
	  (send modal-button-proto :new
		(modal-text "Significance level: ")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    )))

	 (significance-item
	  (send edit-text-item-proto :new "0.05" :text-length 6))
	
	 (class-label (send text-item-proto :new "<Class>:         "))

	 (*class-list*
	  (list 'all 'duals 'a-dual 'r-dual 'classes 'accepted 'rejected))

	 (*class-names*
	  (mapcar #'string *class-list*))
	 
	 (class-item
	  (send choice-item-proto :new
		(mapcar #'string *class-list*) :value 0))

	 (dual-label (send text-item-proto :new "<Dual>:           "))

	 (*dual-names*
	  (list "'a-dual" "'r-dual"
		"'smallest-dual" "'largest-dual" "'both-duals"))

	 (*dual-list*
	  (list 'a-dual 'r-dual 'smallest-dual 'largest-dual 'both-duals))

	 (dual-item
	  (send choice-item-proto :new
		(mapcar #'string *dual-list*) :value 0))

	 (sub-class-label (send text-item-proto :new "<Sub-Class>: "))

	 (*sub-class-names*
	  (list "'decomposable" "'graphical" "'hierarchical"))

	 (*sub-class-list*
	  (list 'decomposable 'graphical 'hierarchical))

	 (sub-class-item
	  (send choice-item-proto :new
		(mapcar #'string *sub-class-list*) :value 1))

	 (strategy-label (send text-item-proto :new "<Strategy>:   "))

	 (*strategy-names*
	  (list "'smallest" "'alternating" "'rough"))

	 (*strategy-list*
	  (list 'smallest 'alternating 'rough))

	 (strategy-item
	  (send choice-item-proto :new
		(mapcar #'string *strategy-list*) :value 0))
	
	 (model-label (send text-item-proto :new "<Model>:         "))

	 (*model-names*
	  (list "'current" "'base" "'last" "'graph" "'all" "'number:"))

	 (*model-list*
	  (list 'current 'base 'last 'graph 'all 'number))

	 (model-item (send choice-item-proto :new *model-names* :value 3))

	 (argument-item
	  (send edit-text-item-proto :new
		(format nil "~d" (send x :slot-value 'model-number))
		:text-length 3))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status")
		:action #'(lambda () (send x :status 'eh))))

	 (find-item
	  (send modal-button-proto :new
		(modal-text "Find dual <dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :find-dual
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value) *sub-class-list*))
		    (send x :status 'eh))))

	 (dispose-item
	  (send modal-button-proto :new
		(modal-text "Dispose of eh <class>")
		:action
		#'(lambda ()
		    (send x :dispose-of-eh
			  (nth (send class-item :value) *class-list*))
		    (send x :status 'eh))))

	 (fit-item
	  (send modal-button-proto :new
		(modal-text "Fit <dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    (send x :fit
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value) *sub-class-list*))
		    (send x :status 'eh))))

	 (accept-item
	  (send modal-button-proto :new
		(modal-text "% Accept <a-/r-dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :accept
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value)*sub-class-list*))
		    (send x :status 'eh))))

	 (reject-item
	  (send modal-button-proto :new
		(modal-text "% Reject <a-/r-dual> <sub-class>")
		:action
		#'(lambda ()
		    (send x :reject
			  (nth (send dual-item :value) *dual-list*)
			  (nth (send sub-class-item :value) *sub-class-list*))
		    (send x :status 'eh))))

	 (fit-model-item
	  (send modal-button-proto :new
		(modal-text "Fit <Model>")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    (send x :fit (send model-item :model
				       (send x :slot-value 'model-number)))
		    (send x :status 'eh))))

	 (accept-model-item
	  (send modal-button-proto :new
		(modal-text "Accept <Model>")
		:action #'(lambda ()
			    (send x :accept
				  (send model-item :model
					(send x :slot-value 'model-number)))
			    (send x :status 'eh))))

	 (reject-model-item
	  (send modal-button-proto :new
		(modal-text "Reject <Model>")
		:action #'(lambda ()
			    (send x :reject
				  (send model-item :model
					(send x :slot-value 'model-number)))
			    (send x :status 'eh))))

	 (search-item
	  (send modal-button-proto :new
		(modal-text "Recursive search <strategy> <sub-class>")
		:action
		#'(lambda ()
		    (send x :set-acceptance (send significance-item :value))
		    (send x :eh
			  :strategy  (nth (send strategy-item :value)
					  *strategy-list*)
			  :sub-class (nth (send sub-class-item :value)
					  *sub-class-list*))
;;;		    (send x :status 'eh)
		    )))

	 (plot-search-result-item
	  (send modal-button-proto :new (modal-text "Plot EH result <class>")
		:action
		#'(lambda ()
		    (let ((current-number
			   (send x :return-model-number 'current)))
		      (send x :plot-EH-search-result
			    (nth (send class-item :value) *class-list*))
		      (send x :make-current current-number)))))

	 (eh-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-eh)
		 (list significance-label significance-item)
		 (list (list sub-class-label sub-class-item)
		       (list strategy-label strategy-item)
		       (list dual-label dual-item)
		       (list class-label class-item)
		       (list model-label model-item argument-item)
		       )
		 (list find-item dispose-item)
		 (list
		  (list fit-item accept-item reject-item)
		  (list fit-model-item accept-model-item reject-model-item))
		 (list search-item plot-search-result-item)
		 status-item
		 ))))

    (defmeth significance-item :value ()
      (eval (with-input-from-string (s (send self :text)) (read s))))

    (defmeth argument-item :value ()
      (let ((digits (- (string-int (send self :text)) 48)))
	(if (and (<= 0 (min digits)) (<= (max digits) 9) )
	    (sum (* (reverse (^ 10 (iseq (length digits)))) digits))))
      )

    (defmeth model-item :model (&optional (model-number -1))
      (let ((model (nth (send model-item :value) *model-list*)))
	(if (equal 'number model)
	    (send argument-item :value)
	  (if (equal 'graph model) model-number model)))
      )
    )
  )

;;

(provide "cocodialogs")
