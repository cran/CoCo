
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for the tests:


;;; :print-common-decompositions, :decompose-models?


(defun test-dialog (x)
  (let* ((dismiss-test
	  (send modal-button-proto :new (modal-text "Dismiss test window")
		:action #'(lambda () (send test-dialog-window :remove))))

	 (test-item
	  (send modal-button-proto :new 
		(modal-text "`t': Test graph against `Base'")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (send x :test))
			      (send x :make-current current-number)))))

	 (argument-item
	  (send edit-text-item-proto :new 
		(to-string (send x :return-name-list) "")
	        ;;; :text-length 66
		))
	
	 (edge-item
	  (send toggle-item-proto :new "Edges (else interactions)" :value T))

	 (factorize-item
	  (send modal-button-proto :new 
		"Factorize test: "
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (send x :factorize
					(if (send edge-item :value)
					    'edges 'interactions) set))
			      (send x :make-current current-number)))))

	 (find-likelihood-item
	  (send modal-button-proto :new 
		(modal-text "Find log(Likelihood)")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :find-log-l))
				)
			      (send x :make-current current-number)))))

	 (find-deviance-item
	  (send modal-button-proto :new 
		(modal-text "Find deviance")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :find-deviance))
				)
			      (send x :make-current current-number)))))

	 (bind-deviance-item
	  (send modal-button-proto :new 
		(modal-text "Bind values of find deviance to *test*")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :compute-deviance))
				)
			      (send x :make-current current-number)))))

	 (bind-test-item
	  (send modal-button-proto :new 
		(modal-text "Bind values of compute test to *test*")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :compute-test))
				)
			      (send x :make-current current-number)))))

	 (bind-test-object-item
	  (send modal-button-proto :new 
		(modal-text "Bind values of test to *test* object")
		:action #'(lambda ()
			    (let ((current-number
				   (send x :return-model-number 'current))
				  (set (send argument-item :text)))
			      (if (send x :make-graph-current-model
					:redraw-plots nil)
				  (setf *test* (send x :return-test-object))
				)
			      (send x :make-current current-number)))))

	 (show-tests-item
	  (send modal-button-proto :new 
		(modal-text "Show computed tests")
		:action #'(lambda () (send x :show-tests))))

	 (dispose-of-tests-item
	  (send modal-button-proto :new 
		(modal-text "Dispose of computed tests")
		:action #'(lambda () (send x :dispose-of-tests))))

	 (argument-slice-item
	  (send edit-text-item-proto :new 
		(to-string (send x :return-name-list) "")
	        ;;; :text-length 66
		))

	 (slice-item
	  (send modal-button-proto :new 
		"Meassures of associations: "
		:action #'(lambda ()
			    (send x :slice (send argument-slice-item :text)))))

	 (test-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-test)
		 test-item
		 find-likelihood-item find-deviance-item
		 bind-test-object-item bind-test-item bind-deviance-item
		 ;;; argument-label
		 (list factorize-item (list edge-item argument-item))
		 show-tests-item dispose-of-tests-item
		 (list slice-item argument-slice-item)
		 )))))
  )
