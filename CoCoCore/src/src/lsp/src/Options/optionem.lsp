
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; the EM algorithm in the CoCo-object.

(defun option-em-dialog (x)

  (let* ((dismiss-option-em
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-em-dialog-window :remove))))
	 (em-item
	  (get-linked-toggle-item x 'em                  ))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, Em")
		:action #'(lambda () (send x :status 'em))))

	 (initial-item
	  (send modal-button-proto :new
		(modal-text "Initial values: ...")
		:action #'(lambda ()
			    (send x :set-em-initial
				  (get-choice-dialog
				   "Initial value:"
				   (list 'uniform 'first 'last
					 'mean 'random 'input)
				   (send x :set-em-initial))))))

	 (epsilon-item
	  (send modal-button-proto :new
		(modal-text "Stopping criterion, Epsilon ...")
		:action
		#'(lambda ()
		    (send x :set-em-epsilon
			  (get-real-dialog
			   "Epsilon:"
			   :initial (car (send x :set-em-epsilon 'what)))))))

	 (em-iterations-item
	  (send modal-button-proto :new
		(modal-text "Maximal number of Iterations ...")
		:action
		#'(lambda ()
		    (send x :set-em-max-iterations
			  (get-real-dialog
			   "Maximal number of Iterations:"
			   :initial (car (send x :set-em-max-iterations
					       'what)))))))

	 (option-em-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-em)
		      status-item
		      em-item
		      initial-item epsilon-item
		      em-iterations-item
		      )))))
  )

