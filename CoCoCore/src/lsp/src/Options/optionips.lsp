
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; the IPS algorithm in the CoCo-object.

(defun option-ips-dialog (x)

  (let* ((dismiss-option-ips
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-ips-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, Ips")
		:action #'(lambda () (send x :status 'ips))))

	 (criterion-item
	  (send modal-button-proto :new
		(modal-text "Criterion, Cell/Sum: ...")
		:action #'(lambda ()
			    (send x :set-ips-stop-criterion
				  (get-choice-dialog
				   "Stop criterion:"
				   (list 'sum 'cell)
				   (send x :set-ips-stop-criterion))))))

	 (epsilon-item
	  (send modal-button-proto :new
		(modal-text "Stopping criterion, Epsilon ...")
		:action
		#'(lambda ()
		    (send x :set-ips-epsilon
			  (get-real-dialog
			   "Epsilon:"
			   :initial (car (send x :set-ips-epsilon 'what)))))))

	 (ips-iterations-item
	  (send modal-button-proto :new
		(modal-text "Maximal number of Iterations ...")
		:action
		#'(lambda ()
		    (send x :set-ips-max-iterations
			  (get-real-dialog
			   "Maximal number of Iterations:"
			   :initial (car (send x :set-ips-max-iterations
					       'what)))))))

	 (option-ips-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-ips)
		      status-item
		      criterion-item epsilon-item
		      ips-iterations-item
		      )))))
  )

