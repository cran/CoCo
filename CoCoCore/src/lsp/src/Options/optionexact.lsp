
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; exact tests in the CoCo-object.

(defun option-exact-dialog (x)

  (let* ((dismiss-option-exact
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-exact-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, Exact tests ")
		:action #'(lambda () (send x :status 'exact))))

	 (exact-test-item
	  (get-linked-toggle-item x 'exact-test          ))

	 (epsilon-item
	  (send modal-button-proto :new
		(modal-text "Epsilon ...")
		:action
		#'(lambda ()
		    (send x :set-exact-epsilon
			  (get-real-dialog
			   "Epsilon limit:"
			   :initial (car (send x :set-exact-epsilon 'what)))))))

	 (exact-test-total-item
	  (get-linked-toggle-item x 'exact-test-total    ))

	 (exact-test-parts-item
	  (get-linked-toggle-item x 'exact-test-parts    ))

	 (exact-test-unparted-item
	  (get-linked-toggle-item x 'exact-test-unparted ))

	 (exact-only-log-l-item
	  (get-linked-toggle-item x 'exact-only-log-l    ))

	 (number-item
	  (send modal-button-proto :new
		(modal-text "Number of tables to generate ...")
		:action
		#'(lambda ()
		    (send x :set-number-of-tables
			  (get-real-dialog
			   "Number of tables to generate:"
			   :initial (car (send x :set-number-of-tables
					       'what)))))))

	 (numbers-item
	  (send modal-button-proto :new
		(modal-text "List of Number of tables to generate .?.")
		:action
		#'(lambda ()
		    (send x :set-list-of-number-of-tables
			  (car
			   (get-value-dialog
			    "List of Number of tables to generate:"
			    :initial
			    `',(send x :set-list-of-number-of-tables'what)
			    :text-length 40
			    ))))))

	 (asymptotic-item
	  (send modal-button-proto :new
		(modal-text "Asymptotic limit ...")
		:action
		#'(lambda ()
		    (send x :set-asymptotic
			  (get-real-dialog
			   "Asymptotic limit:"
			   :initial (car (send x :set-asymptotic 'what)))))))

	 (fast-item
	  (get-linked-toggle-item x 'fast                ))

	 (seed-item
	  (send modal-button-proto :new
		(modal-text "Seed for random generator ...")
		:action
		#'(lambda ()
		    (send x :set-seed
			  (get-real-dialog
			   "Seed limit:"
			   :initial (car (send x :set-seed 'what)))))))

	 (option-exact-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-exact)
		      status-item
		      exact-test-item epsilon-item
		      exact-test-total-item
		      exact-test-parts-item exact-test-unparted-item
		      exact-only-log-l-item
		      number-item numbers-item
		      asymptotic-item fast-item seed-item
		      )))))
  )

