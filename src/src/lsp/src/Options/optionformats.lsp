
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for formats in CoCo-object.

(defun option-formats-dialog (x)

  (let* ((dismiss-option-formats
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-formats-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, formats options ")
		:action #'(lambda () (send x :status 'formats))))

	 (page-formats-item
	  (send modal-button-proto :new
		(modal-text "Page-formats  ...")
		:action #'(lambda ()
			    (send x :set-page-formats
				  (car (get-value-dialog
					"Page-formats:"
					:initial
					`',(send x :set-page-formats 'what)
					))))))

	 (table-formats-item
	  (send modal-button-proto :new
		(modal-text "Table-formats  ...")
		:action #'(lambda ()
			    (send x :set-table-formats
				  (car (get-value-dialog
					"Table-formats:"
					:initial
					`',(send x :set-table-formats 'what)
					))))))

	 (test-formats-item
	  (send modal-button-proto :new
		(modal-text "Test-formats  ...")
		:action #'(lambda ()
			    (send x :set-test-formats
				  (car (get-value-dialog
					"Test-formats:"
					:initial
					`',(send x :set-test-formats 'what)
					))))))

	 (print-formats-item
	  (send modal-button-proto :new
		(modal-text "Print-formats  ...")
		:action #'(lambda ()
			    (send x :set-print-formats
				  (car (get-value-dialog
					"Print-formats:"
					:initial
					`',(send x :set-print-formats 'what)
					))))))

	 (paging-length-item
	  (send modal-button-proto :new
		(modal-text "paging-length  ...")
		:action #'(lambda ()
			    (send x :set-paging-length
				  (car (get-value-dialog
					"Paging-length:"
					:initial
					`',(send x :set-paging-length 'what)
					))))))

	 (short-test-output-item
	  (get-linked-toggle-item x 'short-test-output   ))

	 (pausing-of-output-item
	  (get-linked-toggle-item x 'pausing-of-output   ))

	 (option-formats-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-formats)
		      status-item
		      page-formats-item
		      table-formats-item
		      test-formats-item
		      print-formats-item
		      short-test-output-item
		      pausing-of-output-item
		      paging-length-item
		      )))))
  )

