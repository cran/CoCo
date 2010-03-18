
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting options for
;;; datastructure in the CoCo-object.

(defun option-limits-dialog (x)

  (let* ((dismiss-option-limits
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-limits-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, limit options ")
		:action #'(lambda () (send x :status 'limits))))

	 (large-item
	  (get-linked-toggle-item x 'large               ))
	 (huge-item
	  (get-linked-toggle-item x 'huge                ))
	 (sorted-item
	  (get-linked-toggle-item x 'sorted              ))

	 (option-limits-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-limits)
		      status-item
		      large-item
		      huge-item
		      sorted-item
		      )))))
  )

