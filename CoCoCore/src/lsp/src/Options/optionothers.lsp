
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for selecting ``other'' options
;;; in the CoCo-object.

(defun option-other-dialog (x)

  (let* ((dismiss-option-other
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda ()
			    (send option-other-dialog-window :remove))))

	 (status-item
	  (send modal-button-proto :new
		(modal-text "Status of CoCo-object, other options ")
		:action #'(lambda () (send x :status 'other))))

	 (graph-mode-item
	  (get-linked-toggle-item x 'graph-mode          ))
	 (report-item
	  (get-linked-toggle-item x 'report              ))
	 (trace-item
	  (get-linked-toggle-item x 'trace               ))
	 (debug-item
	  (get-linked-toggle-item x 'debug               ))
	 (timer-item
	  (get-linked-toggle-item x 'timer               ))
	 (echo-item
	  (get-linked-toggle-item x 'echo                ))
	 (note-item
	  (get-linked-toggle-item x 'note                ))
	 (option-item
	  (get-linked-toggle-item x 'option              ))
	 (warnings-item
	  (get-linked-toggle-item x 'warnings            ))

	 (option-other-dialog-window
	  (send modal-dialog-proto :new
		(list (list dismiss-option-other)
		      status-item
		      graph-mode-item
		      report-item
		      trace-item
		      debug-item
		      timer-item
		      echo-item
		      note-item
		      option-item
		      warnings-item
		      )))))
  )


;;

(provide "cocooptions")
